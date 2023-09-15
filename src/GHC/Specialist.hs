{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module GHC.Specialist where

import GHC.Specialist.Wrapper

import GHC.Core
import GHC.Core.Predicate
import GHC.Core.TyCo.Rep (Scaled (..))
import GHC.Plugins
import GHC.Types.Tickish
import GHC.Types.TyThing

import Data.List (find)
import Control.Monad.Reader
import Control.Monad.State.Strict

-------------------------------------------------------------------------------
-- Plugin definition and installation
-------------------------------------------------------------------------------

-- | The Specialist plugin
plugin :: Plugin
plugin =
    defaultPlugin
      { installCoreToDos = \opts todos ->
          let
            specialistEnv = parseSpecialistOpts opts
          in
            runSpecialist specialistEnv emptySpecialistState (install todos)
      }

-- | Install Specialist at the end of the core-to-core passes.
install :: [CoreToDo] -> SpecialistM [CoreToDo]
install todos = do
    slog "Inserting Specialist plugin at end of compilation pipeline"
    todo <- specialist

    -- Insert Specialist at the end of the compilation todos
    return $ foldr (:) [todo] todos

-- | Specialist's core-to-core pass. Prints a message stating which module we
-- are operating on and then traverses the core bindings with 'processBind'.
specialist :: SpecialistM CoreToDo
specialist = do
    specialistEnv <- ask
    return $ CoreDoPluginPass "specialist" $
      \mgs@ModGuts{..} -> do
        let
          currentModuleString = moduleNameString (moduleName mg_module)

        slogCore specialistEnv $
          "Starting Specialist plugin on module: " ++ currentModuleString

        -- Process all top-level binders in the core program
        mg_binds' <- runSpecialist specialistEnv emptySpecialistState $ mapM processBind mg_binds

        return $ mgs { mg_binds = mg_binds' }

parseSpecialistOpts :: [CommandLineOption] -> SpecialistEnv
parseSpecialistOpts opts =
    SpecialistEnv
      { specialistEnvVerbosity = if "v" `elem` opts then Verbose else Silent
      }

-------------------------------------------------------------------------------
-- Monad definition
-------------------------------------------------------------------------------

data SpecialistEnv =
    SpecialistEnv
      { specialistEnvVerbosity :: !Verbosity
      }

data SpecialistState =
    SpecialistState
      { specialistStateLastSourceNote :: !(Maybe (RealSrcSpan, LexicalFastString))
      }

emptySpecialistState :: SpecialistState
emptySpecialistState =
    SpecialistState
      { specialistStateLastSourceNote = Nothing
      }

data Verbosity = Silent | Verbose

newtype SpecialistT m a = SpecialistT { runSpecialistT :: ReaderT SpecialistEnv (StateT SpecialistState m) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader SpecialistEnv
    , MonadState SpecialistState
    )

deriving instance (MonadIO m) => MonadIO (SpecialistT m)

instance MonadTrans SpecialistT where
  lift act = SpecialistT (lift $ lift act)

type SpecialistM = SpecialistT CoreM

runSpecialist :: Monad m => SpecialistEnv -> SpecialistState -> SpecialistT m a -> m a
runSpecialist env s (SpecialistT run) = evalStateT (runReaderT run env) s


-------------------------------------------------------------------------------
-- Plugin AST traversals
-------------------------------------------------------------------------------

-- | Top-level traversal function. Simply calls 'processExpr' on all expressions
-- held in the bindings.
processBind :: CoreBind -> SpecialistM CoreBind
processBind = \case
    NonRec b e ->
      NonRec b <$> processExpr e
    Rec es ->
      Rec <$> mapM (\(b,e) -> (b,) <$> processExpr e) es

-- | This is where most of the work happens. If the expression is an
-- application, we determine whether the function being applied is specialised
-- or not. If it is overloaded, we modify the application by wrapping it in a
-- case statement that does some logging\/reporting when evaluated.
processExpr :: CoreExpr -> SpecialistM CoreExpr
processExpr = \case
    -- The only case we really care about: Function application
    app@App{} -> do
      -- Here we have some application like `f v1 ... vN`, where v1 ... vN
      -- should be the function's type arguments followed by the value
      -- arguments. To determine if the `f` is an overloaded function (i.e. not
      -- specialised), we check if any of the arguments v1 ... vN are
      -- dictionaries.
      let
        (f, xs) = collectArgs app
        resultTy = applyTypeToArgs empty (exprType f) xs

      -- We recursively process the arguments first so we don't have to traverse
      -- the modified expression if it is overloaded.
      oldS <- get
      args <- mapM processExpr xs
      put oldS

      if
           -- Check if any of the arguments are dictionaries
           any isDictExpr args

           -- Attempt to avoid instrumenting dictionary functions, which may be
           -- overloaded if there are superclasses, by checking if the result
           -- type of the function is a dictionary type.
        && not (isDictTy resultTy)

           -- Avoid instrumenting constraint selectors like eq_sel (TODO: Write
           -- more about how these constraints occur. See tests/T3.hs)
        && (typeTypeOrConstraint resultTy /= ConstraintLike)
      then do
        slog "\n\nAn application expression appears to be overloaded"
        slog "Application:"
        slogS app

        -- We now know:
        --   1. At least one of the arguments is a dictionary
        --   2. The function is not a dictionary constructor/selector
        --   3. The function is not a constraint constructor/selector
        --
        -- So we know our function looks something like this:
        --
        --   f t1 ... tN a1 ... aM
        --
        -- Where t1 ... tN are type arguments, a1 ... aM are value arguments,
        -- and one of a1 ... aM is a dictionary. NOTE: I think it should be
        -- impossible for a dictionary argument to occur anywhere but as the
        -- first argument to the function, since they are passed as constraints,
        -- but we handle the general case here. Say the first dictionary
        -- argument to f is aX, the crux then is building a well-typed
        -- application that includes our wrapper function:

        --   case specialistWrapper tX m1 m2 m3 aX of
        --     () -> f t1 ... tN a1 ... aM

        -- Where tX is the type argument to the wrapper function and m1, m2, m3
        -- is the metadata for this specific application. tX must be the type of
        -- the dictionary for this application.

        let
          -- Split the arguments into the type arguments and value arguments.
          -- tyArgs is t1 ... tN above and valArgs is a1 ... aM above.
          (tyArgs, valArgs) = break isValArg args

        slog "Application type args:"
        slogS tyArgs
        slog "Application value args:"
        slogS valArgs
        slog "Application result type:"
        slogS resultTy

        let
          -- Extract the dictionary argument
          dictArg =
            case find isDictExpr valArgs of
              Just expr -> expr
              Nothing -> error "Specialist could not locate a dictionary argument in an overloaded application (impossible?)"

          -- Create the type argument for the wrapper function (the type of the
          -- first dictionary value argument to the overloaded function)
          tX = exprType dictArg

        -- Get the wrapper function
        wrapperId <- lift $ do
          mName <- thNameToGhcName 'specialistWrapper
          case mName of
            Just n -> lookupId n
            Nothing -> error "Specialist plugin failed to obtain the wrapper function"

        -- Info arguments for the wrapper
        (ss, l) <-
          gets specialistStateLastSourceNote >>=
            \case
              Just (ss, LexicalFastString l) -> do
                dflags <- lift getDynFlags
                return $ (showSDoc dflags (ppr ss), unpackFS l)
              Nothing -> return ("", "")
        let
          fIdStr = mkStringLit l
          ssStr = mkStringLit ss
          metaStr = mkStringLit "test metadata"

        let
          wrapperApp =
            mkCoreApps
              (Var wrapperId)
              [ Type tX
              , fIdStr
              , ssStr
              , metaStr
              , dictArg
              ]

          wrappedApp =
            mkWildCase
              wrapperApp
              (Scaled OneTy (exprType wrapperApp))
              resultTy
              [ Alt (DataAlt unitDataCon) [] (mkCoreApps f args)
              ]

        slog "Wrapped application:"
        slogS wrappedApp

        return wrappedApp
      else
        return $ mkCoreApps f args

    -- For any recursive constructors of Expr, we traverse the nested Exprs
    Lam b e ->
      mkCoreLams [b] <$> processExpr e
    Let b e ->
      mkCoreLet <$> processBind b <*> processExpr e
    Case e b t alts ->
      -- TODO: Use some sort of mkCase?
          Case
      <$> processExpr e
      <*> pure b
      <*> pure t
      <*> mapM processAlt alts
    Cast e co ->
      mkCast <$> processExpr e <*> pure co
    Tick t e -> do
      -- If the tick is a SourceNote, track it as a potential source position
      -- for upcoming applications
      trackSourceNote t
      mkTick t <$> processExpr e

    -- For non-recursive constructors of Expr, we do nothing
    x -> return x

processAlt :: CoreAlt -> SpecialistM CoreAlt
processAlt (Alt c bs e) = Alt c bs <$> processExpr e

trackSourceNote :: CoreTickish -> SpecialistM ()
trackSourceNote = \case
    SourceNote !ss !l -> put (SpecialistState (Just (ss,l)))
    _ -> return ()

-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

isDictExpr :: CoreExpr -> Bool
isDictExpr =
    maybe False isDictTy . exprType'
  where
    exprType' :: CoreExpr -> Maybe Type
    exprType' = \case
        Type{} -> Nothing
        expr -> Just $ exprType expr

slog :: String -> SpecialistM ()
slog m =
    asks specialistEnvVerbosity >>= \case
      Silent -> return ()
      Verbose -> lift $ putMsgS m

slogS :: Outputable a => a -> SpecialistM ()
slogS x =
    asks specialistEnvVerbosity >>= \case
      Silent -> return ()
      Verbose -> lift . putMsg $ ppr x

slogCore :: SpecialistEnv -> String -> CoreM ()
slogCore SpecialistEnv{..} x =
    case specialistEnvVerbosity of
      Silent -> return ()
      Verbose -> putMsgS x
