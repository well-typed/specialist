{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module GHC.Specialist where

import GHC.Specialist.Wrapper

import GHC.Core
import GHC.Core.Predicate
import GHC.Plugins
import GHC.Types.TyThing

import Control.Monad.Reader

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
            runSpecialist specialistEnv (install todos)
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
        mg_binds' <- runSpecialist specialistEnv $ mapM processBind mg_binds

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
      { specialistEnvVerbosity :: Verbosity
      }

data Verbosity = Silent | Verbose

newtype SpecialistT m a = SpecialistT { runSpecialistT :: ReaderT SpecialistEnv m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader SpecialistEnv
    , MonadTrans
    )
deriving instance (MonadIO m) => MonadIO (SpecialistT m)

type SpecialistM = SpecialistT CoreM

runSpecialist :: SpecialistEnv -> SpecialistT m a -> m a
runSpecialist env (SpecialistT run) = runReaderT run env


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
-- or not. If it is not specialised, we modify the application by wrapping the
-- applied function with a reporting\/logging function.
processExpr :: CoreExpr -> SpecialistM CoreExpr
processExpr = \case
    -- The only case we really care about: Function application
    app@App{} -> do
      -- Here we have some application like `f v1 ... vN`, where v1 ... vN
      -- should be the function's type arguments followed by the value
      -- arguments. To determine if the `f` is an overloaded function (i.e. not
      -- specialised), we check if any of the arguments v1 ... vN are
      -- dictionaries.
      --
      -- We also attempt to avoid instrumenting a dictionary function, which may
      -- be overloaded if there are superclasses, by checking if the result type
      -- of the function is a dictionary type.

      -- We recursively process the arguments first so we don't have to traverse
      -- the modified expression if it is overloaded.
      let (f, xs) = collectArgs app
      args <- mapM processExpr xs

      if
           -- Any of the arguments are dictionaries
           any isDictExpr args

           -- This function is not itself a dictionary constructor
           -- (avoid instrumenting e.g. $fShowList)
        && not (isDictTy (applyTypeToArgs empty (exprType f) args))
      then do
        slog "\n\nAn application expression appears to be overloaded"
        slog "Application:"
        slogS app

        -- At least one of the arguments is a dictionary, and the function is
        -- not a dictionary constructor/selector. So now we know our function
        -- looks something like this:
        --
        --   f t1 ... tN a1 ... aM
        --
        -- Where t1 ... tN are type arguments, a1 ... aM are value arguments,
        -- and one of a1 ... aM is a dictionary. NOTE: It should be impossible
        -- for a dictionary argument to occur anywhere but as the first argument
        -- to the function, since they are passed as constraints, but we handle
        -- the general case here. Say the first dictionary argument to f is aX,
        -- then we want to focus on:
        --
        -- f t1 ... tN a1 ... a(X-1)
        --
        -- As our "overloaded function".
        --
        -- The crux then is building a well-typed application that includes our
        -- wrapper function:
        --
        --   specialist_wrapper tX tY m (f t1 ... tN a1 ... a(X-1)) aX ... aM
        --
        -- Where tX and tY are the type arguments to the wrapper function and m
        -- is the metadata for this specific application. We need to build tX
        -- and tY based on the type of the overloaded function.
        --
        -- Since specialist_wrapper :: String -> (a -> b) -> a -> b, a ~ tX
        -- should be the type of the first value argument (the dictionary!) to
        -- the overloaded function, and b ~ tY should be the type of the
        -- function after having been partially applied to that value argument.

        let
          -- Split the arguments into the type arguments and value arguments.
          -- tyArgs is t1 ... tN above and valArgs is a1 ... aM above.
          (tyArgs, valArgs) = break isValArg args

        slog "Application type args:"
        slogS tyArgs
        slog "Application value args:"
        slogS valArgs

        let
          -- Take the longest prefix of value arguments that are not
          -- dictionaries. preDictVals is a1 ... a(X-1) above and dictVals is aX
          -- ... aM above.
          --
          -- Invariant: isDictExpr (head dictVals) == True.
          (preDictVals, dictVals) = break isDictExpr valArgs

          -- Apply f to its type arguments and any non-dictionary value
          -- arguments (f t1 ... tN a1 ... a(X-1))
          f' = mkCoreApps f (tyArgs ++ preDictVals)

          -- Create the type arguments for the wrapper function (the type of the
          -- first dictionary value argument to the overloaded function, and the
          -- type of the overloaded function partially applied to the first
          -- value argument)
          (tX, tY) =
            case dictVals of
              (a:_) -> (exprType a, exprType (mkCoreApp empty f' a))
              _ -> error "Specialist plugin found an overloaded function application with no value arguments (impossible?)"

        -- Get the wrapper function
        wrapperId <- lift $ do
          mName <- thNameToGhcName 'specialistWrapper'
          case mName of
            Just n -> lookupId n
            Nothing -> error "Specialist plugin failed to obtain the wrapper function"

        -- This could be anything deemed useful
        let metaStr = mkStringLit "test metadata"

        let
          wrappedApp =
            mkCoreApps
              (mkCoreApps (Var wrapperId) [Type tX, Type tY, metaStr, f']) dictVals

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
    Tick t e ->
      mkTick t <$> processExpr e

    -- For non-recursive constructors of Expr, we do nothing
    x -> return x

processAlt :: CoreAlt -> SpecialistM CoreAlt
processAlt (Alt c bs e) = Alt c bs <$> processExpr e

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
