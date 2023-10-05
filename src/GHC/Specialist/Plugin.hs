{-# LANGUAGE TemplateHaskellQuotes #-}

module GHC.Specialist.Plugin where

import GHC.Specialist.Plugin.Compat
import GHC.Specialist.Plugin.Initialization
import GHC.Specialist.Plugin.Instrumentation
import GHC.Specialist.Plugin.Logging
import GHC.Specialist.Plugin.Orphans ()
import GHC.Specialist.Plugin.Types

import Control.Monad.State
import Control.Monad.Reader
import GHC.Core
import GHC.Core.Predicate
import GHC.Core.TyCo.Rep (Scaled (..))
import GHC.Plugins
import GHC.Types.Tickish
import GHC.Types.TyThing

-------------------------------------------------------------------------------
-- Plugin definition and installation
-------------------------------------------------------------------------------

-- | The Specialist plugin
plugin :: Plugin
plugin =
    defaultPlugin
      { installCoreToDos = \opts todos -> do
          specialistEnv <- mkSpecialistEnv opts
          specialistState <- initSpecialistState specialistEnv
          runSpecialist specialistEnv specialistState (install todos)
      , pluginRecompile = purePlugin
      }

-- | Install Specialist at the end of the core-to-core passes.
install :: [CoreToDo] -> SpecialistM [CoreToDo]
install todos = do
    slogV "Inserting Specialist plugin at end of compilation pipeline"
    todo <- specialist

    -- Insert Specialist at the end of the compilation todos
    return $ foldr (:) [todo] todos

-- | Specialist's core-to-core pass. Prints a message stating which module we
-- are operating on and then traverses the core bindings with 'processBind'.
specialist :: SpecialistM CoreToDo
specialist = do
    specialistEnv <- ask
    specialistState <- get
    return $ CoreDoPluginPass "specialist" $
      \mgs@ModGuts{..} -> do
        let
          currentModuleString = moduleNameString (moduleName mg_module)

        slogCoreV specialistEnv $
          "Starting Specialist plugin on module: " ++ currentModuleString

        -- Process all top-level binders in the core program
        mg_binds' <-
          runSpecialist specialistEnv specialistState $
            mapM processBind mg_binds

        return $ mgs { mg_binds = mg_binds' }

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

           -- Avoid instrumenting join points
        && not (isJoinVarExpr f)
      then do
        slogVV "\n\nAn application expression appears to be overloaded"
        slogVV "Application:"
        slogVV app

        -- We now know:
        --   1. At least one of the arguments is a dictionary
        --   2. The function is not a dictionary constructor/selector
        --   3. The function is not a constraint constructor/selector
        --   4. The function is not a join point
        --
        -- So we know our function looks something like this:
        --
        --   f t1 ... tN a1 ... aM
        --
        -- Where t1 ... tN are type arguments, a1 ... aM are value arguments,
        -- and one or more of a1 ... aM are dictionaries. We want to transform
        -- this application into an expression like the following:
        --
        --   case specialistWrapper ... of () -> f t1 ... tN a1 ... aM
        --
        -- The difficult bit is building a well-typed scrutinee for the case
        -- expression, since the type arguments and value arguments to
        -- specialistWrapper need to be specially constructed for each
        -- instrumented application.
        --
        -- The wrapper function accepts the type of the first dictionary
        -- argument and the type of the overloaded function partially applied to
        -- all arguments up to and including the first dictionary. We build
        -- these now.

        let
          -- Split the arguments into the type arguments and value arguments.
          -- tyArgs is t1 ... tN above and valArgs is a1 ... aM above.
          (tyArgs, valArgs) = break isValArg args

          -- Split the value arguments into those before the first dictionary
          -- argument and those after
          (preDictArgs, dictArg, postDictArgs) =
            case break isDictExpr valArgs of
              (_, []) -> error "Specialist found an overloaded application with no dictionary arguments (impossible?)"
              (pre, dict:post) -> (pre, dict, post)

          -- All dictionaries used in the overloaded call
          dicts = dictArg : filter isDictExpr postDictArgs

          -- Apply f to its type arguments and any non-dictionary value
          -- arguments (f t1 ... tN a1 ... a(X-1)). This is the overloaded
          -- function that will be passed to the wrapper for analysis.
          f' = mkCoreApps f (tyArgs ++ preDictArgs)

          -- Create the type arguments for the wrapper function that
          -- representing the type of the overloaded function partially applied
          -- to the pre-dictionary args)
          (ta, tb) = (exprType dictArg, exprType (mkCoreApp empty f' dictArg))

        slogVV "Application type args:"
        slogVV tyArgs
        slogVV "Application value args:"
        slogVV valArgs
        slogVV "Application result type:"
        slogVV resultTy

        -- Get the wrapper function
        wrapperId <- lift $ do
          mName <- thNameToGhcName 'specialistWrapper
          case mName of
            Just n -> lookupId n
            Nothing -> error "Specialist plugin failed to obtain the wrapper function"

        -- Get the Box type
        boxType <- lift $ do
          mName <- thNameToGhcName 'boxTypeDUMMY
          case mName of
            Just n -> exprType . Var <$> lookupId n
            Nothing -> error "Specialist plugin failed to obtain the box type"

        -- Get the mkBox function
        mkBoxId <- lift $ do
          mName <- thNameToGhcName 'mkBox
          case mName of
            Just n -> lookupId n
            Nothing -> error "Specialist plugin failed to obtain the Box type"

        -- Info arguments for the wrapper
        (ss, l) <-
          gets specialistStateLastSourceNote >>=
            \case
              Just (ss, l) -> do
                dflags <- lift getDynFlags
                return (showSDoc dflags (ppr ss), l)
              Nothing -> return ("", "")

        uniqId <- show <$> getUniqueM
        let
          fIdStr = mkStringLit uniqId
          lStr = mkStringLit l
          ssStr = mkStringLit ss

        let
          wrapperApp =
            mkCoreApps
              (Var wrapperId)
              [ Type ta
              , Type $ getRuntimeRep tb
              , Type tb
              , fIdStr
              , lStr
              , ssStr
              , f'
              , mkListExpr boxType $
                  map
                    ( \d ->
                        mkCoreApps (Var mkBoxId) [Type $ exprType d, d]
                    )
                    dicts
              ]

          wrappedApp =
            mkWildCase
              wrapperApp
              (Scaled OneTy (exprType wrapperApp))
              resultTy
              [ Alt (DataAlt unitDataCon) [] (mkCoreApps f args)
              ]

        slogVV "Wrapped application:"
        slogVV wrappedApp

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

-- | Process a case alternative
processAlt :: CoreAlt -> SpecialistM CoreAlt
processAlt (Alt c bs e) = Alt c bs <$> processExpr e

-- | Stash a 'CoreTickish' away if it is a 'SourceNote' so we can emit the label
-- and source span from the instrumentation for the next overloaded application
-- we see.
trackSourceNote :: CoreTickish -> SpecialistM ()
trackSourceNote = \case
    SourceNote !ss !l ->
      modify $
        \s -> s { specialistStateLastSourceNote = Just (ss, sourceNoteLabelString l) }
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

isJoinVarExpr :: CoreExpr -> Bool
isJoinVarExpr =
    \case
      Var var -> isJoinId var
      _ -> False
