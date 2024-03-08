{-# LANGUAGE TemplateHaskellQuotes #-}

module GHC.Specialist.Plugin where

import GHC.Specialist.Plugin.Compat
import GHC.Specialist.Plugin.Initialization
import GHC.Specialist.Plugin.Instrumentation
import GHC.Specialist.Plugin.Logging
import GHC.Specialist.Plugin.Orphans ()
import GHC.Specialist.Plugin.Types

import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.List
import Data.Maybe
import Data.Set qualified as Set
import GHC.Core
import GHC.Core.Predicate
import GHC.Core.TyCo.Rep (Scaled (..))
import GHC.Plugins
import GHC.Types.CostCentre
import GHC.Types.CostCentre.State
import GHC.Types.Tickish
import GHC.Types.TyThing

-------------------------------------------------------------------------------
-- Plugin definition and installation
-------------------------------------------------------------------------------

-- | The Specialist plugin
plugin :: Plugin
plugin =
    defaultPlugin
      { latePlugin = \hsc_env opts (cgs, cc_state) -> do
          specialistEnv <- mkSpecialistEnv hsc_env opts
          specialistState <- initSpecialistState (cg_module cgs) cc_state specialistEnv
          evalSpecialist
            specialistEnv
            specialistState
            (specialist (cgs, cc_state))
      , pluginRecompile = purePlugin
      }

-- | Specialist's core-to-core pass. Prints a message stating which module we
-- are operating on and then traverses the core bindings with 'processBind'.
specialist
  :: (CgGuts, CostCentreState)
  -> SpecialistM (CgGuts, CostCentreState)
specialist (cgs@CgGuts{..}, _) = do
    let
      currentModuleString = moduleNameString (moduleName cg_module)

    slogV $
      "starting specialist plugin on module: " ++ currentModuleString

    -- Process all top-level binders in the core program
    cg_binds' <- mapM processBind cg_binds
    SpecialistState{..} <- get

    slogV $
      "specialist plugin found " ++ show specialistStateOverloadedCallCount ++
      " overloaded calls in " ++ "module " ++ currentModuleString

    return
      ( cgs
          { cg_binds = cg_binds'
          , cg_ccs = Set.toList specialistStateLocalCcs ++ cg_ccs
          }
      , specialistStateCostCentreState
      )

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
      -- the modified expression if it is overloaded. Make sure we restore the
      -- last source note for this location
      oldSrcNote <- gets specialistStateLastSourceNote
      args <- mapM processExpr xs
      modify' $
        \s -> s { specialistStateLastSourceNote = oldSrcNote }


      name_cache <- asks (hsc_NC . specialistEnvHscEnv)

      if
           -- Check if any of the arguments are dictionaries
           any isDictExpr args

           -- Avoid instrumenting dfuns, which may appear overloaded if they
           -- come from a single method type class with superclasses
        && maybe True (not . ("$f" `isPrefixOf`)) (exprOccNameString f)

           -- TODO: Is this necessary given above check?
        && not (isDictTy resultTy)

           -- Avoid instrumenting constraint selectors like eq_sel (See
           -- tests/unit-tests/T2/Main.hs)
        && (typeTypeOrConstraint resultTy /= ConstraintLike)

           -- Avoid instrumenting join points
        && not (isJoinVarExpr f)
      then do
        slogVV "\n\nAn application expression appears to be overloaded"
        slogVV "Application:"
        slogVV app

        -- Increment the count of overloaded calls in this module
        modify' $
          \s@SpecialistState{..} ->
            s
              { specialistStateOverloadedCallCount =
                  1 + specialistStateOverloadedCallCount
              }

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

        slogVV "Application function:"
        slogVV f
        slogVV "Application function type:"
        slogVV (exprType f)
        slogVV "Application type args:"
        slogVV tyArgs
        slogVV "Application value args:"
        slogVV valArgs
        slogVV "Application result type:"
        slogVV resultTy

        -- Get the wrapper function
        wrapperId <- do
          mName <- liftIO $ thNameToGhcNameIO name_cache 'specialistWrapper
          case mName of
            Just n -> lookupId n
            Nothing -> error "Specialist plugin failed to obtain the wrapper function"

        -- Get the Box type
        boxType <- do
          mName <- liftIO $ thNameToGhcNameIO name_cache 'boxTypeDUMMY
          case mName of
            Just n -> exprType . Var <$> lookupId n
            Nothing -> error "Specialist plugin failed to obtain the box type"

        -- Get the mkBox function
        mkBoxId <- do
          mName <- liftIO $ thNameToGhcNameIO name_cache 'mkBox
          case mName of
            Just n -> lookupId n
            Nothing -> error "Specialist plugin failed to obtain the Box type"

        -- Info arguments for the wrapper
        dflags <- getDynFlags
        (ss, l) <-
          gets specialistStateLastSourceNote >>=
            \case
              Just (ss, l) ->
                return (showSDoc dflags (ppr ss), l)
              Nothing ->
                return ("", "")

        uniqId <- show <$> getUniqueM
        sampleProb <- asks specialistEnvSampleProb

        dictTyPairs <-
          mapM
            ( \d -> do
                tyStr <- mkStringExpr (showPpr dflags $ exprType d)
                return $
                  mkCoreConApps
                    (tupleDataCon Boxed 2)
                    [ Type $ exprType $ mkCoreApps (Var mkBoxId) [Type $ exprType d, d]
                    , Type stringTy
                    , mkCoreApps (Var mkBoxId) [Type $ exprType d, d]
                    , tyStr
                    ]
            )
            dicts
        let
          fIdStr = mkStringLit uniqId
          lStr = mkStringLit l
          ssStr = mkStringLit ss
          sampleProbExpr = mkDoubleExpr sampleProb
          wrapperApp =
            mkCoreApps
              (Var wrapperId)
              [ Type ta
              , Type $ getRuntimeRep tb
              , Type tb
              , sampleProbExpr
              , fIdStr
              , lStr
              , ssStr
              , f'
              , mkListExpr (mkTupleTy Boxed [boxType, stringTy]) dictTyPairs
              ]


        -----------------------------------------------------------------------
        -- Cost centre insertion
        -----------------------------------------------------------------------
        insertCCs <- asks specialistEnvCostCenters

        ccWrap <-
          if insertCCs then do
            -- What is the current module?
            curMod <-
              gets specialistStateCurrentModule >>= \case
                Just m ->
                  return m
                Nothing ->
                  error "Specialist plugin could not determine current module"

            -- Extract a name and source location from the function being applied
            let
              !f_name_maybe = exprName app
              f_srcspan_maybe = nameSrcSpan <$> f_name_maybe
              cc_id_fs =
                fsLit $
                  "<OVERLOADED call to function: " ++
                  maybe "(no name available)" getOccString f_name_maybe ++
                  "> call id " ++ uniqId

            -- Cost centre index
            -- Note: This should always be zero, since we include the unique call
            -- site ID in the name
            cc_index <- mkCcIndex cc_id_fs

            let
              overloadedCc =
                mkUserCC
                  cc_id_fs
                  curMod
                  ( fromMaybe
                      (UnhelpfulSpan UnhelpfulNoLocationInfo)
                      f_srcspan_maybe
                  )
                  (mkExprCCFlavour cc_index)
              overloadedTick = ProfNote overloadedCc True True

            modify' $
              \s@SpecialistState{..} ->
                s { specialistStateLocalCcs =
                      Set.insert overloadedCc specialistStateLocalCcs
                  }

            return $ mkTick overloadedTick
          else
            return id

        let
          wrappedApp =
            ccWrap $
              mkWildCase
                wrapperApp
                (Scaled OneTy (exprType wrapperApp))
                resultTy
                [ Alt (DataAlt unitDataCon) [] $ mkCoreApps f args
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
isDictExpr e =
    case exprType' e of
      Just t ->
        -- Do not treat implicit parameters like dictionaries!
        not (isIPLikePred t) && isDictTy t
      Nothing ->
        False
  where
    exprType' :: CoreExpr -> Maybe Type
    exprType' = \case
        Type{} -> Nothing
        expr -> Just $ exprType expr

isJoinVarExpr :: CoreExpr -> Bool
isJoinVarExpr =
    \case
      Var var -> isJoinId var
      Tick _ e -> isJoinVarExpr e
      Cast e _ -> isJoinVarExpr e
      _ -> False

exprOccNameString :: CoreExpr -> Maybe String
exprOccNameString = fmap (occNameString . occName . idName) . exprId

exprOccName :: CoreExpr -> Maybe OccName
exprOccName = fmap (occName . idName) . exprId

exprName :: CoreExpr -> Maybe Name
exprName = fmap idName . exprId

exprId :: CoreExpr -> Maybe Id
exprId =
    \case
      App f _ ->
        exprId f
      Var f ->
        Just f
      Tick _ e ->
        exprId e
      Cast e _ ->
        exprId e
      _ ->
        Nothing
