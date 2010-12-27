{-# LANGUAGE BangPatterns,RankNTypes,DeriveDataTypeable #-}
module ModML.Solver.BasicDAESolver
    (
     CodeGenerationError (OtherProblem),
     AllowCodeGenError,
     IntegrationResult (FatalError, Warning, CheckedConditionFail, Result, Success),
     SolverParameters (SolverParameters),
     defaultSolverParameters,
     modelToResults,
     makeCodeFor,
     tStart,
     maxSolverStep,
     maxReportStep,
     tEnd,
     showEveryStep,
     reltol,
     abstol,
     variableOverrides
    )
where

import ModML.Core.BasicDAEModel
import Control.Monad
import Control.Monad.Error
import Data.List hiding ((\\))
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set ((\\))
import Data.Generics
import Control.Monad.State
import Data.Maybe
import Text.ParserCombinators.Parsec
import qualified System.IO as S
import qualified System.IO.Unsafe as S
import qualified System.Process as S
import System.FilePath
import System.Process
import System.Exit
import qualified Control.Exception as S
import System.Random
import Numeric
import Paths_ModML_Solver
import Data.Ord
import Data.Maybe
import System.Directory
import Control.Concurrent
import Control.Concurrent.MVar

data CodeGenerationError = OtherProblem String
instance Error CodeGenerationError where strMsg s = OtherProblem s
instance Show CodeGenerationError where showsPrec _ (OtherProblem s) = showString "Error: " . showString s
type AllowCodeGenError a = Either CodeGenerationError a

data IntegrationResult = FatalError (Int, String, String, String) |
                         Warning (Int, String, String, String) |
                         CheckedConditionFail String |
                         Result (Double, [Double], [Double]) |
                         Success deriving (Show, Read)

data SolverParameters a =
  SolverParameters {tStart :: Double,
                    maxSolverStep :: Double,
                    maxReportStep :: Double,
                    tEnd :: Double,
                    showEveryStep :: Double,
                    reltol :: Double,
                    abstol :: Double, 
                    variableOverrides :: [(a, Double)]
                   } deriving (Eq, Ord, Show)

defaultSolverParameters = SolverParameters 0 1 0.1 10 1 1E-6 1E-6 []

withTemporaryDirectory fp f = do
  ids <- forM [0..4] $ \_ -> (randomIO :: IO Int)
  let tmppart = foldl' (\f n -> f . showString "-" . showHex (abs n)) id ids "tmp"
  let fn = fp </> (tail tmppart)
  S.bracket (createDirectory fn >> return fn) removeDirectoryRecursive f

robustReadProcessWithExitCode
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO (ExitCode,String,String) -- ^ exitcode, stdout, stderr
robustReadProcessWithExitCode cmd args input = do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe }

    outMVar <- newEmptyMVar

    -- fork off a thread to start consuming stdout
    out  <- S.hGetContents outh
    _ <- forkIO $ S.evaluate (length out) >> putMVar outMVar ()

    -- fork off a thread to start consuming stderr
    err  <- S.hGetContents errh
    _ <- forkIO $ S.evaluate (length err) >> putMVar outMVar ()

    -- now write and flush any input
    when (not (null input)) $ S.handle ((\e -> return ()) :: (S.IOException -> IO ())) $
      do S.hPutStr inh input; S.hFlush inh
    S.hClose inh -- done with stdin

    -- wait on the output
    takeMVar outMVar
    takeMVar outMVar
    S.hClose outh

    -- wait on the process
    ex <- waitForProcess pid

    return (ex, out, err)

compileCodeGetResults :: [SolverParameters (Int, Int, Int)] -> String -> IO (Either CodeGenerationError [IntegrationResult])
compileCodeGetResults params code = 
    withTemporaryDirectory "./" $ \dir ->
      do
        let codegenc = dir </> "codegen.c"
        let codegenx = dir </> "codegen"
        writeFile codegenc code
        dataIncludeDir <- liftM (takeDirectory . takeDirectory) $ getDataFileName "solver-support/SolverHead.h"
        ret <- rawSystem "gcc" ["-O0", "-ggdb", "-I", ".",
                                "-I", dataIncludeDir,
                                codegenc, 
                                "-lsundials_ida", "-lsundials_nvecserial", "-lm",
                                 "-llapack", "-llevmar",
                                "-o", codegenx]
        case ret
          of
            ExitFailure _ -> return $ Left (strMsg "Model doesn't compile")
            ExitSuccess -> -- trace (show params) $
                do
                  (_, inp, _) <- robustReadProcessWithExitCode codegenx
                                  [] (show params)
                  return (return (read inp))

lookupOverrideVariables :: M.Map RealVariable Int -> M.Map RealExpression Int ->
                           SolverParameters RealVariable -> SolverParameters (Int, Int, Int)
lookupOverrideVariables varmap parammap params =
  params { variableOverrides = flip mapMaybe (variableOverrides params) $ \(var, val) ->
            liftM (\var' -> (var', val)) $ liftM3 (,,) (M.lookup var varmap) (M.lookup (RealVariableE var) parammap)
                                                       (Just . fromMaybe (-1) $ M.lookup (Derivative (RealVariableE var)) parammap)
         }

modelToResults :: BasicDAEModel -> [SolverParameters RealVariable] -> AllowCodeGenError (M.Map RealVariable Int, [IntegrationResult])
modelToResults mod [] = return (M.empty, [])
modelToResults mod params@((SolverParameters { variableOverrides = l}):_) =
   -- trace (show ("modelToResults params = ", params)) $   
    do
        (varmap, parammap, code) <- makeCodeFor mod params
        res <- S.unsafePerformIO $ compileCodeGetResults (map (lookupOverrideVariables varmap parammap) params) code
        return (varmap, res)

makeCodeFor :: BasicDAEModel -> [SolverParameters RealVariable] -> AllowCodeGenError (M.Map RealVariable Int, M.Map RealExpression Int, String)
makeCodeFor mod ((SolverParameters {variableOverrides = overrides}):_) =
  do
    let mod' = removeUnusedVariablesAndCSEs (simplifyDerivatives (simplifyMaths mod))
    let (varCount, varNumMap) = numberVariables (variables mod')
    let overridenVariables = map fst overrides
    let (paramNumMap, paramCount) = numberParameters mod' overridenVariables
    let neqs = length (equations mod')
    let nvars = length (variables mod') - (length overrides)
    when (neqs /= nvars) $
       Left $
         strMsg (showString "Number of state variables does not match number of equations: " .
                 shows neqs . showString " equations, but " .
                 shows nvars . showString " variables: Variable IDs present: " .
                 (shows $ map variableId $ variables mod') .
                 showString "; Equations: " . shows (equations mod') $ "")
    return $ (varNumMap, paramNumMap,
              (showString "#include \"solver-support/SolverHead.h\"\n" .
                (makeResFn mod' varNumMap) .
                (makeJacFn mod' varNumMap) .
                (makeBoundaryAssignmentFn mod' varNumMap overridenVariables) .
                (makeBoundaryResFn mod' paramNumMap) .
                (makeTranslateParams varNumMap paramNumMap) .
                (makeRootFn mod' varNumMap) .
                (makeVarId mod') .
                (makeSettings mod' varCount paramCount) .
                (makeConditionChecks mod varNumMap)) "")

simplifyMaths mod@(BasicDAEModel {equations = eqns, boundaryEquations = beqns, interventionRoots = ivrs,
                                  forcedInequalities = fieqs, checkedConditions = cconds }) =
  mod { equations =
           map (\(RealEquation rex1 rex2) -> RealEquation (simplifyRealExpression rex1) (simplifyRealExpression rex2) ) eqns,
        boundaryEquations = 
          map (\(bex, RealEquation rex1 rex2) ->
                (bex, RealEquation (simplifyRealExpression rex1) (simplifyRealExpression rex2))) beqns,
        interventionRoots =
          map simplifyRealExpression ivrs,
        forcedInequalities = map simplifyRealExpression fieqs,
        checkedConditions = map (\(str,bex) -> (str, simplifyBoolExpression bex)) cconds
      }

sortAndGroupBy o = groupBy (\a b -> o a b == EQ) . sortBy o

data ComputationTarget = VariableCT RealVariable | DerivativeCT RealVariable deriving (Eq, Ord, Typeable, Data)

-- | Variation of everything with an added stop condition.
everythingBut :: (r -> r -> r) -> GenericQ (r, Bool) -> GenericQ r
everythingBut k f x =
  let
    (v, deeper) = f x
  in
   if deeper
     then foldl k v (gmapQ (everythingBut k f) x)
     else v

dependencyAnalysis expr =
  everythingBut S.union (mkQ (S.empty, True) addDependency) expr

dependencyAnalysisOnRecord (targ, expr, eq) =
  (targ, (dependencyAnalysis expr, eq))

addDependency (Derivative (RealVariableE v)) = (S.singleton (DerivativeCT v), False)
addDependency (Derivative _) = (S.empty, False)
addDependency (RealVariableE v) = (S.singleton (VariableCT v), False)
addDependency _ = (S.empty, True)

buildEquationSetsForCondition (BasicDAEModel {equations = eqns, boundaryEquations = beqns}) =
  let
    boundaryByCond = sortAndGroupBy (comparing fst) beqns
  in
   mapMaybe
     (\l ->
       let
         cond = fst (head l)
         l' = map snd l
       in
        if dependencyAnalysis cond == S.empty
        then
          Just (cond, M.fromList $ map dependencyAnalysisOnRecord $ mapMaybe equationToAssignment (l' ++ eqns))
        else
          Nothing
     ) boundaryByCond

equationToAssignment (RealEquation a b) =
  case expressionToComputationTarget a
  of
    Nothing ->
      case expressionToComputationTarget b
      of
        Nothing -> Nothing
        Just b' -> Just (b', a, (b, a))
    Just a' -> Just (a', b, (a, b))
    
expressionToComputationTarget (RealVariableE rv) = Just $ VariableCT rv
expressionToComputationTarget (Derivative (RealVariableE rv)) = Just $ DerivativeCT rv
expressionToComputationTarget _ = Nothing

-- | Orders a set of values so that all possible entries appear in the list
-- | with the constraint that a value can only appear if the all dependencies
-- | in the set (first element of value pair) are 'resolved' by an earler
-- | occurrence of the map key.
resolveDependencySet :: (Ord a, Ord v) => M.Map a (S.Set a, v) -> S.Set a -> [v]
resolveDependencySet m already =
  let
    (_, rl) = foldl' (\(seen, l) item ->
                       fst $ resolveDependenciesFor m seen item l) (already, []) (M.toList m)
  in
   reverse rl

resolveDependenciesFor :: (Ord a, Ord v) => (M.Map a (S.Set a, v)) -> S.Set a -> (a, (S.Set a, v)) -> [v] -> ((S.Set a, [v]), Bool)
resolveDependenciesFor m seen (r, (deps, v)) l =
  let
    ((seen', l'), success) = foldl' (resolveOneDependency m) ((seen, l), True) (S.toList deps)
  in
   if success
   then
     ((S.insert r seen', v:l'), True)
   else
     ((seen', l'), False)

resolveOneDependency :: (Ord a, Ord v) => (M.Map a (S.Set a, v)) -> ((S.Set a, [v]), Bool) -> a -> ((S.Set a, [v]), Bool)
resolveOneDependency m ((seen, l), success) dep =
  if S.member dep seen
  then
    ((seen, l), success)
  else
    case M.lookup dep m
    of
      Nothing -> ((seen, l), False)
      Just item -> resolveDependenciesFor m seen (dep, item) l

findSimpleInitialisation mod nameMap cseNo0 realCSEMap0 boolCSEMap0 eqset overridenvars =
  let
    ordeq = resolveDependencySet eqset overridenvars
    (_, _, _, ss) =
      foldl' (\(cseNo, realCSEMap, boolCSEMap, sh) (lhs, rhs) ->
               let
                 knownRealCSEs = S.fromList $ M.keys realCSEMap
                 knownBoolCSEs = S.fromList $ M.keys boolCSEMap
                 usedRealCSEs = everything S.union (S.empty `mkQ` findOneUsedRealCSE) (lhs, rhs)
                 usedBoolCSEs = everything S.union (S.empty `mkQ` findOneUsedBoolCSE) (lhs, rhs)
                 newRealCSEs = usedRealCSEs \\ knownRealCSEs
                 newBoolCSEs = usedBoolCSEs \\ knownBoolCSEs
                 (cseNo', newRealCSEMap, newBoolCSEMap, realcses) = buildRealCSEs' mod nameMap cseNo realCSEMap boolCSEMap newRealCSEs
                 realCSEMap' = realCSEMap `M.union` newRealCSEMap
                 boolCSEMap' = boolCSEMap `M.union` newBoolCSEMap
                 (cseNo'', _, newBoolCSEMap', boolcses) = buildBoolCSEs mod nameMap cseNo' realCSEMap' boolCSEMap' newBoolCSEs
                 boolCSEMap'' = boolCSEMap' `M.union` newBoolCSEMap'
               in
                (cseNo'', realCSEMap', boolCSEMap'',
                 sh . realcses . boolcses .
                 realExpressionToString nameMap realCSEMap' boolCSEMap'' lhs .
                 showString " = " . realExpressionToString nameMap realCSEMap' boolCSEMap'' rhs .
                 showString ";\n")
             ) (cseNo0, realCSEMap0, boolCSEMap0, id) ordeq
  in
   ss
  
findSimpleInitialisations nameMap mod overridenvars =
  let
    overridenvarsset = (S.fromList (map VariableCT overridenvars)) `S.union`
                       (S.fromList (map DerivativeCT overridenvars))
    eqsets = buildEquationSetsForCondition mod
    conds = map fst eqsets
    usedRealCSEs = everything S.union (S.empty `mkQ` findOneUsedRealCSE) conds
    usedBoolCSEs = everything S.union (S.empty `mkQ` findOneUsedBoolCSE) conds
    (cseNo, realCSEMap, boolCSEMap', realcses) = buildRealCSEs mod nameMap usedRealCSEs
    (cseNo', _, boolCSEMap, boolcses) = buildBoolCSEs mod nameMap cseNo realCSEMap boolCSEMap' usedBoolCSEs
  in
   realcses . boolcses .   
   (foldl' (\s (cond, eqset) ->
     let
       in
        s . showString "if (" . boolExpressionToString undefined realCSEMap boolCSEMap cond .
        showString ")\n\
        \{\n" . findSimpleInitialisation mod nameMap cseNo' realCSEMap boolCSEMap eqset overridenvarsset .
        showString "}\n") id eqsets)
    
makeBoundaryAssignmentFn mod varNumMap overridenVars =
  showString "void boundaryAssignment(double t, double* v, double* dv)\n\
             \{\n" .
  findSimpleInitialisations (variableOrDerivDisplay varNumMap) mod overridenVars .
  showString "}\n"

makeTranslateParams varNumMap paramNumMap =
    showString "void translateParams(N_Vector y, N_Vector derivy, double* params)\n\
               \{\n\
               \ double * v = N_VGetArrayPointer(y),\n\
               \        * dv = N_VGetArrayPointer(derivy);\n" .
    (foldl' (translateOneParam varNumMap) id (M.toList paramNumMap)) .
    showString "\n\
               \}\n\
               \void reverseTranslateParams(N_Vector y, N_Vector derivy, double* params)\n\
               \{\n\
               \ double * v = N_VGetArrayPointer(y),\n\
               \        * dv = N_VGetArrayPointer(derivy);\n" .
    (foldl' (reverseTranslateOneParam varNumMap) id (M.toList paramNumMap)) .
    showString "\n\
               \}\n"

translateOneParam varNumMap s ((RealVariableE v), paramNo) =
  s . showString "v" . translateOneParamVar varNumMap v paramNo
translateOneParam varNumMap s ((Derivative (RealVariableE v)), paramNo) =
  s . showString "dv" . (translateOneParamVar varNumMap v paramNo)

translateOneParamVar varNumMap v paramNo =
  showString "[" . shows ((M.!) varNumMap v) . showString "] = params[" .
  shows paramNo . showString "];\n"

reverseTranslateOneParam varNumMap s ((RealVariableE v), paramNo) =
  s . reverseTranslateOneParamVar "v" varNumMap v paramNo
reverseTranslateOneParam varNumMap s ((Derivative (RealVariableE v)), paramNo) =
  s . reverseTranslateOneParamVar "dv" varNumMap v paramNo

reverseTranslateOneParamVar n varNumMap v paramNo =
  showString "params[" . shows paramNo . showString "] = " . showString n .
  showString "[" . shows ((M.!) varNumMap v) . showString "];\n"

numberParameters :: BasicDAEModel -> [RealVariable] -> (M.Map RealExpression Int, Int)
numberParameters mod overrides =
  execState (everywhereM (mkM numberOneParameter) (mod, map (Derivative . RealVariableE) overrides)) (M.empty, 0)

insertExpression e = do
  (paramNumMap, nextNum) <- get
  case (M.lookup e paramNumMap) of
    Nothing -> do
      put (M.insert e nextNum paramNumMap, nextNum + 1)
      return e
    Just _ -> return e

numberOneParameter (e@(Derivative (RealVariableE _))) = insertExpression e
numberOneParameter e@(RealVariableE _) = insertExpression e
  
numberOneParameter e = return e

makeVarId mod =
  let
      varsWithDerivs = everything S.union (S.empty `mkQ` findOneDeriv) (equations mod)
  in
   showString "static void setupIdVector(N_Vector id)\n\
              \{\n\
              \  double *r = N_VGetArrayPointer(id);\n" .
   foldl' (\s (i, v) -> s . showString "r[" .
                        shows i . showString "] = " .
                        showString (boolToDoubleStr $ S.member v varsWithDerivs) .
                        showString ";\n") id
      (zip [0..] $ variables mod) .
   showString "}\n"

findOneDeriv (Derivative (RealVariableE v)) = S.singleton v
findOneDeriv _ = S.empty
boolToDoubleStr True = "1.0"
boolToDoubleStr False = "0.0"

makeSettings mod varCount paramCount =
  showString "static int gNumVars = " . shows varCount . showString ";\n\
   \static int gNumEquations = " . shows (length $ equations mod) . showString ";\n\
   \static int gNumBoundaryEquations = " . shows ((length $ boundaryEquations mod) + (length $ forcedInequalities mod)) . showString ";\n\
   \static int gNumParams = " . shows paramCount . showString ";\n\
   \static int gNumInterventions = " . shows (length $ interventionRoots mod) .
  showString ";\n"

makeJacFn mod varNumMap =
  let
    (mod', cseDerivMap) = makeDerivativeCSEs mod
    numberedResids = zip [0..] (map (\(RealEquation ex1 ex2) -> ex1 `Minus` ex2) $ equations mod')
    numberedVars = zip [0..] (variables mod')
    jacOrds = [(res, wrt) | res <- numberedResids, wrt <- numberedVars]
    jacAssignments = map (\((nr,r),(nv,v)) ->
                           ((nr, nv),
                            simplifyRealExpression $ takePartialDerivative cseDerivMap (VariableCT v) r,
                            simplifyRealExpression $ takePartialDerivative cseDerivMap (DerivativeCT v) r
                            )) jacOrds
    usedRealCSEs = everything S.union (S.empty `mkQ` findOneUsedRealCSE) jacAssignments
    usedBoolCSEs = everything S.union (S.empty `mkQ` findOneUsedBoolCSE) jacAssignments
    (cseNo, realCSEMap, boolCSEMap', realcses) = buildRealCSEs mod (variableOrDerivDisplay varNumMap) usedRealCSEs
    (_, _, boolCSEMap, boolcses) = buildBoolCSEs mod (variableOrDerivDisplay varNumMap) cseNo realCSEMap boolCSEMap' usedBoolCSEs
  in
   showString "static int modelJacobian(double t, double* v, double* dv, double alpha, double** jac)\n\
     \{\n" . realcses . boolcses . 
     makeJacobian jacAssignments varNumMap realCSEMap boolCSEMap .
     showString "  return 0;\n\
     \}\n"

makeJacobian :: [((Int, Int), RealExpression, RealExpression)] -> M.Map RealVariable Int ->
                M.Map RealCommonSubexpression String -> M.Map BoolCommonSubexpression String ->
                String -> String
makeJacobian jacAssignments varNumMap realCSEMap boolCSEMap =
  let
    jacStrs =
      map (\(o, ex1, ex2) ->
            (o, realExpressionToString (variableOrDerivDisplay varNumMap) realCSEMap boolCSEMap ex1,
             realExpressionToString (variableOrDerivDisplay varNumMap) realCSEMap boolCSEMap ex2
            )
          ) jacAssignments
  in
   -- The Jacobian column is the variable index, row is the residual index.
   foldl' (\s ((nr,nv), ex1, ex2) ->
            s . showString "  jac[" . shows nv . showString "][" . shows nr . showString "] = (" .
            ex1 . showString ") + alpha*(" . ex2 . showString ");\n") id jacStrs

makeResFn mod varNumMap =
  let
      considerCSEIn = (equations mod, forcedInequalities mod)
      usedRealCSEs = everything S.union (S.empty `mkQ` findOneUsedRealCSE) considerCSEIn
      usedBoolCSEs = everything S.union (S.empty `mkQ` findOneUsedBoolCSE) considerCSEIn
      (cseNo, realCSEMap, boolCSEMap', realcses) = buildRealCSEs mod (variableOrDerivDisplay varNumMap) usedRealCSEs
      (_, _, boolCSEMap, boolcses) = buildBoolCSEs mod (variableOrDerivDisplay varNumMap) cseNo realCSEMap boolCSEMap' usedBoolCSEs
  in
    showString "static int modelResiduals(double t, double* v, double* dv, double* res)\n\
               \{\n" .
    realcses . boolcses .
    (makeResiduals mod varNumMap realCSEMap boolCSEMap) .
    showString "  return 0;\n\
               \}\n"

makeBoundaryResFn mod paramNumMap =
  let
      considerCSEIn = (equations mod, forcedInequalities mod, boundaryEquations mod)
      usedRealCSEs = everything S.union (S.empty `mkQ` findOneUsedRealCSE) considerCSEIn
      usedBoolCSEs = everything S.union (S.empty `mkQ` findOneUsedBoolCSE) considerCSEIn
      (cseNo, realCSEMap, boolCSEMap', realcses) = buildRealCSEs mod (paramDisplay paramNumMap) usedRealCSEs
      (_, _, boolCSEMap, boolcses) = buildBoolCSEs mod (paramDisplay paramNumMap) cseNo realCSEMap boolCSEMap' usedBoolCSEs
  in
    showString "static int boundaryResiduals(double t, double *params, double *res)\n\
               \{\n" .
    realcses . boolcses .
    makeBoundaryResiduals mod paramNumMap realCSEMap boolCSEMap .
    showString "  return 0;\n\
               \}\n"

makeRootFn :: BasicDAEModel -> M.Map RealVariable Int -> (String -> String)
makeRootFn mod varNumMap =
  let
      usedRealCSEs = everything S.union (S.empty `mkQ` findOneUsedRealCSE) (interventionRoots mod)
      usedBoolCSEs = everything S.union (S.empty `mkQ` findOneUsedBoolCSE) (interventionRoots mod)
      (cseNo, realCSEMap, boolCSEMap', realcses) = buildRealCSEs mod (variableOrDerivDisplay varNumMap) usedRealCSEs
      (_, _, boolCSEMap, boolcses) = buildBoolCSEs mod (variableOrDerivDisplay varNumMap) cseNo realCSEMap boolCSEMap' usedBoolCSEs
  in
    showString "static int modelRoots(double t, N_Vector y, N_Vector derivy, realtype *gout, void *user_data)\n\
               \{\n\
               \  double* v = N_VGetArrayPointer(y),\n\
               \        * dv = N_VGetArrayPointer(derivy);\n" .
    realcses . boolcses .
    makeInterventionRoots mod varNumMap realCSEMap boolCSEMap .
    showString "  return 0;\n\
               \}\n"

makeInterventionRoots :: BasicDAEModel -> M.Map RealVariable Int -> M.Map RealCommonSubexpression String -> M.Map BoolCommonSubexpression String -> String -> String
makeInterventionRoots mod varMap realCSEMap boolCSEMap =
  foldl' (\s v -> s . makeOneInterventionRoot varMap realCSEMap boolCSEMap v) id (zip [0..] (interventionRoots mod))

makeOneInterventionRoot varMap realCSEMap boolCSEMap (i, root) =
  showString "gout[" . shows i . showString "] = " .
  realExpressionToString (variableOrDerivDisplay varMap) realCSEMap boolCSEMap root .
  showString ";\n"

escapeCString [] = []
escapeCString (c:s)
  | c == '\n' = '\\':'n':(escapeCString s)
  | c == '\r' = '\\':'n':(escapeCString s)
  | c == '\\' = '\\':'\\':(escapeCString s)
  | c == '\"' = '\\':'\"':(escapeCString s)
  | otherwise = c:(escapeCString s)

oneConditionCheck :: M.Map RealVariable Int -> M.Map RealCommonSubexpression String -> M.Map BoolCommonSubexpression String -> (String -> String) -> (String, BoolExpression) -> String -> String
oneConditionCheck varNumMap realCSEMap boolCSEMap s (msg, cond) =
    s . showString "if (" .
    boolExpressionToString (variableOrDerivDisplay varNumMap) realCSEMap boolCSEMap cond .
    showString ")\n\
      \{\n\
      \  checkedConditionFail(\"" . showString (escapeCString msg) . showString "\");\n\
      \  return -1;\n\
      \}\n"

makeConditionChecks mod@(BasicDAEModel { checkedConditions = ccList}) varNumMap =
    let
      considerCSEIn = checkedConditions
      usedRealCSEs = everything S.union (S.empty `mkQ` findOneUsedRealCSE) considerCSEIn
      usedBoolCSEs = everything S.union (S.empty `mkQ` findOneUsedBoolCSE) considerCSEIn
      (cseNo, realCSEMap, boolCSEMap', realcses) = buildRealCSEs mod (variableOrDerivDisplay varNumMap) usedRealCSEs
      (_, _, boolCSEMap, boolcses) = buildBoolCSEs mod (variableOrDerivDisplay varNumMap) cseNo realCSEMap boolCSEMap' usedBoolCSEs
    in
      showString "static int checkConditions(double t, N_Vector y, N_Vector derivy)\n\
        \{\n\
        \  double* v = N_VGetArrayPointer(y),\n\
        \        * dv = N_VGetArrayPointer(derivy);\n" .
      realcses . boolcses .
      foldl' (oneConditionCheck varNumMap realCSEMap boolCSEMap) id ccList .
      showString "  return 0;\n}\n"

inequalityResidual vnf realCSEMap boolCSEMap ieq =
  showString "-min(0, " . (realExpressionToString vnf realCSEMap boolCSEMap ieq) .
  showString ")"
inequalityTest vnf realCSEMap boolCSEMap ieq =
  showString "((" . (realExpressionToString vnf realCSEMap boolCSEMap ieq) . showString ") > 0)"
makeResidual (n, str) =
    showString "res[" . shows n . showString "] = " . str . showString ";\n"

equationToResidualString vnf realCSEMap boolCSEMap (RealEquation e1 e2) =
    showString "((" . realExpressionToString vnf realCSEMap boolCSEMap e1 .
    showString ") - (" .
    realExpressionToString vnf realCSEMap boolCSEMap e2 .
    showString "))"

buildRealCSE m vnf cse@(RealCommonSubexpression id ex) st@(nalloc, realCSEMap, boolCSEMap, s)
    | isJust $ M.lookup cse realCSEMap = st
    | otherwise = buildRealCSENotExists m vnf cse st

buildRealCSENotExists m vnf cse@(RealCommonSubexpression id ex) st@(nalloc, realCSEMap, boolCSEMap, s) =
        let
            neededRealCSEs = everything S.union (S.empty `mkQ` findOneUsedRealCSE) ex
            neededBoolCSEs = everything S.union (S.empty `mkQ` findOneUsedBoolCSE) ex
        in
         buildRealCSEFromNeeded m vnf cse st neededRealCSEs neededBoolCSEs

buildRealCSEFromNeeded m vnf cse@(RealCommonSubexpression id ex)
                       st@(nalloc, realCSEMap, boolCSEMap, s) neededRealCSEs
                       neededBoolCSEs =
        let
            st' :: (Int,M.Map RealCommonSubexpression String,M.Map BoolCommonSubexpression String,String -> String)
            st' = S.fold (buildRealCSE m vnf) st neededRealCSEs
            (nalloc'', realCSEMap'', boolCSEMap'', s'') =
                 S.fold (buildBoolCSE m vnf) st' neededBoolCSEs
            v = "c" ++ show nalloc''
        in
          (nalloc'' + 1, M.insert cse v realCSEMap'', boolCSEMap'',
           s'' . showString "double " . showString v . showString " = " .
           realExpressionToString vnf realCSEMap'' boolCSEMap'' ex . showString ";\n")

buildBoolCSE :: BasicDAEModel -> (RealExpression -> String) -> BoolCommonSubexpression ->
                (Int, M.Map RealCommonSubexpression String, M.Map BoolCommonSubexpression String, String -> String) ->
                (Int, M.Map RealCommonSubexpression String, M.Map BoolCommonSubexpression String, String -> String)
buildBoolCSE m vm cse@(BoolCommonSubexpression id ex) st@(nalloc, realCSEMap, boolCSEMap, s)
    | isJust $ M.lookup cse boolCSEMap = st
    | otherwise =
        let
            neededBoolCSEs = everything S.union (S.empty `mkQ` findOneUsedBoolCSE) ex
            (nalloc', realCSEMap', boolCSEMap', s') =
                 S.fold (buildBoolCSE m vm) (nalloc, realCSEMap, boolCSEMap, s) neededBoolCSEs
            neededRealCSEs = everything S.union (S.empty `mkQ` findOneUsedRealCSE) ex
            (nalloc'', realCSEMap'', boolCSEMap'', s'') =
                 S.fold (buildRealCSE m vm) (nalloc', realCSEMap', boolCSEMap', s') neededRealCSEs
            v = "c" ++ show nalloc''
        in
          (nalloc'' + 1, realCSEMap'', M.insert cse v boolCSEMap'', 
           s'' . showString "int " . showString v . showString " = " .
           boolExpressionToString vm realCSEMap'' boolCSEMap'' ex .
           showString ";\n")

buildRealCSEs' :: BasicDAEModel -> (RealExpression -> String) -> Int -> M.Map RealCommonSubexpression String ->
                  M.Map BoolCommonSubexpression String -> S.Set RealCommonSubexpression ->
                  (Int, M.Map RealCommonSubexpression String, M.Map BoolCommonSubexpression String, String -> String)
buildRealCSEs' model vm cseNo realCSEMap boolCSEMap =
  S.fold (buildRealCSE model vm) (cseNo, realCSEMap, boolCSEMap, id)
buildRealCSEs model vm = buildRealCSEs' model vm 0 M.empty M.empty
buildBoolCSEs model vm cseNo realCSEMap boolCSEMap =
    S.fold (buildBoolCSE model vm) (cseNo, realCSEMap, boolCSEMap, id)

boolExpressionToString :: (RealExpression -> String) -> M.Map RealCommonSubexpression String -> M.Map BoolCommonSubexpression String -> BoolExpression -> String -> String
boolExpressionToString _ _ _ (BoolConstant True) = showString "1"
boolExpressionToString _ _ _ (BoolConstant False) = showString "0"
boolExpressionToString _ _ bcem (BoolCommonSubexpressionE bce) = showString $ (M.!) bcem bce
boolExpressionToString vm rcem bcem (be1 `And` be2) =
  showString "("  . boolExpressionToString vm rcem bcem be1 .
  showString ")&&(" .
  boolExpressionToString vm rcem bcem be2 .
  showString ")"

boolExpressionToString vm rcem bcem (be1 `Or` be2) =
  showString "(" . boolExpressionToString vm rcem bcem be1 .
  showString ")||(" . boolExpressionToString vm rcem bcem be2 .
  showString ")"

boolExpressionToString vm rcem bcem (Not be1) =
  showString "!(" . boolExpressionToString vm rcem bcem be1 . showString ")"

boolExpressionToString vm rcem bcem (re1 `LessThan` re2) =
  showString "(" . realExpressionToString vm rcem bcem re1 .
  showString ")<(" . realExpressionToString vm rcem bcem re2 .
  showString ")"
boolExpressionToString vm rcem bcem (re1 `Equal` re2) =
  showString "(" . realExpressionToString vm rcem bcem re1 .
  showString ")==(" . realExpressionToString vm rcem bcem re2 . 
  showString ")"

variableOrDerivDisplay :: M.Map RealVariable Int -> RealExpression -> String
variableOrDerivDisplay vm (RealVariableE v) =
  (showString "v[" . shows ((M.!) vm v)) "]"
variableOrDerivDisplay vm (Derivative (RealVariableE v)) =
  (showString "dv[" . shows ((M.!) vm v)) "]"
variableOrDerivDisplay vm _ = undefined

paramDisplay :: M.Map RealExpression Int -> RealExpression -> String
paramDisplay m ex = (showString "params[" . shows ((M.!) m ex)) "]"

realExpressionToString :: (RealExpression -> String) -> M.Map RealCommonSubexpression String -> M.Map BoolCommonSubexpression String -> RealExpression -> String -> String
realExpressionToString _ _ _ (RealConstant c) = shows c
realExpressionToString vm _ _ e@(RealVariableE v) = showString (vm e)
realExpressionToString _ _ _ (BoundVariableE) = showString "t"
realExpressionToString vm _ _ e@(Derivative (RealVariableE v)) = showString (vm e)
realExpressionToString _ rcem _ (RealCommonSubexpressionE cse) = showString ((M.!) rcem cse)
realExpressionToString vm rcem bcem (If b1 r1 r2) =
  showString "(" .
  boolExpressionToString vm rcem bcem b1 .
  showString ") ? (" .
  realExpressionToString vm rcem bcem r1 .
  showString ") : (" . realExpressionToString vm rcem bcem r2 .
  showString ")"
realExpressionToString vm rcem bcem (r1 `Plus` r2) =
  showString "(" . realExpressionToString vm rcem bcem r1 .
  showString ")+(" . realExpressionToString vm rcem bcem r2 .
  showString ")"
realExpressionToString vm rcem bcem (r1 `Minus` r2) =
    showString "(" . realExpressionToString vm rcem bcem r1 .
    showString ")-(" .
    realExpressionToString vm rcem bcem r2 . showString ")"
realExpressionToString vm rcem bcem (r1 `Times` r2) =
  showString "(" . realExpressionToString vm rcem bcem r1 .
  showString ")*(" . realExpressionToString vm rcem bcem r2 . 
  showString ")"
realExpressionToString vm rcem bcem (r1 `Divided` r2) =
  showString "(" . realExpressionToString vm rcem bcem r1 .
  showString ")/(" . realExpressionToString vm rcem bcem r2 . 
  showString ")"
realExpressionToString vm rcem bcem (r1 `Power` r2) =
  showString "pow((" . realExpressionToString vm rcem bcem r1 .
  showString "), (" . realExpressionToString vm rcem bcem r2 . showString "))"
realExpressionToString vm rcem bcem (Floor r1) = 
  showString "floor(" . realExpressionToString vm rcem bcem r1 . showString ")"
realExpressionToString vm rcem bcem (Ceiling r1) =
  showString "ceiling(" . realExpressionToString vm rcem bcem r1 . showString ")"
realExpressionToString vm rcem bcem (LogBase r1 r2) =
  showString "log(" . realExpressionToString vm rcem bcem r2 . showString ") / log(" .
  realExpressionToString vm rcem bcem r1 . showString ")"
realExpressionToString vm rcem bcem (Sin r1) =
  showString "sin(" . realExpressionToString vm rcem bcem r1 . 
  showString ")"
realExpressionToString vm rcem bcem (Tan r1) =
  showString "tan(" . realExpressionToString vm rcem bcem r1 . showString ")"
realExpressionToString vm rcem bcem (Cos r1) =
  showString "cos(" . realExpressionToString vm rcem bcem r1 . showString ")"
realExpressionToString vm rcem bcem (ASin r1) =
  showString "asin(" . realExpressionToString vm rcem bcem r1 . showString ")"
realExpressionToString vm rcem bcem (ATan r1) =
  showString "atan(" . realExpressionToString vm rcem bcem r1 . showString ")"
realExpressionToString vm rcem bcem (ACos r1) =
  showString "acos(" . realExpressionToString vm rcem bcem r1 . showString ")"
realExpressionToString vm rcem bcem (Sinh r1) =
  showString "sinh(" . realExpressionToString vm rcem bcem r1 . showString ")"
realExpressionToString vm rcem bcem (Tanh r1) =
  showString "tanh(" . realExpressionToString vm rcem bcem r1 . showString ")"
realExpressionToString vm rcem bcem (Cosh r1) =
  showString "cosh(" . realExpressionToString vm rcem bcem r1 . showString ")"
realExpressionToString vm rcem bcem (ASinh r1) =
  showString "asinh(" . realExpressionToString vm rcem bcem r1 . showString ")"
realExpressionToString vm rcem bcem (ATanh r1) =
  showString "atanh(" . realExpressionToString vm rcem bcem r1 . showString ")"
realExpressionToString vm rcem bcem (ACosh r1) =
  showString "acosh(" . realExpressionToString vm rcem bcem r1 . showString ")"
realExpressionToString vm rcem bcem (RealExpressionTag _ r) =
  realExpressionToString vm rcem bcem r

makeResiduals :: BasicDAEModel -> M.Map RealVariable Int -> M.Map RealCommonSubexpression String ->
                 M.Map BoolCommonSubexpression String -> String -> String
makeResiduals m@(BasicDAEModel { equations = eqns, forcedInequalities = ieqs }) varNumMap realCSEMap boolCSEMap =
    let
        ieqStrList = map (flip (inequalityTest (variableOrDerivDisplay varNumMap) realCSEMap
                                boolCSEMap) "") ieqs
        eqnStrList = map (equationToResidualString (variableOrDerivDisplay varNumMap) realCSEMap boolCSEMap) eqns
    in
     foldl' (\s v -> s . makeResidual v) id (zip [0..] eqnStrList) .
     if null ieqStrList then id else
       showString "if (!(" . showString (intercalate "&&" ieqStrList) . showString "))\n\
         \  return 1;\n"

makeBoundaryResiduals :: BasicDAEModel -> M.Map RealExpression Int -> M.Map RealCommonSubexpression String ->
                 M.Map BoolCommonSubexpression String -> String -> String
makeBoundaryResiduals m@(BasicDAEModel { equations = meqns, boundaryEquations = beqns, forcedInequalities = ieqs }) paramNumMap realCSEMap boolCSEMap =
    let
        eqns = (map ((,) (BoolConstant True)) meqns) ++ beqns
        estrList = map (\(c, eq) -> (boolExpressionToString (paramDisplay paramNumMap) realCSEMap boolCSEMap c,
                                    equationToResidualString (paramDisplay paramNumMap) realCSEMap boolCSEMap eq)) eqns
        ieqStrList = map (\ieq -> (showString "1", inequalityResidual (paramDisplay paramNumMap) realCSEMap boolCSEMap ieq)) ieqs
        strList = estrList ++ ieqStrList
    in
      foldl' (\s (i, (c, res)) ->
               s .
               (if c "" == "1"
                then
                  makeResidual (i, res)
                else
                  showString "if (" . c . showString ")\n " . makeResidual (i, res) .
                  showString "else\n " . makeResidual (i, showString "0")
               )
             ) id (zip [0..] strList)

simplifyDerivatives mod =
  snd $
    until ((==0) . fst) (\(_, mod') -> simplifyDerivativesRound mod' (buildVarMap mod')) (1, mod)
simplifyDerivativesRound mod varmap =
    (count, mod' { variables = varlist, nextID = nextIDv, equations = equationlist ++ (equations mod')})
    where
      (mod', (count, varmap', varlist, equationlist, nextIDv)) =
          runState (everywhereM (mkM simplifyOneDerivative) mod) (0, varmap, variables mod, [], nextID mod)

simplifyOneDerivative d@(Derivative (RealVariableE _)) = return d
simplifyOneDerivative (Derivative ex) =
  do
    ex' <- everywhereM (mkM simplifyOneDerivative) ex
    (count, varmap, varlist, eqnlist, id) <- get
    let mv = M.lookup ex' varmap
    let (v, varmap', varlist', eqnlist', id') = case mv
         of
           Just v -> (v, varmap, varlist, eqnlist, id)
           Nothing ->
            (v, M.insert ex' v varmap, v:varlist, (RealEquation (RealVariableE v) ex'):eqnlist, id - 1)
              where
                v = RealVariable id
    put (count + 1, varmap', varlist', eqnlist', id')
    return $ Derivative (RealVariableE v)
simplifyOneDerivative d = return d

buildVarMap m = foldl' processEquationForVarMap M.empty (equations m)
processEquationForVarMap st (RealEquation (RealVariableE var) ex) =
    M.insert ex var st
processEquationForVarMap st (RealEquation ex1 ex2@(RealVariableE var)) = processEquationForVarMap st (RealEquation ex2 ex1)
processEquationForVarMap st _ = st

removeUnusedVariablesAndCSEs mod =
  let
    usedRealVars = everything S.union (S.empty `mkQ` findOneUsedRealVariable) (equations mod)
    considerCSEIn = (equations mod, boundaryEquations mod, interventionRoots mod, forcedInequalities mod, checkedConditions mod)
    usedRealCSEs = everything S.union (S.empty `mkQ` findOneUsedRealCSE) considerCSEIn
    usedBoolCSEs = everything S.union (S.empty `mkQ` findOneUsedBoolCSE) considerCSEIn
  in
    mod { variables = S.toList $ usedRealVars,
          commonSubexpressions = map FromRealCommonSubexpression (S.toList $ usedRealCSEs) ++
                                 map FromBoolCommonSubexpression (S.toList $ usedBoolCSEs) }

findOneUsedRealVariable :: RealVariable -> S.Set RealVariable
findOneUsedRealVariable = S.singleton
findOneUsedRealCSE :: RealCommonSubexpression -> S.Set RealCommonSubexpression
findOneUsedRealCSE = S.singleton
findOneUsedBoolCSE :: BoolCommonSubexpression -> S.Set BoolCommonSubexpression
findOneUsedBoolCSE = S.singleton

numberVariables vars =
    (length vars, M.fromList $ zip vars [0..])

makeDerivativeCSEs :: BasicDAEModel -> (BasicDAEModel, M.Map (RealCommonSubexpression, ComputationTarget) RealCommonSubexpression)
makeDerivativeCSEs m =
  let
    rcses = extractDirectRealSubexpressions (equations m)
    (cm, nid) = makeDerivativeCSEsForList (variables m) (nextID m) M.empty (S.toList rcses)
    newSubexpressions = map FromRealCommonSubexpression (M.elems cm)
  in
   (m{nextID=nid, commonSubexpressions=commonSubexpressions m ++ newSubexpressions}, cm)
    
makeDerivativeCSEsForList vars nid cm cses =
  foldl' (deriveOneCSE vars) (cm, nid)
    [(cse, ct) | cse <- cses,
                 ct <- (map VariableCT vars) ++ (map DerivativeCT vars)]

extractDirectRealSubexpressions expr = everythingBut S.union (extQ (mkQ (S.empty, True) extractOneRealDirectSubexpression)
                                                              stopOnBoolDirectSubexpression) expr
extractOneRealDirectSubexpression (RealCommonSubexpressionE rce) = (S.singleton rce, False)
extractOneRealDirectSubexpression _ = (S.empty, True)
stopOnBoolDirectSubexpression (BoolCommonSubexpressionE bce) = (S.empty, False)
stopOnBoolDirectSubexpression _ = (S.empty, True)

deriveOneCSE vars (cm, nid) (targ@(cse@(RealCommonSubexpression _ ex), ct)) =
  case M.lookup targ cm
  of
    Just _ -> (cm, nid)
    Nothing ->
      let
        cses = S.toList $ extractDirectRealSubexpressions cse
        (cm', nid') = makeDerivativeCSEsForList vars nid cm cses
      in
       case M.lookup targ cm'
       of
         Just _ -> (cm', nid')
         Nothing ->
           let
             deriv = simplifyRealExpression $ takePartialDerivative cm' ct ex
           in
            (M.insert targ (RealCommonSubexpression nid' deriv) cm', nid' + 1)

takePartialDerivative _ _ (RealConstant _) = RealConstant 0
takePartialDerivative _ (VariableCT rv1) (RealVariableE rv2)
  | rv1 == rv2 = RealConstant 1
  | otherwise = RealConstant 0
takePartialDerivative _ _ (RealVariableE _) = RealConstant 0
takePartialDerivative _ _ BoundVariableE = RealConstant 0
takePartialDerivative _ (DerivativeCT rv1) (Derivative (RealVariableE rv2))
  | rv1 == rv2 = RealConstant 1
  | otherwise = RealConstant 0
-- This assumes all derivatives are of variables (we normalise to that earlier).
takePartialDerivative _ _ (Derivative _) = RealConstant 0
takePartialDerivative cdm ct (RealCommonSubexpressionE cse) =
  RealCommonSubexpressionE (fromMaybe (error "takePartialDerivative: Derivative map missing an entry")
                            (M.lookup (cse, ct) cdm))
takePartialDerivative cdm ct (If cond ex1 ex2) =
  If cond (takePartialDerivative cdm ct ex1) (takePartialDerivative cdm ct ex2)
takePartialDerivative cdm ct (Plus ex1 ex2) =
  Plus (takePartialDerivative cdm ct ex1) (takePartialDerivative cdm ct ex2)
takePartialDerivative cdm ct (Minus ex1 ex2) =
  Minus (takePartialDerivative cdm ct ex1) (takePartialDerivative cdm ct ex2)
takePartialDerivative cdm ct (Times ex1 ex2) =
  (ex1 `Times` takePartialDerivative cdm ct ex2) `Plus`
  (ex2 `Times` takePartialDerivative cdm ct ex1)
takePartialDerivative cdm ct (Divided ex1 ex2) =
  ((takePartialDerivative cdm ct ex1 `Times` ex2) `Minus` (takePartialDerivative cdm ct ex2 `Times` ex1)) `Divided`
  (Power ex2 (RealConstant 2))
  
-- This specialised case helps to avoid division by zero problems when
-- differentiating squares etc; we wouldn't need it if we had a more advanced
-- simplifier.
takePartialDerivative cdm ct (Power exb (RealConstant cn)) =
  (RealConstant cn) `Times` (exb `Power` (RealConstant (cn - 1))) `Times` (takePartialDerivative cdm ct exb)
  
takePartialDerivative cdm ct (Power exb expo) =
  (Power exb expo) `Times` (((expo `Divided` exb) `Times` (takePartialDerivative cdm ct exb)) `Plus`
                            ((takePartialDerivative cdm ct expo) `Times` (LogBase (RealConstant (exp 1)) exb)))
takePartialDerivative _ _ (Floor _) = RealConstant 0
takePartialDerivative _ _ (Ceiling _) = RealConstant 0
takePartialDerivative cdm ct (LogBase ex1 ex2) = 
  ((((takePartialDerivative cdm ct ex2) `Divided` ex2) `Times`
    (LogBase (RealConstant (exp 1)) ex1)) `Minus`
   ((takePartialDerivative cdm ct ex1 `Divided` ex1) `Times`
    LogBase (RealConstant (exp 1)) ex2)) `Divided`
  (Power (LogBase (RealConstant (exp 1)) ex1) (RealConstant 2))
  
takePartialDerivative cdm ct (Sin ex) = takePartialDerivative cdm ct ex `Times` Cos ex
takePartialDerivative cdm ct (Cos ex) = RealConstant 0 `Minus` (takePartialDerivative cdm ct ex `Times` Sin ex)
takePartialDerivative cdm ct (Tan ex) = takePartialDerivative cdm ct ex `Times`
                                        (RealConstant 1 `Divided` (Power (Cos ex) (RealConstant 2)))
takePartialDerivative cdm ct (ASin ex) = takePartialDerivative cdm ct ex `Divided`
                                         (Power (RealConstant 1 `Minus` (ex `Power` RealConstant 2)) (RealConstant 0.5))
takePartialDerivative cdm ct (ACos ex) = RealConstant 0 `Minus`
                                         (takePartialDerivative cdm ct ex `Divided`
                                          (Power (RealConstant 1 `Minus` (ex `Power` RealConstant 2))
                                                 (RealConstant 0.5)))
takePartialDerivative cdm ct (ATan ex) = takePartialDerivative cdm ct ex `Divided`
                                           (RealConstant 1 `Plus` (ex `Power` RealConstant 2))
takePartialDerivative cdm ct (Sinh ex) = takePartialDerivative cdm ct ex `Times` Cosh ex
takePartialDerivative cdm ct (Cosh ex) = RealConstant 0 `Minus` (takePartialDerivative cdm ct ex `Times` Sinh ex)
takePartialDerivative cdm ct (Tanh ex) = takePartialDerivative cdm ct ex `Times`
                                        (RealConstant 1 `Divided` (Power (Cosh ex) (RealConstant 2)))
takePartialDerivative cdm ct (ASinh ex) = takePartialDerivative cdm ct ex `Divided`
                                         (Power (RealConstant 1 `Plus` (ex `Power` RealConstant 2)) (RealConstant 0.5))
takePartialDerivative cdm ct (ACosh ex) = takePartialDerivative cdm ct ex `Divided`
                                          (Power ((ex `Power` RealConstant 2) `Minus` RealConstant 1)
                                                 (RealConstant 0.5))
takePartialDerivative cdm ct (ATanh ex) = takePartialDerivative cdm ct ex `Divided`
                                           (RealConstant 1 `Minus` (ex `Power` RealConstant 2))
takePartialDerivative cdm ct (RealExpressionTag s ex) = RealExpressionTag s (takePartialDerivative cdm ct ex)

unaryEvaluateOrPassthrough c f rex =
  case (simplifyRealExpression rex)
  of
    RealConstant rc -> RealConstant (f rc)
    rex' -> c rex'

simplifyRealExpression ex@(RealConstant _) = ex
simplifyRealExpression ex@(RealVariableE _) = ex
simplifyRealExpression ex@BoundVariableE = ex
simplifyRealExpression (Derivative ex) = Derivative (simplifyRealExpression ex)
simplifyRealExpression (RealCommonSubexpressionE (RealCommonSubexpression id ex)) = 
  case simplifyRealExpression ex
  of
    rc@(RealConstant _) -> rc
    rce@(RealCommonSubexpressionE (RealCommonSubexpression _ _)) -> rce
    rv@(RealVariableE _) -> rv
    ex' -> RealCommonSubexpressionE (RealCommonSubexpression id ex')
simplifyRealExpression (If bex rex1 rex2) =
  let
    bex' = simplifyBoolExpression bex
    rex1' = simplifyRealExpression rex1
    rex2' = simplifyRealExpression rex2
  in
   case bex'
   of
     BoolConstant True -> rex1'
     BoolConstant False -> rex2'
     _ | rex1' == rex2' -> rex1'
     otherwise -> If bex' rex1' rex2'
     
simplifyRealExpression (Plus rex1 rex2) =
  case (simplifyRealExpression rex1, simplifyRealExpression rex2)
  of
    (RealConstant 0, rex') -> rex'
    (rex', RealConstant 0) -> rex'
    (RealConstant rc1, RealConstant rc2) -> RealConstant (rc1 + rc2)
    (rex1', rex2') -> Plus rex1' rex2'
            
simplifyRealExpression (Minus rex1 rex2) =
  case (simplifyRealExpression rex1, simplifyRealExpression rex2)
  of
    (rex', RealConstant 0) -> rex'
    (RealConstant rc1, RealConstant rc2) -> RealConstant (rc1 - rc2)
    (rex1', rex2') -> Minus rex1' rex2'
   
simplifyRealExpression (Times rex1 rex2) = 
  case (simplifyRealExpression rex1, simplifyRealExpression rex2)
  of
    (RealConstant 0, _) -> RealConstant 0
    (_, RealConstant 0) -> RealConstant 0
    (RealConstant 1, rex') -> rex'
    (rex', RealConstant 1) -> rex'
    (RealConstant rc1, RealConstant rc2) -> RealConstant (rc1 * rc2)
    (RealConstant (-1), rex') -> (RealConstant 0 `Minus` rex')
    (rex', RealConstant (-1)) -> (RealConstant 0 `Minus` rex')
    (rex1', rex2') -> Times rex1' rex2'
   
simplifyRealExpression (Divided rex1 rex2) =
  case (simplifyRealExpression rex1, simplifyRealExpression rex2)
  of
    (RealConstant 0, _) -> RealConstant 0
    (RealConstant rc1, RealConstant rc2) -> RealConstant (rc1 / rc2)
    (LogBase rex1 rex2, LogBase rex3 rex4) | rex1 == rex3 -> LogBase rex4 rex2
    (rex1', rex2') -> Divided rex1' rex2'
    
simplifyRealExpression (Power rex1 rex2) =
  case (simplifyRealExpression rex1, simplifyRealExpression rex2)
  of
    (RealConstant 0, _) -> RealConstant 0
    (RealConstant 1, _) -> RealConstant 1
    (RealConstant rc1, RealConstant rc2) -> RealConstant (rc1 ** rc2)
    (rex1', rex2') -> Power rex1' rex2'
    
simplifyRealExpression (Ceiling rex1) =
  case (simplifyRealExpression rex1)
  of
    (RealConstant rc) -> RealConstant (fromIntegral $ ceiling rc)
    rex1' -> Ceiling rex1'
    
simplifyRealExpression (LogBase rex1 rex2) =
  case (simplifyRealExpression rex1, simplifyRealExpression rex2)
  of
    (RealConstant rc1, RealConstant rc2) -> RealConstant $ logBase rc1 rc2
    (rex1', rex2') -> LogBase rex1' rex2'
      
simplifyRealExpression (Sin rex) = unaryEvaluateOrPassthrough Sin sin rex
simplifyRealExpression (Tan rex) = unaryEvaluateOrPassthrough Tan tan rex
simplifyRealExpression (Cos rex) = unaryEvaluateOrPassthrough Cos cos rex
simplifyRealExpression (ASin rex) = unaryEvaluateOrPassthrough ASin asin rex
simplifyRealExpression (ATan rex) = unaryEvaluateOrPassthrough ATan atan rex
simplifyRealExpression (ACos rex) = unaryEvaluateOrPassthrough ACos acos rex
simplifyRealExpression (Sinh rex) = unaryEvaluateOrPassthrough Sinh sinh rex
simplifyRealExpression (Tanh rex) = unaryEvaluateOrPassthrough Tanh tanh rex
simplifyRealExpression (Cosh rex) = unaryEvaluateOrPassthrough Cosh cosh rex
simplifyRealExpression (ASinh rex) = unaryEvaluateOrPassthrough ASinh asinh rex
simplifyRealExpression (ATanh rex) = unaryEvaluateOrPassthrough ATanh atanh rex
simplifyRealExpression (ACosh rex) = unaryEvaluateOrPassthrough ACosh acosh rex
simplifyRealExpression (RealExpressionTag s ex) = RealExpressionTag s (simplifyRealExpression ex)

simplifyBoolExpression (BoolConstant b) = BoolConstant b
simplifyBoolExpression (BoolCommonSubexpressionE (BoolCommonSubexpression id bex)) =
  BoolCommonSubexpressionE (BoolCommonSubexpression id (simplifyBoolExpression bex))
simplifyBoolExpression (And bex1 bex2) =
  case (simplifyBoolExpression bex1, simplifyBoolExpression bex2)
  of
    (BoolConstant True, bex') -> bex'
    (BoolConstant False, _) -> BoolConstant False
    (bex', BoolConstant True) -> bex'
    (_, BoolConstant False) -> BoolConstant False
    (bex1', bex2') -> And bex1' bex2'
simplifyBoolExpression (Not bex) =
  case (simplifyBoolExpression bex)
  of
    Not bex' -> bex'
    BoolConstant bc -> BoolConstant (not bc)
    bex' -> Not bex'
simplifyBoolExpression (Or bex1 bex2) =
  case (simplifyBoolExpression bex1, simplifyBoolExpression bex2)
  of
    (BoolConstant True, _) -> BoolConstant True
    (BoolConstant False, bex') -> bex'
    (_, BoolConstant True) -> BoolConstant True
    (bex', BoolConstant False) -> bex'
    (bex1', bex2') -> Or bex1' bex2'
simplifyBoolExpression (LessThan rex1 rex2) =
  case (simplifyRealExpression rex1, simplifyRealExpression rex2)
  of
    (RealConstant rc1, RealConstant rc2) -> BoolConstant (rc1 < rc2)
    (rex1', rex2') -> LessThan rex1' rex2'
simplifyBoolExpression (Equal rex1 rex2) =
  case (simplifyRealExpression rex1, simplifyRealExpression rex2)
  of
    (RealConstant rc1, RealConstant rc2) -> BoolConstant (rc1 == rc2)
    (rex1', rex2') -> Equal rex1' rex2'
