module ModML.Solver.CSVSolver (csvMain)
where

import qualified ModML.Solver.BasicDAESolver as S
import qualified ModML.Core.BasicDAEModel as B
import Data.List
import Data.Ord
import qualified Data.Map as M
import System.Environment
import Control.Monad
import System.Directory

solverParamsArgsOrDefault (ver:tStartStr:maxSolverStepStr:maxReportStepStr:tEndStr:showEveryStepStr:reltolStr:abstolStr:_) =
    if ver /= "1"
    then
        error $ "Your installed ModML-Solver package is not compatible with your autosolver - try cabal install ModML-Solver"
    else
        S.SolverParameters { S.tStart = read tStartStr,
                             S.maxSolverStep = read maxSolverStepStr,
                             S.maxReportStep = read maxReportStepStr,
                             S.tEnd = read tEndStr,
                             S.showEveryStep = if (read showEveryStepStr) then 1.0 else 0.0,
                             S.reltol = read reltolStr,
                             S.abstol = read abstolStr
                           }
solverParamsArgsOrDefault _ = S.defaultSolverParameters

solverParams = liftM solverParamsArgsOrDefault getArgs

displayResultsAsCSV model (varMap, rows) =
    do
      putStrLn $ titleRow model varMap
      displayRemainingRows rows

displayRemainingRows [] = return ()
displayRemainingRows (r:rest) = do
  putStrLn $ displayOneRow r
  displayRemainingRows rest

withFst :: (a -> b) -> (a, c) -> (b, c)
withFst f (a, c) = (f a, c)

titleRow model m =
    let
        byIds = sortBy (comparing snd) $ M.toList m
        nameByIds = map (withFst (nameVariable model)) byIds
        maxId = snd $ last byIds
        titleStrs = reverse $ orderedNumberedListToList "Unknown" nameByIds 0 maxId []
    in
      -- XXX "Time" should come from annotations - might not always be time.
      intercalate "," $ "Time":(titleStrs ++ (map ("Rate of "++) titleStrs))

orderedNumberedListToList dflt onl curId maxId l
    | curId > maxId = l
    | otherwise = orderedNumberedListToList' dflt onl curId maxId l

orderedNumberedListToList' dflt [] curId maxId l =
    orderedNumberedListToList dflt [] (curId + 1) maxId (dflt:l)
orderedNumberedListToList' dflt (onl@((a, id):onl')) curId maxId l
    | curId == id = orderedNumberedListToList dflt onl' (curId + 1) maxId (a:l)
    | otherwise   = orderedNumberedListToList dflt onl (curId + 1) maxId (dflt:l)

nameVariable model (v@(B.RealVariable id)) =
    case M.lookup (show v, "\"nameIs\"") (B.annotations model)
    of
      Just n -> (read n) :: String
      Nothing -> (showString "Unnamed Variable #" . shows id) ""


displayOneRow r =
    case r
    of
      S.CheckedConditionFail msg -> showString "Checked condition failed: " msg
      S.Warning (code, mod, func, msg) -> showString "Warning #" . shows code . showString msg .
                                          showString " in function " . showString func .
                                          showString " in module " $ mod
      S.FatalError (code, mod, func, msg) -> showString "Fatal Error #" . shows code . showString ": " .
                                             showString msg .
                                             showString " in function " . showString func .
                                             showString " in module " $ mod
      S.Success -> ""
      S.Result (t, lr, lr') -> intercalate "," $ (show t):((map show lr) ++ (map show lr'))

csvMain model = do
  params <- solverParams
  let res = S.modelToResults model params
  case res
    of
      Left err -> print err
      Right res -> displayResultsAsCSV model res
