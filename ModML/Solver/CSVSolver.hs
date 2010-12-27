module ModML.Solver.CSVSolver (csvMain, AnalysisType(..), SensitivityVariable(..))
where

import qualified ModML.Solver.BasicDAESolver as S
import qualified ModML.Core.BasicDAEModel as B
import Data.List
import Data.Ord
import qualified Data.Map as M
import System.Environment
import Control.Monad
import ModML.Solver.ModelTransformations

data SensitivityVariable = SensitivityVariable { sensitivityVariableName :: String,
                                                 sensitivityRange :: (Double, Double),
                                                 sensitivityStep :: Double
                                               } deriving (Eq, Ord, Show)
data AnalysisType = UniformTimeCourse | SensitivityAnalysis { sensistivityVariables :: [SensitivityVariable] }
                    deriving (Eq, Ord, Show)

displayUniformTimeCourseResultsAsCSV model (varMap, rows) =
    case (M.null varMap)
      of
        True -> putStrLn "No variables in model"
        False ->
            do
              putStrLn $ titleRow ("Time":) model varMap
              displayRemainingRows rows

displaySensitivityAnalysisResultsAsCSV sa model (varMap, rows) =
    case (M.null varMap)
      of
        True -> putStrLn "No variables in model"
        False ->
          do
            putStrLn $ titleRow id model varMap
            displayRemainingLastRows rows
            
displayRemainingRows [] = return ()
displayRemainingRows (r:rest) = do
  putStrLn $ displayOneRow r
  displayRemainingRows rest

displayRemainingLastRows [] = return ()
displayRemainingLastRows (S.Result (_, v, dv):S.Success:rest) = do
  putStrLn $ displayOneSensitivityRow v dv
  displayRemainingLastRows rest
displayRemainingLastRows ((r@(S.Result _)):rest) = displayRemainingLastRows rest
displayRemainingLastRows (r:rest) = do
  putStrLn $ displayOneRow r
  displayRemainingRows rest

withFst :: (a -> b) -> (a, c) -> (b, c)
withFst f (a, c) = (f a, c)

titleRow prefix model m =
    let
        byIds = sortBy (comparing snd) $ M.toList m
        nameByIds = map (withFst (nameVariable model)) byIds
        maxId = snd $ last byIds
        titleStrs = reverse $ orderedNumberedListToList "Unknown" nameByIds 0 maxId []
    in
      -- XXX "Time" should come from annotations - might not always be time.
      intercalate "," $ prefix (titleStrs ++ (map ("Rate of "++) titleStrs))

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

displayOneSensitivityRow v dv =
  intercalate "," $ ((map show v) ++ (map show dv))

buildSensitivityParams model params sa =
  foldl' (buildOneSensitivityParam model) [params { S.showEveryStep = 0.0, S.maxReportStep = S.tEnd params }] sa

buildOneSensitivityParam model paramList (SensitivityVariable { sensitivityVariableName = svn,
                                                                sensitivityRange = (sensLow, sensHigh), 
                                                                sensitivityStep = sensStep }) =
  case getVariableByName svn model
  of
    Nothing -> paramList
    Just v ->
      concatMap (\p -> [p { S.variableOverrides = (v, val):(S.variableOverrides p)} |
                        val <- [sensLow,(sensLow + sensStep)..sensHigh]]) paramList      

doRequest False model paramList resHandler =
  case S.modelToResults model paramList
  of
    Left err -> print err
    Right res -> resHandler model res
    
doRequest True model paramList _ =
  case S.makeCodeFor model paramList
  of
    Left err -> print err
    Right (_, _, code) -> putStrLn code

handleRequest dumpInt model (params, UniformTimeCourse) =
  doRequest dumpInt model [params] displayUniformTimeCourseResultsAsCSV
    
handleRequest dumpInt model (params, SensitivityAnalysis sa) = 
  doRequest dumpInt model (buildSensitivityParams model params sa) (displaySensitivityAnalysisResultsAsCSV sa)
    
csvMain model dumpIntermediate transformations request = -- trace (showString "Original # of variables = " . shows (length . B.variables $ model) . showString ", # of eqns = "  . shows (length . B.equations $ model) . showString ", # of boundary eqns = " . shows (length . B.boundaryEquations $ model) $ "") $
  let
    model' = applyTransformations model transformations
  in
   -- trace (showString "New # of variables = " . shows (length . B.variables $ model') . showString ", # of eqns = "  . shows (length . B.equations $ model') . showString ", # of boundary eqns = " . shows (length . B.boundaryEquations $ model') $ "") $
     handleRequest dumpIntermediate model' request
