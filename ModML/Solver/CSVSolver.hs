module ModML.Solver.CSVSolver (csvMain)
where

import qualified ModML.Solver.BasicDAESolver as S

-- TODO - Read solver params from command line, passed from autosolver...
solverParams = return S.defaultSolverParameters

displayResultsAsCSV (varMap, rows) =
    do
      putStrLn $ titleRow varMap
      displayRemainingRows rows

displayRemainingRows [] = return ()
displayRemainingRows (r:rest) = do
  putStrLn $ oneRow r
  displayRemainingRows rest

titleRow m =
    let
        M.toList m
displayOneRow r = undefined -- TODO

csvMain model = do
  params <- solverParams
  res <- S.modelToResults m params 
  case res
    of
      Left err -> print err
      Right res -> displayResultsAsCSV res