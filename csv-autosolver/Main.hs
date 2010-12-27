{-# LANGUAGE DeriveDataTypeable #-}

import Data.Typeable
import Data.Data
import System.Console.CmdArgs
import Text.Parsec
import Text.Parsec.String
import System.FilePath
import System.Path
import System.Process
import Control.Monad
import Control.Exception hiding (try)
import Data.Maybe
import System.Directory
import System.IO
import System.Environment
import Data.List
import qualified Data.Map as M
import Control.Monad
import ModML.Solver.ModelTransformations
import qualified ModML.Core.BasicDAEModel as B
import qualified ModML.Solver.CSVSolver as S
import qualified ModML.Solver.BasicDAESolver as S

data CSVSolve = CSVSolve { modelFile :: String, csvOutFile :: Maybe String,
                           intermediateC :: Bool,
                           profile :: Bool,
                           includedir :: [String],
                           startTime :: Double, maxSolverStep :: Double, maxReportStep :: Double,
                           endTime :: Double, showEveryStep :: Bool, relativeErrorTolerance :: Double,
                           absoluteErrorTolerance :: Double, removeConstraint :: [String],
                           addConstraint :: [(String, Double)],
                           sensitivityVariable :: [String], sensitivityLowerBound :: [Double],
                           sensitivityUpperBound :: [Double], sensitivityStep :: [Double]
                         }
                  deriving (Show, Data, Typeable)

csvSolveArgs =
    CSVSolve { modelFile = def &= argPos 0 &=
                 typ "MODELFILE",
               csvOutFile = Nothing &=
                 typ "FILENAME",
               intermediateC = def &=
                 help "Shows the intermediate C code used to generate results",
               profile = def &=
                 help "Sets up the generated program for profiling",
               includedir = def &=
                 help "Directories to search for includes" &=
                 explicit &= name "I" &=
                 typDir,
               startTime = 0 &=
                 help "The starting 'time' point" &= typ "REAL",
               maxSolverStep = 1 &=
                 help "The maximum solver step to take" &= typ "REAL",
               maxReportStep = 0.1 &=
                 help "The maximum 'time' between reports" &= typ "REAL",
               endTime = 10 &=
                 help "The ending 'time' point" &= typ "REAL",
               showEveryStep = False &=
                 help "Causes every internal step to be returned",
               relativeErrorTolerance = 1E-6 &=
                 help "The relative error tolerance" &= typ "REAL",
               absoluteErrorTolerance = 1E-6 &=
                 help "The absolute error tolerance" &= typ "REAL",
               removeConstraint = def &=
                 help "Constraints in the form of variable=value to remove (given as annotated variable name)",
               addConstraint = def &=
                 help "Constraints to add, as annotated variable name and real number value",
               sensitivityVariable = def &=
                 help "Annotated variable name to do sensitivity analysis on",
               sensitivityLowerBound = def &=
                 help "Lower bound for the sensitivity analysis",
               sensitivityUpperBound = def &=
                 help "Upper bound for the sensitivity analysis",
               sensitivityStep = def &=
                 help "Step size for the sensitivity analysis"
             }

main = cmdArgs csvSolveArgs >>= csvAutoSolver

withRight (Left e) = error (shows e "Unexpected Left: ")
withRight (Right x) = x

extractPackages = do
  many whitespace
  optional languageOptions
  liftM catMaybes (many (try commentMaybePackage))

whitespaceNonbreaking = oneOf " \t"
whitespaceBreaking = oneOf "\r\n"
whitespace = whitespaceNonbreaking <|> whitespaceBreaking

languageOptions = do
  string "{-"
  manyTill anyChar (try $ string "-}")

commentMaybePackage =
    do
      many whitespace
      string "-- "
      (try $ do
         char '+'
         many whitespaceNonbreaking
         string "Require"
         many whitespaceNonbreaking
         liftM Just $ liftM2 (:) alphaNum $ many (alphaNum <|> char '_' <|> char '-')
       ) <|> (manyTill anyChar whitespaceBreaking >> return Nothing)

showCSVSolver modelModule (CSVSolve { intermediateC = intermediateC, csvOutFile = csvOutFile,
                              startTime = startTime, maxSolverStep = maxSolverStep,
                              maxReportStep = maxReportStep, endTime = endTime,
                              showEveryStep = showEveryStep, relativeErrorTolerance = relativeErrorTolerance,
                              absoluteErrorTolerance = absoluteErrorTolerance, removeConstraint = removeConstraint,
                              addConstraint = addConstraint, sensitivityVariable = sensitivityVariable,
                              sensitivityLowerBound = sensitivityLowerBound, sensitivityUpperBound = sensitivityUpperBound,
                              sensitivityStep = sensitivityStep }) =
  let
    transformations = ((map DropConstraint removeConstraint) ++ (map (\(n, v) -> FixVariable n v) addConstraint))
    sensitivitySettings = zip4 sensitivityVariable sensitivityLowerBound sensitivityUpperBound sensitivityStep
    analysis = if null sensitivitySettings
               then
                 S.UniformTimeCourse
               else
                 S.SensitivityAnalysis (map (\(v, lb, ub, s) -> S.SensitivityVariable v (lb, ub) s) sensitivitySettings)
    params :: S.SolverParameters B.RealVariable
    params = S.defaultSolverParameters {
               S.tStart = startTime, S.maxSolverStep = maxSolverStep,
               S.maxReportStep = maxReportStep, S.tEnd = endTime,
               S.showEveryStep = if showEveryStep then 1.0 else 0.0,
               S.reltol = relativeErrorTolerance, S.abstol = absoluteErrorTolerance }
  in
    showString "import " .
    showString modelModule .
    showString "\n\
               \import ModML.Solver.CSVSolver\n\
               \import ModML.Solver.ModelTransformations\n\
               \import ModML.Solver.BasicDAESolver\n\
               \main = csvMain model " . shows intermediateC . showString " (" .
    shows transformations .
    showString ") (" .
    shows (params, analysis) .
    showString ") (" . shows csvOutFile . showString ")\n"

intermix :: [a] -> [a] -> [a]
intermix [] _ = []
intermix _ [] = []
intermix (a1:l1) (a2:l2) = a1:a2:(intermix l1 l2)

beforeEach :: a -> [a] -> [a]
beforeEach p l = intermix (repeat p) l 

whileM :: Monad m => m Bool -> m ()
whileM m = do
  nxt <- m
  case nxt
    of
      False -> return ()
      True -> whileM m

csvAutoSolver (args@CSVSolve { modelFile = modelFile,
                               profile = profile, includedir = includedir }) = do
  -- Work out what packages we need...
  packages <- liftM withRight $ parseFromFile extractPackages modelFile
  -- Get a name for the model...
  let (modelPath, modelFilePart) = splitFileName modelFile
  cwd <- getCurrentDirectory
  let modelPath' = fromMaybe (error "Invalid relative model path") $ absNormPath cwd modelPath
  let modelModule = dropExtension modelFilePart
  let usePackages = ("base":"ModML-Core":"ModML-Solver":packages)
  tmpdir <- getTemporaryDirectory
  brackettmpdir (tmpdir </> "solveXXXXXX") $ \dirname -> do
      let includePaths = (dirname:modelPath':includedir)
      let solveFile = dirname </> "SolveAsCSV.hs"
      withFile solveFile WriteMode $ \hSolveFile ->
        -- Write the model...
        hPutStr hSolveFile (showCSVSolver modelModule args "")
      -- Compile it...
      let profRtsOpts = if profile then ["+RTS", "-p", "-xc"] else []
      if profile
        then do
          rawSystem "ghc" ("--make":"-hide-all-packages":"-auto-all":"-caf-all":(
                              (map ("-i"++) includePaths) ++
                              (beforeEach "-package" usePackages) ++
                              [solveFile]))
          rawSystem "ghc" $ ("--make":"-hide-all-packages":
                             "-prof":"-auto-all":"-caf-all":"-rtsopts":
                             "-osuf":"p_o":((map ("-i"++) includePaths) ++
                                            (beforeEach "-package" usePackages) ++
                                            [solveFile]))
        else
          rawSystem "ghc" $ ("--make":"-hide-all-packages":
                               ((map ("-i"++) includePaths) ++
                                (beforeEach "-package" usePackages) ++
                                [solveFile]))

      let binary = tmpdir </> (dropExtension solveFile)
      rawSystem binary profRtsOpts
      return ()
