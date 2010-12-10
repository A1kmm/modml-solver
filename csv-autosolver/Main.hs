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
import Data.IORef
import qualified Data.Map as M
import Control.Monad

data CSVSolve = CSVSolve { modelFile :: String, {- debug :: Bool, -} intermediateC :: Bool,
                           profile :: Bool,
                           includedir :: [String],
                           startTime :: Double, maxSolverStep :: Double, maxReportStep :: Double,
                           endTime :: Double, showEveryStep :: Bool, relativeErrorTolerance :: Double,
                           absoluteErrorTolerance :: Double
                         }
                  deriving (Show, Data, Typeable)

csvSolveArgs =
    CSVSolve { modelFile = def &= argPos 0 &=
                 typ "MODELFILE",
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
                 help "The absolute error tolerance" &= typ "REAL"
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

showCSVSolver modelModule dumpIntermediate =
    showString "import " .
    showString modelModule .
    showString "\n\
               \import ModML.Solver.CSVSolver\n\
               \main = csvMain model " . shows dumpIntermediate

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

csvAutoSolver (args@CSVSolve { intermediateC = dumpc, profile = prof, modelFile = mf, includedir = dirs }) = do
  let dbg = False
  -- Work out what packages we need...
  packages <- liftM withRight $ parseFromFile extractPackages mf
  -- Get a name for the model...
  let (modelPath, modelFile) = splitFileName mf
  cwd <- getCurrentDirectory
  let modelPath' = fromMaybe (error "Invalid relative model path") $ absNormPath cwd modelPath
  let modelModule = dropExtension modelFile
  let usePackages = ("base":"ModML-Core":"ModML-Solver":packages)
  tmpdir <- getTemporaryDirectory
  brackettmpdir (tmpdir </> "solveXXXXXX") $ \dirname -> do
      let includePaths = (dirname:modelPath':dirs)
      let solveFile = dirname </> "SolveAsCSV.hs"
      withFile solveFile WriteMode $ \hSolveFile ->
        -- Write the model...
        hPutStr hSolveFile (showCSVSolver modelModule dumpc "")
      -- Compile it...
      case dbg
        of
          False ->  do
            let profRtsOpts = if prof then ["+RTS", "-p"] else []
            rawSystem "ghc" $ ("--make":"-hide-all-packages":((map ("-i"++) includePaths) ++
                                                             (beforeEach "-package" usePackages) ++
                                                             [solveFile]))
            when prof $ do
              rawSystem "ghc" $ ("--make":"-hide-all-packages":
                                 "-prof":"-auto-all":"-caf-all":"-rtsopts":
                                 "-osuf":"p_o":((map ("-i"++) includePaths) ++
                                           (beforeEach "-package" usePackages) ++
                                           [solveFile]))
              return ()
            let binary = tmpdir </> (dropExtension solveFile)
            rawSystem binary $ ["1", show . startTime $ args, show . maxSolverStep $ args,
                                show .  maxReportStep $ args, show . endTime $ args,
                                show . showEveryStep $ args, show . relativeErrorTolerance $ args,
                                show . absoluteErrorTolerance $ args] ++ profRtsOpts
            return ()
          True -> undefined
