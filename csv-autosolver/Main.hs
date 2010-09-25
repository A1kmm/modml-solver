{-# LANGUAGE DerivateDataTypeable #-}
import Data.Typeable
import Data.Data
import System.Console.CmdArgs
import Text.Parsec.Char
import Text.Parsec.String
import System.FilePath
import System.Process

data CSVSolve = CSVSolve { modelFile :: String, includedir :: [String]
                         }
                  deriving (Show, Data, Typeable)

csvSolveArgs =
    CSVSolve { modelFile = def &=
                 help "The .hs toplevel model file" &=
                 typFile &= argPos 0,
               includedir = def &=
                 help "Directories to search for includes" &=
                 explicit &= name "I" &=
                 typDir
             }

main = cmdArgs csvSolveArgs >>= csvAutoSolver

withRight (Left e) = exception (shows e "Unexpected Left: ")
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

commentMaybePackage = do
  many whitespace
  char '#'
  (
   optional $ do
     char '+'
     many whitespaceNonbreaking
     string "Require"
     many whitespaceNonbreaking
     liftM2 (:) alphaNum $ many (alphaNum <|> char '_' <|> char '-')
  ) <|>
  (
   manyTill anyChar whitespaceBreaking
  )

showCSVSolver modelModule = showString $
  "import " . showString modelModule . showString "\n\
  \import ModML.Solver.CSVSolver\
  \main = csvMain model"

csvAutoSolver (CSVSolve { modelFile = mf, includedir = dirs }) = do
  -- Work out what packages we need...
  packages <- liftM withRight $ parseFromFile extractPackages mf
  -- Get a name for the model...
  let (modelPath, modelFile) = splitFileName mf
  let modelModule = dropExtension modelFile
  tmpdir <- getTemporaryDirectory
  (solveFile, hSolveFile) openTempFile tmpdir "SolveAsCSV.hs"
  -- Write the model...
  flip finally (hClose h) $
    hPutStr hSolveFile (showCSVSolver modelModule "")
  -- Compile it...
  rawSystem "ghc" $ "--make":"-hide-all-packages":((map ("-i"++) (modelPath:dirs)) ++ (map ()))

