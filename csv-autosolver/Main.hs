{-# LANGUAGE DeriveDataTypeable #-}

import Data.Typeable
import Data.Data
import System.Console.CmdArgs
import Text.Parsec
import Text.Parsec.String
import System.FilePath
import System.Process
import Control.Monad
import Control.Exception hiding (try)
import Data.Maybe
import System.Directory
import System.IO

data CSVSolve = CSVSolve { modelFile :: String, includedir :: [String]
                         }
                  deriving (Show, Data, Typeable)

csvSolveArgs =
    CSVSolve { modelFile = def &= argPos 0 &=
                 typ "MODELFILE",
               includedir = def &=
                 help "Directories to search for includes" &=
                 explicit &= name "I" &=
                 typDir
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
      try $ string "--"
      (try $ do
         char '+'
         many whitespaceNonbreaking
         string "Require"
         many whitespaceNonbreaking
         liftM Just $ liftM2 (:) alphaNum $ many (alphaNum <|> char '_' <|> char '-')
       ) <|> (manyTill anyChar whitespaceBreaking >> return Nothing)

showCSVSolver modelModule =
    showString "import " .
    showString modelModule .
    showString "\n\
               \import ModML.Solver.CSVSolver\n\
               \main = csvMain model"

intermix :: [a] -> [a] -> [a]
intermix [] _ = []
intermix _ [] = []
intermix (a1:l1) (a2:l2) = a1:a2:(intermix l1 l2)

beforeEach :: a -> [a] -> [a]
beforeEach p l = intermix (repeat p) l 

csvAutoSolver (CSVSolve { modelFile = mf, includedir = dirs }) = do
  -- Work out what packages we need...
  packages <- liftM withRight $ parseFromFile extractPackages mf
  -- Get a name for the model...
  let (modelPath, modelFile) = splitFileName mf
  let modelModule = dropExtension modelFile
  tmpdir <- getTemporaryDirectory
  (solveFile, hSolveFile) <- openTempFile tmpdir "SolveAsCSV.hs"
  flip finally (removeFile solveFile) $ do
    -- Write the model...
    flip finally (hClose hSolveFile) $
         hPutStr hSolveFile (showCSVSolver modelModule "")
    -- Compile it...
    rawSystem "ghc" $ "--make":"-hide-all-packages":((map ("-i"++) (modelPath:dirs)) ++
                                                   (beforeEach "-package" ("base":"ModML-Solver":packages)) ++
                                                   [solveFile])
    let binary = tmpdir </> (dropExtension solveFile)
    rawSystem binary []
