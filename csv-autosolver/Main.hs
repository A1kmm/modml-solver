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
import GHC
import FastString
import qualified GHC.Paths as GHCP
import qualified Outputable as O
import qualified DynFlags as GHCDF
import qualified Data.Dequeue as DQ
import MonadUtils
import qualified Data.Map as M

data CSVSolve = CSVSolve { modelFile :: String, debug :: Bool, includedir :: [String]
                         }
                  deriving (Show, Data, Typeable)

csvSolveArgs =
    CSVSolve { modelFile = def &= argPos 0 &=
                 typ "MODELFILE",
               debug = def &=
                 help "Steps through your model to help track down exceptions",
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
      string "-- "
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

whileM :: Monad m => m Bool -> m ()
whileM m = do
  nxt <- m
  case nxt
    of
      False -> return ()
      True -> whileM m

debugAnException :: MonadIO m => IORef (M.Map FastString (DQ.BankersDequeue SrcSpan)) -> SomeException -> m ()
debugAnException logInfoRef ex =
  liftIO $ do
    putStrLn "There was an uncaught exception running the model:"
    putStrLn "Breakdown of last steps in each module:"
    logInfo <- readIORef logInfoRef
    forM_ (M.toList logInfo) $ \(mod, dqSteps) -> do
        putStrLn $ showString "=== " . shows mod $ " ==="
        forM_ (DQ.takeFront histKeep dqSteps) $ \histspan ->
            print $ O.ppr histspan O.defaultUserStyle
    putStrLn "Exception follows:"
    throw ex

histKeep = 30

allCombined = (mkFastString "All Combined")
logBreakpoint :: GhcMonad m => IORef (M.Map FastString (DQ.BankersDequeue SrcSpan)) -> m ()
logBreakpoint logInfoRef = do
  span <- liftM resumeSpan $ liftM head getResumeContext
  logOneName logInfoRef span $! srcSpanFile span
  logOneName logInfoRef span allCombined
  return ()

logOneName logInfoRef name mod =
    liftIO $ modifyIORef logInfoRef $ (flip M.alter mod) $ \oldhist ->
        Just $
          case oldhist
            of
              Nothing -> DQ.fromList [name]
              Just oldhist' ->
                  let
                      lastHist = fromMaybe undefined (DQ.last oldhist')
                      withNew = DQ.pushBack oldhist' name
                  in
                    if lastHist == name
                    then
                        oldhist'
                    else
                        if DQ.length oldhist' >= histKeep
                        then
                            snd . DQ.popFront $ withNew
                        else
                            withNew

csvAutoSolver (CSVSolve { debug = dbg, modelFile = mf, includedir = dirs }) = do
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
        hPutStr hSolveFile (showCSVSolver modelModule "")
      -- Compile it...
      case dbg
        of
          False ->  do
            rawSystem "ghc" $ "--make":"-hide-all-packages":((map ("-i"++) includePaths) ++
                                                             (beforeEach "-package" usePackages) ++
                                                             [solveFile])
            let binary = tmpdir </> (dropExtension solveFile)
            rawSystem binary []
            return ()
          True ->
              do
                runGhc (Just GHCP.libdir) $ do
                  dflags <- getSessionDynFlags
                  setSessionDynFlags (dflags {ghcMode = CompManager,
                                              ghcLink = LinkInMemory, hscTarget = HscInterpreted,
                                              includePaths = includePaths,
                                              packageFlags = GHCDF.packageFlags dflags ++ (map GHCDF.ExposePackage usePackages),
                                              flags = GHCDF.Opt_HideAllPackages:(GHCDF.flags dflags)})
                  target <- guessTarget solveFile Nothing
                  setTargets [target]
                  load LoadAllTargets
                  mainMod <- findModule (mkModuleName "Main") Nothing
                  setContext [mainMod] []
                  loggedInfoRef <- liftIO $ newIORef M.empty
                  runStmt "main" SingleStep
                  whileM $ do
                    res <- resume (const True) SingleStep
                    case res
                      of
                        RunOk _ -> return False
                        RunFailed -> (liftIO $ putStrLn "Compilation failed - see above") >> (return False)
                        RunException e -> (debugAnException loggedInfoRef e) >> (return False)
                        RunBreak _ _ _ -> (logBreakpoint loggedInfoRef) >> (return True)
                return ()
