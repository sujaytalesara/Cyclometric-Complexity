module Main where

import System.Environment (getArgs)
import System.Process
import System.Directory (doesDirectoryExist)
import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure
import Control.Monad (forever, forM_)
import Data.List
import Data.List.Split
import Data.String.Utils
import Argon
import qualified Pipes.Prelude as P
import Pipes
import Pipes.Safe (runSafeT)
import System.IO.Silently


workerAnalyseComplexity :: (ProcessId, NodeId, String) -> Process ()
workerAnalyseComplexity (master, workerId, url) = do
  liftIO ( putStrLn $ "Starting worker : " ++ (show workerId) ++ " with parameter: " ++ url)
  let repoName = last $ splitOn "/" url
  gitRepoExists <- liftIO $ doesDirectoryExist ("/tmp/" ++ repoName)
  if not gitRepoExists then do
    liftIO $ callProcess "/usr/bin/git" ["clone", url, "/tmp/" ++ repoName]
  else do
    liftIO $ putStrLn "Repository already there."
  let conf = (Config 6 [] [] [] Colored)
  let source = allFiles ("/tmp/" ++ repoName)
              >-> P.mapM (liftIO . analyze conf)
              >-> P.map (filterResults conf)
              >-> P.filter filterNulls
  liftIO $ putStrLn $ "Launching analyse for " ++ url
  (output, _) <- liftIO $ capture $ runSafeT $ runEffect $ exportStream conf source
  -- liftIO $ callProcess "/bin/rm" ["-rf", "/tmp/" ++ repoName]
  liftIO ( putStrLn $ "End of worker : " ++ (show workerId) ++ " with parameter: " ++ url)
  send master $ (workerId, url, output)


remotable ['workerAnalyseComplexity]
