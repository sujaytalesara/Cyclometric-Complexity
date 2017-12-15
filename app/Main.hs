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


workerAys_Complex :: (procID, nodID, String) -> Process ()

workerAys_Complex (master, work_ID, Url) = do
  lifttIO ( putStrLn $ "worker start : " ++ (show work_ID) ++ " -- parameters : --" ++ Url)
  let RpName = last $ splitOn "/" Url
  gitRepoEx <- lifttIO $ doesDirectoryExist ("/tmp/" ++ RpName)
  if not gitRepoEx then do
    lifttIO $ callProcess "/usr/bin/git" ["clone", Url, "/tmp/" ++ RpName]
  else do
    lifttIO $ putStrLn "Already a Repository present." -- tell repo exits
  let confg = (Config 6 [] [] [] Colored)
  let source = allFiles ("/tmp/" ++ RpName)
              >-> P.mapM (lifttIO . analyze confg)
              >-> P.map (filterResults confg)
              >-> P.filter filterNulls
  lifttIO $ putStrLn $ "Launching analyses " ++ Url
  (outp, _) <- lifttIO $ capture $ runSafeT $ runEffect $ exportStream confg source
  -- lifttIO $ callProcess "/bin/rm" ["-rf", "/tmp/" ++ repoName]
  lifttIO ( putStrLn $ "End of worker : " ++ (show work_ID) ++ " with parameter: " ++ Url)
  send master $ (work_ID, Url, outp)


remotable ['workerAys_Complex]

myRemotTabl :: RemoteTable
myRemotTabl = Main.__remoteTable initRemoteTable

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["master", host, port] -> do
      iback <- initializeBackend host port myRemotTabl
      startMaster iback (master iback)
    ["slave", host, port] -> do
      iback <- initializeBackend host port myRemotTabl
      startSlave iback

master :: Backend -> [nodID] -> Process ()
master iback slaves = do
  lifttIO . putStrLn $ "Slaves: " ++ show slaves
  let repos = ["https://github.com/qnnguyen/HaskellAtHome","https://github.com/wanermiranda/haskell", "https://github.com/stevenchen3/learning-haskell","https://github.com/tmcgilchrist/transformers-either", "https://github.com/wangbj/haskell", "https://github.com/scharris/hmq", "https://github.com/snepo/depot", "https://github.com/treeowl/hstats"]
  resp <- fSlaveGetResp repos slaves [] []
  lifttIO $ mapM (\(r,u) -> putStrLn $ "\n\n\n\n*************************\n" ++ u ++ " :\n***************************************\n\n" ++  r) resp
  return ()
  -- It will terminate all slaves


fSlaveGetResp :: [String] -> [nodID] -> [nodID] -> [(String,String)] -> Process [(String,String)]
fSlaveGetResp [] freeSlaves [] resp = return resp
fSlaveGetResp repos freeSlaves busySlaves resp = do
  (restRepos, newBySlaves, newFreeSlaves) <- feedSlaves repos freeSlaves []
  n <- expectTimeout 60000000 -- 1min max for each repo
  case n of
    Nothing            -> die "failure Master --> exiting."
    Just (slave, Url, resp) -> fSlaveGetResp restRepos (slave:newFreeSlaves) (delete slave (newBySlaves ++ busySlaves)) ((resp,url):resp)


fSlave :: [String] -> [nodID] -> [nodID] -> Process ([String], [nodID], [nodID])
fSlave [] slaves newBySlave = return ([], newBySlaves, slaves)
fSlave repos [] newBySlaves = return (repos, newBySlaves, [])
fSlave (repo:repos) (oneSlave:slaves) newBySlaves = do
  masterPid <- getSelfPid
  _ <- spawn oneSlave $ $(mkClosure 'workerAys_Complex) (masterPid, oneSlave, repo :: String)
  fSlave repos slaves (oneSlave:newBySlaves)

