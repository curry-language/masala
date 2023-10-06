--------------------------------------------------------------------------
--- This module implements the management to execute user processes.
--------------------------------------------------------------------------

module System.Processes
  ( processNames
  , isInProcess, startProcess, removeCurrentProcess, advanceInProcess
  , nextControllerRefInProcessOrForUrl
  ) where

import Control.AllValues    ( getOneValue )
import HTML.Base
import HTML.Session

import Config.RoutesData    ( ControllerReference )
import Config.UserProcesses ( ControllerResult, Processes(..), availableProcesses )
import System.Routes        ( getControllerReference )

--------------------------------------------------------------------------
-- A operations on the datatype for process systems.

processesOf :: Processes a -> [(String, a)]
processesOf  (ProcSpec procs _ _) = procs

--- The names of the processes in a process system.
processNames :: Processes _ -> [String]
processNames (ProcSpec procs _ _) = map fst procs

getControllerForState :: stid -> Processes stid -> ControllerReference
getControllerForState sid (ProcSpec _ ctrlof _) = ctrlof sid

--- Is a state a final state, i.e., without successors, in a process system?
isFinalState :: a -> Processes a -> IO Bool
isFinalState sid (ProcSpec _ _ trans) = do
  succs <- getOneValue (trans sid _)
  return (maybe True (const False) succs)

--------------------------------------------------------------------------
-- The current processes are stored in a persistent entity.
currentProcess :: SessionStore String
currentProcess = sessionStore "currentProcess"

--- Returns the process state stored in the user session.
getCurrentProcess :: Read a => IO (Maybe a)
getCurrentProcess = do
  curProc <- fromFormReader $ getSessionMaybeData currentProcess
  case curProc of
    Just sids -> return $ Just (read sids)
    Nothing -> return Nothing

--- Is the current user session in a process interaction?
isInProcess :: IO Bool
isInProcess = fromFormReader $
  getSessionMaybeData currentProcess >>= return . maybe False (const True)

--- Saves the state of a process, i.e., a node in the process graph,
--- in the user session.
saveCurrentProcess :: Show a => a -> IO ()
saveCurrentProcess sid = putSessionData currentProcess (show sid)

--- Deletes the process in the user session.
removeCurrentProcess :: IO ()
removeCurrentProcess = removeSessionData currentProcess

--- Starts a new process with a given name. In the next step, the
--- controller of the start state of the process is executed.
startProcess :: String -> IO [BaseHtml]
startProcess pname =
  maybe (return [htxt $ "startProcess: process not found: " ++ pname])
        (\state -> do saveCurrentProcess state
                      return [htxt ""] -- triggers redirect
        )
        (lookup pname (processesOf availableProcesses))

--- Advance by one state in the process description.
--- The parameter contains some optional data from the controllers
--- in order to influence the next transition.
advanceInProcess :: Maybe ControllerResult -> IO ()
advanceInProcess ctrlinfo = do
  curprocess <- getCurrentProcess
  case curprocess of
    Nothing  -> return () -- no active process, do nothing
    Just sid -> do
     let (ProcSpec _ _ trans) = availableProcesses
     nextsid <- getOneValue (trans sid ctrlinfo)
     case nextsid of
        Just sid' -> saveCurrentProcess sid'
        Nothing   -> return () -- this case should not occur in a good spec.

--- Returns the next controller in the current process or,
--- if there is no current process, the controller associated to the given URL.
nextControllerRefInProcessOrForUrl :: String
                                   -> IO (Maybe ControllerReference)
nextControllerRefInProcessOrForUrl url = do
  curprocess <- getCurrentProcess
  case curprocess of
    Nothing  -> getControllerReference url -- no current process
    Just sid -> do isfinal <- isFinalState sid availableProcesses
                   if isfinal then removeCurrentProcess
                              else return ()
                   return (Just (getControllerForState sid availableProcesses))
