--------------------------------------------------------------------------
--- This module defines operations which provide a connection to
--- CPM's data.
--------------------------------------------------------------------------

module System.CPM
 where

import System.Directory
import System.FilePath
import System.IOExts      ( readCompleteFile )
import Text.CSV           ( readCSV )

import Config.Masala
import Model.Masala2

------------------------------------------------------------------------------
--- Checks whether the package has been tested by CPM.
--- If yes, return an appropriate string, otherwise return `Nothing`.
getVersionTestTime :: Package -> Version -> IO (Maybe String)
getVersionTestTime pkg vers = do
  let pkgid = packageName pkg ++ "-" ++ versionVersion vers
  cpmbasedir <- getCPMBaseDir
  let testfile = cpmbasedir </> "TEST" </> pkgid ++ ".csv"
  hastests <- doesFileExist testfile
  if hastests
    then do
      tftime <- getModificationTime testfile
      if tftime > versionUploadDate vers
        then do
          tinfos <- readCompleteFile testfile >>= return . readCSV
          case tinfos of
            [_, (_:ct:rc:_)] | rc == "0" -> return $ Just $
                                              "Succesfully tested at " ++ ct
            _                           -> return Nothing
        else return Nothing
    else return Nothing

------------------------------------------------------------------------------
