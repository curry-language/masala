--------------------------------------------------------------------------
--- This module defines operations to support the uploading of packages.
--------------------------------------------------------------------------

module System.PackageHelpers
 where

import System.Directory
import System.FilePath
import System.IOExts      ( evalCmd, readCompleteFile )
import System.Process     ( getPID, system )
import Text.CSV           ( readCSV )

import CPM.Package
import CPM.ErrorLogger
import CPM.FileUtil
import CPM.Package.Helpers ( installPackageSourceTo )

import Config.Masala

------------------------------------------------------------------------------
--- The information extracted from a CPM package specfication passed to Masala.
type PackageJSON = (String,String,String,[String],[String],[String])

--- The list of dependencies.
jsonDependencies :: PackageJSON -> [String]
jsonDependencies (_,_,_,deps,_,_) = deps

--- The list of categories.
jsonCategories :: PackageJSON -> [String]
jsonCategories (_,_,_,_,_,cats) = cats

--- The package name.
jsonName :: PackageJSON -> String
jsonName (name,_,_,_,_,_) = name

--- The package version.
jsonVersion :: PackageJSON -> String
jsonVersion (_,vsn,_,_,_,_) = vsn

--- The package id.
jsonPackageID :: PackageJSON -> String
jsonPackageID pj = jsonName pj ++ "-" ++ jsonVersion pj

--- Parses a string representation of a package description
--- and returns either an error message or some package data
--- (name, version, description, dependencies, exported modules, categories).
readPackageData :: String
  -> IO (Either String PackageJSON)
readPackageData s =
  either (return . Left)
         (\pkg -> checkPackageForUpload pkg >>=
                  either (return . Left)
                         (return . Right . masalaDataOfPackage))
         (readPackageSpec s)

--- Checks whether the package is appropriate for uploading.
--- Currently, the `source` field must be defined to be a http address
--- or a git repository. Furthermore, the source must be ready for download.
checkPackageForUpload :: Package -> IO (Either String Package)
checkPackageForUpload pkg = case source pkg of
  Nothing            -> return $
                          Left "The package description has no 'source' field!"
  Just (Http url)    -> downloadSource pkg (Http url)
  Just (Git url rev) -> downloadSource pkg (Git url rev)
  _                  -> return $
                          Left "The package has an illegal 'source' field!"

--- Tries to download the source of the package. If this is not possible,
--- return `Left errormsg`, otherwise return the package.
downloadSource :: Package -> PackageSource -> IO (Either String Package)
downloadSource pkg pkgsrc = do
  downloadsrcdir <- getDownloadSourceDir
  if null downloadsrcdir
    then return $ Right pkg
    else do
      recreateDirectory $ downloadsrcdir </> packageId pkg
      (msgs,ires) <- fromErrorLoggerMsgs Info $
                       installPackageSourceTo pkg pkgsrc downloadsrcdir
      let downloaderr = "COULD NOT DOWNLOAD PACKAGE SOURCE:\n"
      maybe (return $ Left $ downloaderr ++ msgs)
            (const (createTarFile downloadsrcdir))
            ires
 where
  createTarFile downloadsrcdir = do
    createDirectoryIfMissing True downloadTarDir
    let pkgid = packageId pkg
    tarfile <- getAbsolutePath $ downloadTarDir </> pkgid ++ ".tar.gz"
    system $ unwords [ "rm", "-f", tarfile]
    let cmd = unwords [ "cd", downloadsrcdir </> pkgid, "&&"
                      , "tar", "czf", tarfile, ".", "&&"
                      , "chmod", "644", tarfile
                      ]
    ecode <- system cmd
    if ecode > 0
      then return $ Left $ "ERROR IN TARRING PACKAGE '" ++ pkgid ++"'!"
      else return $ Right pkg

--- Transforms a package specification into the data stored in Masala
--- (name, version, description, dependencies, exported modules, categories).
masalaDataOfPackage :: Package
                    -> PackageJSON
masalaDataOfPackage pkg =
  ( name pkg
  , showVersion $ version pkg
  , pkg2desc pkg
  , map (\ (Dependency p' _) -> p') (dependencies pkg)
  , exportedModules pkg
  , category pkg
  )
 where
  pkg2desc p = unwords $ words $ -- to remove leading blanks
    maybe (synopsis p)
          id
          (description p)

--- Stores the text of package specification with the same directory structure
--- as the CPM index.
storePackageSpec :: String -> String -> String -> IO ()
storePackageSpec pname pvers pkgtxt = do
  let specdir = packageSpecDir </> pname </> pvers
  recreateDirectory specdir
  writeFile (specdir </> packageSpecFile) pkgtxt

--- Publish a package and return True if it was possible.
--- Should later be implemented by contacting the cpm-upload script.
publishPackageVersion :: String -> String -> IO (Either String String)
publishPackageVersion pname pvers = do
  let specfile = packageSpecDir </> pname </> pvers </> packageSpecFile
      tarfile  = downloadTarDir </> pname ++ "-" ++ pvers ++ ".tar.gz"
  sfexists <- doesFileExist specfile
  tfexists <- doesFileExist tarfile
  testsys  <- isTestSystem
  if sfexists && (testsys || tfexists)
    then if testsys
           then readFile specfile >>= uploadPackageToMasalaStore pname pvers
           else readFile specfile >>= uploadPackageToCPM
    else return $ Left $
           if sfexists then "Tar file missing"
                       else "Specification file missing"

--- Uploads a package specification where the tar file is already stored
--- in directory `downloadTarDir` to the CPM repository via the
--- `cpm-upload-masala.cgi` script.
--- Returns either an error message or the standard output.
uploadPackageToCPM :: String -> IO (Either String String)
uploadPackageToCPM pkgspec = do
  (rc,out,err) <- evalCmd "curl"
                    ["--max-time", "30", "--data-binary", "@-", cpmUploadURL ]
                    pkgspec
  return $ if rc == 0 then Right out
                      else Left $ err ++ out

-- Implementation in a test system without really uploading: store only
-- the package specification in the `published` directory.
uploadPackageToMasalaStore :: String -> String -> String
                           -> IO (Either String String)
uploadPackageToMasalaStore pname pvers pkgspec = do
  let pubdir = masalaDataDir </> "published" </> pname ++ "-" ++ pvers
  recreateDirectory pubdir
  writeFile (pubdir </> packageSpecFile) pkgspec
  return $ Right "Package stored in published packages"

------------------------------------------------------------------------------
-- The name of the package specification file.
packageSpecFile :: String
packageSpecFile = "package.json"

------------------------------------------------------------------------------
