--------------------------------------------------------------------------
--- This module defines operations to support the handling of routes
--- to controllers.
--------------------------------------------------------------------------

module System.PackageHelpers
 where

import CPM.Package

--- Parses a string representation of a package description
--- and returns either an error message or some package data
--- (name, version, description, dependencies, exported modules, categories).
readPackageData :: String
  -> IO (Either String (String,String,String,[String],[String],[String]))
readPackageData s =
  either (return . Left)
         (\pkg -> checkPackageForUpload pkg >>=
                  either (return . Left)
                         (return . Right . masalaDataOfPackage))
         (readPackageSpec s)

--- Checks whether the package is appropriate for uploading.
--- Currently, the `source` field must be defined to be a http address
--- or a git repository.
--- Later, one should also check whether the source can be downloaded.
checkPackageForUpload :: Package -> IO (Either String Package)
checkPackageForUpload pkg = case source pkg of
  Nothing            -> return $
                          Left "The package description has no 'source' field!"
  Just (Http url)    -> return $ Right pkg
  Just (Git url rev) -> return $ Right pkg
  _                  -> return $
                          Left "The package has an illegal 'source' field!"

--- Transforms a package specification into the data stored in Masala
--- (name, version, description, dependencies, exported modules, categories).
masalaDataOfPackage :: Package
                    -> (String,String,String,[String],[String],[String])
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
