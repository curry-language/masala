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
             -> Either String (String,String,String,[String],[String],[String])
readPackageData s =
  either Left
         (Right . masalaDataOfPackage)
         (readPackageSpec s)

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
