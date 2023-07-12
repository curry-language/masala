module View.Upload
  ( uploadView )
 where

import HTML.Base
import HTML.Styles.Bootstrap4 ( hrefScndSmButton, primSmButton, scndButton )
import HTML.WUI

import Database.CDBI.Connection

import Data.List
import Data.Maybe
import Data.Time

import Config.UserProcesses
import System.Processes
import System.Spicey
import System.Authentication
import System.PackageHelpers
import Model.Masala2
import Model.Queries
import View.EntitiesToHtml

uploadView :: String -> [HtmlExp]
uploadView loginName =
  [ h3 [htxt "Upload a package:"]
  , par [htxt $ "To upload a new package or a new version of a package " ++
                "maintained by you, copy the contents of 'package.json' of " ++
                "your package into the text field below:"]
  --, htxt "PackageJSON:", nbsp
  , textArea uploadfield (20,60) "", nbsp
  , primSmButton "Upload" uploadHandler]
    where 
      uploadfield free

      uploadHandler env = do 
        let packageJson = env uploadfield

        uploadPackage loginName packageJson >>= getPage

uploadPackage :: String -> String -> IO [BaseHtml] --String -> String -> IO ViewBlock
uploadPackage loginName packageJson = do
    -- Read Package JSON
    readresult <- readPackageData packageJson
    case readresult of 
        Left err -> displayError err 
        Right json -> do
          userResult <- getUserByName loginName
          case userResult of 
            Nothing -> displayError ("User " ++ loginName ++ " does not exist, although logged in")
            Just user -> do 
              time <- getClockTime
              result <- runT $ 
                uncurry6 (action user time) json
              case result of 
                Left (DBError _ msg) -> displayError msg
                Right pkg -> return $ displaySuccess pkg

  where
    uncurry6 :: (a -> b -> c -> d -> e -> f -> g) -> (a,b,c,d,e,f) -> g
    uncurry6 func (a,b,c,d,e,f) = func a b c d e f

    getDependencies :: [String] -> DBAction [Maybe Package]
    getDependencies = mapM getPackageWithNameAction
    
    getCategories :: [String] -> DBAction [Maybe Category]
    getCategories = mapM getCategoryWithNameAction

    createNewModules :: [String] -> [CurryModule] -> DBAction [CurryModule]
    createNewModules curryModules existingModules = 
      mapM newCurryModule (curryModules \\ map curryModuleName existingModules)

    getExistingModules :: [String] -> DBAction [CurryModule]
    getExistingModules mods = fmap catMaybes $ mapM getCurryModuleWithNameAction mods
    

    action :: User -> ClockTime -> String -> String -> String -> [String] -> [String] -> [String] -> DBAction Package
    action user time name version description dependencies curryModules categories = do 
      -- Find all dependencies
      depsResults <- getDependencies dependencies
      case sequence depsResults of 
        Nothing -> actionError "Some dependencies do not exist"
        Just deps -> do
          -- Find all categories
          catsResult <- getCategories categories
          case sequence catsResult of 
            Nothing -> actionError "Some categories do not exist yet"
            Just cats -> do 
              -- Find package
              pkgResult <- getPackageWithNameAction name
              pkg <- case pkgResult of 
                -- Create new package if not already exists
                Nothing -> do
                  pkg <- newPackage name False
                  -- Add uploader as maintainer
                  newMaintainer (userKey user) (packageKey pkg)
                  return pkg
                -- Package found
                Just pkg -> return pkg
              isMaintainer <- checkIfMaintainerAction pkg user
              case isMaintainer of 
                False -> actionError "User is not a maintainer of this package"
                True -> do 
                  vsnResult <- getPackageVersionByNameAction name version
                  case vsnResult of 
                    -- Version already exists, deny upload
                    Just vsn -> actionError "Version already exists, please choose another version"
                    -- Version available, upload package
                    Nothing -> do 
                      -- Create version
                      vsn <- newVersionWithPackageVersioningKeyWithUserUploadKey
                        version False False description "JobStatus" 0 time False (packageKey pkg) (userKey user)
                      -- Create dependencies
                      mapM_ (newDepending (versionKey vsn)) (map packageKey deps)
                      -- Create and get CurryModules
                      -- Find already existing modules
                      existingModules <- getExistingModules curryModules
                      -- Create not existing modules
                      newModules <- createNewModules curryModules existingModules
                      -- Create exports
                      let mods = newModules ++ existingModules
                      -- Create exports
                      mapM_ (newExporting (versionKey vsn)) (map curryModuleKey mods)
                      -- Create categorizes
                      mapM_ (flip newCategorizes (versionKey vsn)) (map categoryKey cats)

                      return pkg

    
    actionError msg = failDB (DBError UnknownError msg)

    displaySuccess :: Package -> [BaseHtml] -- ViewBlock
    displaySuccess pkg = 
      [htxt $ "Package " ++ (packageName pkg) ++ " was successfully uploaded!"]
        


-----------------------------------------------------------------------------
