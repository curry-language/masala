module View.Upload
  ( wUploadJson, wUploadCheck, uploadPackage, uploadPackageAction )
 where

import HTML.Base
import HTML.Styles.Bootstrap4 ( hrefScndSmButton, primSmButton, scndSmButton )
import HTML.WUI

import Database.CDBI.Connection

import Data.Either
import Data.List
import Data.Maybe
import Data.Time

import Config.UserProcesses
import Controller.Version ( deleteVersionT )
import Controller.Mail ( sendNotificationEmail )
import System.Processes
import System.SessionInfo
import System.Spicey
import System.Authentication
import System.PackageHelpers
import System.PreludeHelpers
import Model.Masala2
import Model.Queries
import View.EntitiesToHtml

--- The labels of an Upload, as used in HTML tables.
uploadJsonLabelList :: HTML h => [[h]]
uploadJsonLabelList =
  [[textstyle "spicey_label spicey_label_for_type_string" "JSON"]]

--- The WUI specification for an Upload.
--- The field gets a json string.
wUploadJson :: WuiSpec String
wUploadJson = 
  withRendering
    (wTextArea (20,60))
    (renderLabels uploadJsonLabelList)

--- A confirmation page for an Admin when a version already exists or
--- some categories do not exist.
wUploadCheck :: (String, String, Maybe PackageJSON) -> [HtmlExp]
wUploadCheck (msg, packageJson, jsonMaybe) =
  [ h3 [htxt msg]
  , par [htxt $ "To upload the new package anyways, an already existing " ++
                "version might be overwritten or new categories may be " ++
                "created. Please confirm or cancel the upload."]
  , primSmButton "Upload" uploadHandler, nbsp
  , scndSmButton "Cancel" cancelHandler]
    where
      uploadHandler _ = do
        sinfo <- getUserSessionInfo
        case userLoginOfSession sinfo of 
          Nothing -> displayUrlError >>= getPage
          Just (login, _) -> 
            case jsonMaybe of 
              Nothing -> displayError "No json data" >>= getPage
              Just jsonData -> do
                uploadPackage login packageJson jsonData True >>= getPage

      cancelHandler _ = redirectController "?Upload" >>= getPage

--- This function uploads a new Version for a Package and returns the next webpage.
--- @param loginName - The name of the User uploading the Package
--- @param packageJson - The json string of the package
--- @param jsonData - The tuple read from the json string
--- @param adminConfirmation - A confirmation from an Admin whether to overwrite
---                            an existing Version or to create non-existing Categories
uploadPackage :: String -> String -> PackageJSON -> Bool -> IO [BaseHtml]
uploadPackage loginName packageJson jsonData adminConfirmation = do
    userResult <- getUserByName loginName
    case userResult of 
      Nothing -> displayError ("User " ++ loginName ++ " does not exist, although logged in")
      Just user -> do 
        time <- getClockTime        
        result <- runT $ 
          uncurry6 (uploadPackageAction user time adminConfirmation) jsonData
        case result of 
          Left (DBError _ msg) -> displayError msg
          Right (pkg, vsn) -> do
            storePackageSpec (jsonName jsonData) (jsonVersion jsonData) packageJson
            watchingUsers <- getWatchingUsers pkg
            mapM_ (sendNotificationEmail pkg vsn) watchingUsers
            return $ displaySuccess pkg (jsonVersion jsonData)
  where
    displaySuccess :: Package -> String -> ViewBlock
    displaySuccess pkg vers = 
      [htxt $ "Package '" ++ packageName pkg ++ "-" ++ vers ++
              "' successfully uploaded!"]

--- This action uploads a new Version for a Package
--- @param user - The User uploading the new Version
--- @param time - The current time
--- @param adminConfirmation - A confirmation from an Admin whether to overwrite
---                            an existing Version or to create non-existing Categories
--- @param name - The name of the Package
--- @param version - The version number
--- @param description - The description of the Version
--- @param dependencies - The list of dependencies
--- @param curryModules - The modules being exported
--- @param categories - The list of categories
uploadPackageAction
  :: User -> ClockTime -> Bool -> String -> String -> String -> [String]
  -> [String] -> [String] -> DBAction (Package, Version)
uploadPackageAction user time adminConfirmation name version description dependencies curryModules categories = do
    -- Find all dependencies
    depsResult <- getDependenciesWithNameAction dependencies
    let (missingDeps, deps) = partitionEithers depsResult
    -- Find all categories
    catsResult <- getCategoriesWithNameAction categories
    let (missingCats, cats) = partitionEithers catsResult
    -- Look for missing dependencies and categories
    case null missingDeps && (null missingCats || adminConfirmation) of
      False -> dependencyCategoryError missingDeps missingCats
      True -> do 
        -- Find package
        pkgResult <- getPackageWithNameAction name
        pkg <- case pkgResult of 
          -- Create new package if not already exists
          Nothing -> do
            newpkg <- newPackage name False
            -- Add uploader as maintainer
            newMaintainer (userKey user) (packageKey newpkg)
            return newpkg
          -- Package found
          Just pkg' -> return pkg'
        isMaintainer <- checkIfMaintainerAction pkg user
        case isMaintainer of 
          False -> actionError "User is not a maintainer of this package"
          True -> do 
            vsnResult <- getPackageVersionByNameAction name version
            if isJust vsnResult && not adminConfirmation
              -- Version already exists, no admin confirmation, deny upload
              then actionError $ "Version '" ++ name ++ "-" ++ version ++
                                 "' already exists, please choose another version"
              -- Version does not exist or we have admin confirmation
              else do
                case vsnResult of 
                  Nothing -> return ()
                  Just vsn -> do 
                    -- Delete Version
                    deleteVersionT vsn
                vsn <- newVersionWithPackageVersioningKeyWithUserUploadKey
                    version False False description "JobStatus" 0 time False (packageKey pkg) (userKey user)
                -- Create dependencies
                mapM_ (newDepending (versionKey vsn)) (map packageKey deps)
                -- Find already existing modules
                existingModules <- getExistingModules curryModules
                -- Create not existing modules
                newModules <- createNewModules curryModules existingModules
                -- Create exports
                let mods = newModules ++ existingModules
                -- Create exports
                mapM_ (newExporting (versionKey vsn)) (map curryModuleKey mods)
                -- We have admin confirmation, create missing Categories
                newCats <- createNewCategories missingCats
                -- Create categorizes
                mapM_ (flip newCategorizes (versionKey vsn)) (map categoryKey (cats ++ newCats))

                return (pkg, vsn)
  where
    actionError msg = failDB (DBError UnknownError msg)

    dependencyCategoryError :: [String] -> [String] -> DBAction (Package, Version)
    dependencyCategoryError missingDeps missingCats = let
        msg = dependencyError missingDeps ++ "; " ++ categoryError missingCats
      in actionError msg

    dependencyError :: [String] -> String
    dependencyError missingDeps = case missingDeps of
      [] -> ""
      _  -> "Some dependencies do not exist: " ++ show missingDeps
    
    categoryError :: [String] -> String
    categoryError missingCats = case missingCats of
      [] -> ""
      _  -> "Some categories do not exist: " ++ show missingCats

    createNewModules :: [String] -> [CurryModule] -> DBAction [CurryModule]
    createNewModules curryMods existingMods = 
      mapM newCurryModule (curryMods \\ map curryModuleName existingMods)
    
    createNewCategories :: [String] -> DBAction [Category]
    createNewCategories = mapM (flip newCategory "")

    getExistingModules :: [String] -> DBAction [CurryModule]
    getExistingModules mods = fmap catMaybes $ mapM getCurryModuleWithNameAction mods
-----------------------------------------------------------------------------
