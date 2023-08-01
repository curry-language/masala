module Controller.Upload
  ( uploadController, uploadJsonForm, uploadCheckForm, uploadByName ) where

import Data.List ( last, sortBy, (\\) )

import Data.Either
import Data.Maybe
import Data.Time
import HTML.Base
import HTML.Session
import HTML.WUI
import Model.Masala2
import Model.Queries
import Config.EntityRoutes
import Config.Roles
import Config.UserProcesses
import Controller.Mail       ( sendNotificationEmail )
import System.SessionInfo
import System.Authorization
import System.AuthorizedActions
import System.PackageHelpers
import System.PreludeHelpers
import System.Spicey
import View.EntitiesToHtml
import View.Upload
import Database.CDBI.Connection

uploadController :: Controller
uploadController = do 
    args <- getControllerParams
    case args of
        [] -> uploadJsonController
        ["check"] -> uploadCheckController
        _ -> displayUrlError

uploadJsonController :: Controller
uploadJsonController =
    checkAuthorization (packageOperationAllowed NewEntity)
        $ (\sinfo -> do
            setParWuiStore uploadJsonStore sinfo ""
            return [formElem uploadJsonForm])

uploadJsonForm :: HtmlFormDef (UserSessionInfo,WuiStore String)
uploadJsonForm =
        pwui2FormDef "Controller.Upload.uploadJsonForm" uploadJsonStore
            (\_ -> wUploadJson)
            (\_ json -> 
                checkAuthorization (packageOperationAllowed NewEntity)
                (\sinfo -> do
                    jsonResult <- readPackageData json
                    case jsonResult of
                        Left err -> displayError err
                        Right jsonData -> do
                            -- Check if version exists
                            vsnExist <- do
                                pkgResult <- getPackageWithName (jsonName jsonData)
                                case pkgResult of 
                                    Nothing -> return False
                                    Just _ -> do
                                        vsnResult <- getPackageVersionByName (jsonName jsonData) (jsonVersion jsonData)
                                        return $ isJust vsnResult
                            -- Check if some cats do not exist
                            nonExistingCats <- do
                                cats <- getCategoriesWithName (jsonCategories jsonData)
                                return $ lefts cats
                            let msg = errorMessage jsonData vsnExist nonExistingCats
                            -- Check if admin
                            if isAdminSession sinfo && (vsnExist || not (null nonExistingCats))
                                then do
                                    putSessionData uploadCheckStore (msg, json, Just jsonData)
                                    redirectController "?Upload/check"
                                else do 
                                    case userLoginOfSession sinfo of 
                                        Nothing -> displayError "User not logged in"
                                        Just (login, _) -> do
                                            uploadPackageView login json jsonData False
                )
            )
            (\sinfo -> renderWUI sinfo "Upload Package" "Upload" "?" ())
    where
        errorMessage :: PackageJSON -> Bool -> [String] -> String
        errorMessage jd vsnExist nonExistingCats = unlines [vsnMsg, catMsg]
         where
          vsnMsg = if vsnExist
                     then "Version '" ++ jsonPackageID jd ++ "' already exists!"
                     else ""
          catMsg = if null nonExistingCats
                     then "" 
                     else "Some categories do not exist: " ++ unwords nonExistingCats

uploadJsonStore :: SessionStore (UserSessionInfo,WuiStore String)
uploadJsonStore = sessionStore "uploadJsonStore"

uploadCheckController :: Controller
uploadCheckController = do
    checkAuthorization
        (packageOperationAllowed NewEntity)
        (\sinfo ->
            if isAdminSession sinfo
                then return [formElem uploadCheckForm]
                else displayUrlError
        )

uploadCheckForm :: HtmlFormDef (String, String, Maybe PackageJSON)
uploadCheckForm = formDefWithID "Controller.Upload.uploadCheckForm"
    (getSessionData uploadCheckStore ("", "", Nothing)) wUploadCheck

uploadCheckStore :: SessionStore (String, String, Maybe PackageJSON)
uploadCheckStore = sessionStore "uploadCheckStore"

--------------------------------------------------------

--- Uploads a package in batch mode, i.e., the parameters are the
--- login name, the encrypted password and the text of the package
--- specification (`package.json` file contents.
--- Then it is checked whether the password is correct and the user
--- is allowed to upload the package.
--- The result is either an error message (`Left`) or
--- a message about the successful upload (`Right`).
--- If the `publish` parameter is `True`, the package should directly
--- be published (by calling `System.PackageHelpers.pubhlishPackageVersion`)
--- if the user is an admin or a truested(!) maintainer of the package.
--- If the `force` parameter is `True`, overwriting an existing version
--- is allowed if the user as an admin.
uploadByName :: String -> String -> String -> Bool -> Bool
             -> IO (Either String String)
uploadByName login passwd packagetxt publish force = do
  userResult <- getUserByLoginData login passwd
  case userResult of 
    Nothing -> uploadFail loginErrMsg
    Just user -> do
      case force && userRole user /= roleAdmin of
        True -> uploadFail "'force' can only be used by an Admin"
        False -> do
          case publish && 
               userRole user /= roleAdmin && userRole user /= roleTrusted of
            True -> do upmsg <- uploadByName login passwd packagetxt False force
                       return $ either Left (\m -> Right $ noPublishCmt ++ m)
                                       upmsg
            False -> do
              jsonResult <- readPackageData packagetxt
              case jsonResult of
                Left err -> uploadFail err
                Right json -> do
                  time <- getClockTime
                  uploadResult <- uncurry6
                                    (uploadPackage packagetxt user time force)
                                    json
                  case uploadResult of
                    Left msg -> uploadFail msg
                    Right (pkg, vsn) ->
                      if publish
                        then do
                          publishResult <- publishPackageVersion
                                          (packageName pkg) (versionVersion vsn)
                          case publishResult of 
                              Left err -> uploadFail $
                                "Package successfully uploaded, " ++
                                "but publishing failed: " ++ err
                              Right msg -> do
                                vsnResult <- runT $ updateVersion $
                                               setVersionPublished vsn True
                                case vsnResult of
                                  Left _  -> uploadFail setPublishErrMsg
                                  Right _ -> return $ Right $ publishMessage msg
                        else return $ Right uploadMessage
 where
  uploadFail msg = return $ Left $ "Upload failure: " ++ msg

  uploadMessage = "Package is successfully uploaded but not yet published.\n" ++
    "To publish it, go to the Masala web site (if you are a trusted user)\n" ++
    "or ask the Masala admin for publishing."

  publishMessage out =
    "Package successfully uploaded and published.\n" ++
    "Output from package uploader:\n" ++ out

  noPublishCmt = "Publishing packages is only allowed for trusted users!\n"
  
  setPublishErrMsg =
    "Package successfully uploaded but something went wrong publishing it"
  
  loginErrMsg = "Masala login failed (wrong login name or password?)"
    