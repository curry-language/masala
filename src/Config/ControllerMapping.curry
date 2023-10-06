module Config.ControllerMapping where

import System.Spicey
import System.Routes
import Controller.SpiceySystem
import Config.RoutesData
import Controller.User
import Controller.Package
import Controller.Version
import Controller.Category
import Controller.CurryModule
import Controller.ValidationToken
import Controller.Registration
import Controller.Validation
import Controller.Upload
import Controller.Mail

--- Maps the controllers associated to URLs in module RoutesData
--- into the actual controller operations.
getController :: ControllerReference -> Controller
getController fktref =
  case fktref of
    ProcessListController -> processListController
    WelcomeController -> welcomeController
    MailController -> mailController
    LoginController -> loginController
    UserController -> mainUserController
    PackageController -> mainPackageController
    VersionController -> mainVersionController
    CategoryController -> mainCategoryController
    CurryModuleController -> mainCurryModuleController
    ValidationTokenController -> mainValidationTokenController
    RegistrationController -> registrationController
    ValidationController -> validationController
    UploadController -> uploadController
    -- _ -> displayError "getController: no mapping found"
