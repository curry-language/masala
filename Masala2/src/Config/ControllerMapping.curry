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

--- Maps the controllers associated to URLs in module RoutesData
--- into the actual controller operations.
getController :: ControllerReference -> Controller
getController fktref =
  case fktref of
    ProcessListController -> processListController
    LoginController -> loginController
    UserController -> mainUserController
    PackageController -> mainPackageController
    VersionController -> mainVersionController
    CategoryController -> mainCategoryController
    CurryModuleController -> mainCurryModuleController
    ValidationTokenController -> mainValidationTokenController
    -- _ -> displayError "getController: no mapping found"
