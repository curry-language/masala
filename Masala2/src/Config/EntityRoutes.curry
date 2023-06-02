module Config.EntityRoutes () where

import System.Spicey
import Model.Masala2

instance EntityController User where
  controllerOnKey s = applyControllerOn (readUserKey s) (runJustT . getUser)

  entityRoute r ent = concat ["?User/",r,"/",showUserKey ent]

instance EntityController Package where
  controllerOnKey s =
    applyControllerOn (readPackageKey s) (runJustT . getPackage)

  entityRoute r ent = concat ["?Package/",r,"/",showPackageKey ent]

instance EntityController Version where
  controllerOnKey s =
    applyControllerOn (readVersionKey s) (runJustT . getVersion)

  entityRoute r ent = concat ["?Version/",r,"/",showVersionKey ent]

instance EntityController Category where
  controllerOnKey s =
    applyControllerOn (readCategoryKey s) (runJustT . getCategory)

  entityRoute r ent = concat ["?Category/",r,"/",showCategoryKey ent]

instance EntityController CurryModule where
  controllerOnKey s =
    applyControllerOn (readCurryModuleKey s) (runJustT . getCurryModule)

  entityRoute r ent = concat ["?CurryModule/",r,"/",showCurryModuleKey ent]

instance EntityController ValidationToken where
  controllerOnKey s =
    applyControllerOn (readValidationTokenKey s)
     (runJustT . getValidationToken)

  entityRoute r ent =
    concat ["?ValidationToken/",r,"/",showValidationTokenKey ent]
