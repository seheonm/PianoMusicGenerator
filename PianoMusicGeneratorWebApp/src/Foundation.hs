{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module Foundation where

import Yesod.Core
import Yesod.Static

data App = App
    { getStatic :: Static
    }

$(staticFiles "static")

mkYesodData "App" $(parseRoutesFile "routes.yesodroutes")

instance Yesod App where
