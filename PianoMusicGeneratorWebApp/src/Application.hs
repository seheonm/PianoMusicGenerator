{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import Home 
import Foundation
import Yesod.Core
import Add

-- Generate the dispatch function and associated routing for the 'App' data type
mkYesodDispatch "App" resourcesApp