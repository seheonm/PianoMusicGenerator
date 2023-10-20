{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Foundation where

import Yesod.Core
import Yesod.Static
import Euterpea (Pitch, absPitch, pitch)
import Yesod.Core (PathPiece(..))
import qualified Data.Text.Read as TR
import Data.Text (pack, unpack)


data App = App
    { getStatic :: Static
    }

$(staticFiles "static")

mkYesodData "App" $(parseRoutesFile "routes.yesodroutes")

instance PathPiece Pitch where
    toPathPiece p = pack (show (absPitch p))
    fromPathPiece t =
        case TR.decimal t of
            Right (a, "") -> Just (pitch a)
            _             -> Nothing

instance Yesod App where
