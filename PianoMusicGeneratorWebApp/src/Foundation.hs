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

-- Instance declaration to be used as part of URL paths.
instance PathPiece Pitch where
    -- Converts a `Pitch` to a `Text` representation.
    toPathPiece p = pack (show (absPitch p))
    -- Tries to convert from `Text` to `Pitch`.
    fromPathPiece t =
        case TR.decimal t of
            Right (a, "") -> Just (pitch a) -- Successfully parsed an integer and used it to create a `Pitch`.
            _             -> Nothing -- Parsing failed, returns Nothing.

instance Yesod App where
