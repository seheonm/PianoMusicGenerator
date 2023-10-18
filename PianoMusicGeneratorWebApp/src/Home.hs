{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Home where

import Foundation
import Yesod.Core

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|
Hello, World!
<img src=@{StaticR _IMG_1960_jpg}>
|]