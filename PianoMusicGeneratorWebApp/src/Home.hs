{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Home where

import Foundation
import Yesod.Core

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|
    <h1>Hello, Marina!</h1>
    <p>Click the button below:</p>
    <button>Click me!</button>
|]
