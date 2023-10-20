{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Home where

import Foundation
import Yesod.Core
import Euterpea

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello, welcome to our Piano Music Generator!

<audio id="myAudio">
    <source src=@{StaticR _Recording_m4a} type="audio/x-m4a">
<button onclick="playAudio()">Play Sound</button>
<script>
    function playAudio() {
        var x = document.getElementById("myAudio");
        x.play();
    }
<img src=@{StaticR _IMG_1960_jpg}>

|]
