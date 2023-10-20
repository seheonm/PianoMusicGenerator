{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Home where

import Foundation
import Yesod.Core
import Euterpea

playNote :: Pitch -> IO ()
playNote p = play $ note qn p

getPlayNoteR :: Pitch -> Handler Html
getPlayNoteR p = liftIO (playNote p) >> return [shamlet|Note played|]

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|
    Hello, welcome to our Piano Music Generator!
    <button onclick="playNote('C4')">C</button>
    <button onclick="playNote('Cs4')">C#</button>
    <button onclick="playNote('D4')">D</button>
    <button onclick="playNote('Ds4')">D#</button>
    <button onclick="playNote('E4')">E</button>
    <button onclick="playNote('F4')">F</button>
    <button onclick="playNote('Fs4')">F#</button>
    <button onclick="playNote('G4')">G</button>
    <button onclick="playNote('Gs4')">G#</button>
    <button onclick="playNote('A4')">A</button>
    <button onclick="playNote('As4')">A#</button>
    <button onclick="playNote('B4')">B</button>
    <script>
        function playNote(note) {
            fetch(`/playNote/${note}`);
        }
|]
