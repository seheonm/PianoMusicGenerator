{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Home where
    
import HomePage (HasHomeHandler(..))
import Data.Char (isLetter)
import Foundation
import Yesod.Core
import Euterpea
import Control.Concurrent (threadDelay)

playMusic :: Music Pitch -> IO ()
playMusic m = play $ tempo (120/60) m

getGenerateMusicR :: Handler Html
getGenerateMusicR = do
    liftIO $ playMusic $ line $ replicate 16 $ note qn (C, 4)
    return [shamlet|Music Played|]


playNote :: Pitch -> IO ()
playNote p = play $ note qn p

getPlayNoteR :: String -> Handler Html
getPlayNoteR noteStr = do
    liftIO $ putStrLn $ "Received note: " ++ noteStr  -- Log the received note
    case parsePitch noteStr of
        Just pitch -> do
            liftIO $ putStrLn $ "Parsed pitch: " ++ show pitch  -- Log the parsed pitch
            -- Parsed the Pitch, so play the note
            liftIO $ playNote pitch
            return [shamlet|Note played|]
        Nothing -> do
            liftIO $ putStrLn $ "Failed to parse pitch: " ++ noteStr  -- Log the failure
            -- Did not parse a Pitch, so return an error
            return [shamlet|Invalid note: #{noteStr}|]

parsePitch :: String -> Maybe Pitch
parsePitch s = case span isLetter s of
    ("C", rest)      -> Just (C, readOctave rest)
    ("Cs", rest)  -> Just (Cs, readOctave rest)
    ("D", rest)      -> Just (D, readOctave rest)
    ("Ds", rest)  -> Just (Ds, readOctave rest)
    ("E", rest)      -> Just (E, readOctave rest)
    ("F", rest)      -> Just (F, readOctave rest)
    ("Fs", rest)  -> Just (Fs, readOctave rest)
    ("G", rest)      -> Just (G, readOctave rest)
    ("Gs", rest)  -> Just (Gs, readOctave rest)
    ("A", rest)  -> Just (A, readOctave rest)
    ("As", rest)  -> Just (As, readOctave rest)
    ("B", rest)      -> Just (B, readOctave rest)
    _                -> Nothing

  where
    readOctave str = 
      case reads str :: [(Int, String)] of
        [(val, "")] -> val
        _           -> error $ "Invalid octave: " ++ str


getHomeR :: HasHomeHandler master => HandlerFor master Html
getHomeR = getHomeHandler