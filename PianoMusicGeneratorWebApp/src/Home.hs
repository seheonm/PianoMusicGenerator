{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- This module contains the home page logic, including music generation and playing functionalities.

module Home where
    
import HomePage (HasHomeHandler(..))
import Data.Char (isLetter)
import Foundation
import Yesod.Core
import Euterpea
import System.Random (randomRIO)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Control.Concurrent (forkIO)

-- Takes a beats per minute (bpm), a time signature, and a music piece
playMusic :: Double -> TimeSignature -> Music Pitch -> IO ()
-- Plays the music piece at a tempo, which is adjusted by the bpm relative to the tempo (120 bpm)
playMusic bpm timeSignature m = play $ tempo (toRational (bpm / 120)) m

-- Maps a musical key signature represented as a string to a corresponding Pitch. This includes the notes and octaves
-- If the key signature is not recognized, it returns Nothing
keySignatureToPitch :: String -> Maybe Pitch
keySignatureToPitch "C" = Just (C, 4)
keySignatureToPitch "G" = Just (G, 4)
keySignatureToPitch "D" = Just (D, 4)
keySignatureToPitch "A" = Just (A, 4)
keySignatureToPitch "E" = Just (E, 4)
keySignatureToPitch "B" = Just (B, 4)
keySignatureToPitch "Gb" = Just (Fs, 4)
keySignatureToPitch "Db" = Just (Cs, 4)
keySignatureToPitch "Ab" = Just (Gs, 4)
keySignatureToPitch "Eb" = Just (Ds, 4)
keySignatureToPitch "Bb" = Just (As, 4)
keySignatureToPitch "F" = Just (F, 4)
keySignatureToPitch _ = Nothing

-- Converts a note value into a corresponding duration (whole note, half note)
-- Throws an error if an unsupported note value is provided
noteValueToDuration :: Int -> Dur
-- Mapping for each note value to its corresponding duration
noteValueToDuration 1 = wn
noteValueToDuration 2 = hn
noteValueToDuration 4 = qn
noteValueToDuration 8 = en
noteValueToDuration 16 = sn
noteValueToDuration _ = error "Unsupported note value"

-- Web handler that generates and plays music based on parameters received in a GET request
-- These parameters include key signatue, bpm, and time signature
getGenerateMusicR :: Handler Html
getGenerateMusicR = do
    -- Retrieving parameters from the GET request
    mbKeySignature <- lookupGetParam "keySignature"
    mbBpm <- lookupGetParam "bpm"
    
    -- Default values and parsing for time signature and bpm
    mbTimeSignature <- lookupGetParam "timeSignature"
    let timeSignature = fromMaybe (4,4) (mbTimeSignature >>= parseTimeSignature . T.unpack)
        numMeasures = 2
        keySignature = fromMaybe "C" (fmap T.unpack mbKeySignature)
        bpm = fromMaybe 120 (mbBpm >>= readMaybe . T.unpack)  -- Default to 120 if not present or invalid
    
    -- Generate and play music based on the key signature
    case keySignatureToPitch keySignature of
        Just pitch -> do
            music <- liftIO $ generateMusic timeSignature numMeasures pitch
            _ <- liftIO $ forkIO $ playMusic (fromIntegral bpm) timeSignature music
            return [shamlet|Music Played|]
        Nothing -> return [shamlet|Invalid Key Signature|]

-- Generates a music piece based on a given time signature, number of measures, and starting pitch
generateMusic :: TimeSignature -> Int -> Pitch -> IO (Music Pitch)
generateMusic (numBeats, beatValue) numMeasures pitch@(pc, oct) = do
  let dur = noteValueToDuration beatValue
      scale = majorScale pitch  
      -- Inner function to generate a random pitch from the scale
      generateRandomPitch :: IO Pitch
      generateRandomPitch = do
        index <- randomRIO (0, length scale - 1)
        return $ scale !! index
        -- Inner function to generate a measure of music
      generateMeasure :: IO (Music Pitch)
      generateMeasure = do
        notes <- mapM (\_ -> generateRandomPitch >>= \p -> return $ note dur p) [1..numBeats]
        return $ line notes
  -- Generating the full piece by combining measures
  measures <- mapM (\_ -> generateMeasure) [1..numMeasures]
  return $ line measures

-- type for a time signature represented as a pair of intergers
type TimeSignature = (Int, Int)

-- Parses a string to extract a time signature as a pair of integers
-- Returns Nothing if the parsing fails
parseTimeSignature :: String -> Maybe TimeSignature
parseTimeSignature str = case map T.unpack . T.splitOn (T.pack "/") . T.pack $ str of
    [num, denom] -> Just (read num, read denom)
    _            -> Nothing

-- Generates a major scale starting from a given pitch
majorScale :: Pitch -> [Pitch]
majorScale (p, o) = take 8 $ iterate (nextPitch) (p, o)
  where
    -- Calculates the next pitch in the scale
    nextPitch :: Pitch -> Pitch
    nextPitch (p, o) = case p of
      B  -> (C, o + 1)
      E  -> (F, o)
      _  -> (succ p, o)

-- Takes a Pitch and plays a single note
playNote :: Pitch -> IO ()
-- Creates a quarter note with the given Pitch
playNote p = play $ note qn p

-- Handler function that takes a string representation of a note and tries to parse it into a Pitch 
-- If successful, it plays the note, otherwise there's an error
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

-- Function tries to convert a string into a Pitch and it looks for a note name followed by an octave number
-- If the parsing works, it returns Just Pitch and if not, it returns Nothing
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

  -- Takes a string and tries to parse it into an integer representing the octave. 
  -- If the string is parsed into a number then by nothing else, that number is returned, but if not there will be an error
  where
    readOctave str = 
      case reads str :: [(Int, String)] of
        [(val, "")] -> val
        _           -> error $ "Invalid octave: " ++ str

getHomeR :: HasHomeHandler master => HandlerFor master Html
getHomeR = getHomeHandler