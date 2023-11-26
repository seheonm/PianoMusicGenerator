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
import Control.Monad (replicateM)
import System.Random (randomRIO, randomIO)


-- type for a time signature represented as a pair of intergers
type TimeSignature = (Int, Int)


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
        numMeasures = 8 -- this is behaving weird, the bass doesnt cycle the chord progression
        keySignature = fromMaybe "C" (fmap T.unpack mbKeySignature)
        bpm = fromMaybe 120 (mbBpm >>= readMaybe . T.unpack)  -- Default to 120 if not present or invalid
    
    -- Generate and play music based on the key signature
    case keySignatureToPitch keySignature of
        Just pitch -> do
            music <- liftIO $ composeMelodyAndBass timeSignature numMeasures pitch
            _ <- liftIO $ forkIO $ playMusicWithTempo (fromIntegral bpm) timeSignature music
            return [shamlet|Music Played|]
        Nothing -> return [shamlet|Invalid Key Signature|]

-- Generates a melody and a bass line, then combines them
composeMelodyAndBass :: TimeSignature -> Int -> Pitch -> IO (Music Pitch)
composeMelodyAndBass ts numMeasures pitch = do
    melodyLine <- generateMelodyLine ts numMeasures pitch  -- Melody line
    bassLine <- generateBassLine ts numMeasures (lowerOctave pitch)  -- Bass line an octave lower
    return (melodyLine :=: bassLine)

-- Takes a beats per minute (bpm), a time signature, and a music piece
playMusicWithTempo :: Double -> TimeSignature -> Music Pitch -> IO ()
playMusicWithTempo bpm timeSignature m = play $ tempo (toRational (bpm / 120)) m

-- Function to generate a bass line for a given time signature, number of measures, and pitch
generateBassLine :: TimeSignature -> Int -> Pitch -> IO (Music Pitch)
generateBassLine (numBeats, beatType) numMeasures pitch = do
    progression <- randomProgression
    let scale = majorScale pitch
    let progressionPitches = map (\degree -> scale !! degree) progression
    let duration = wn * fromIntegral numBeats / fromIntegral beatType
    let bassNotes = map (\p -> note duration p) progressionPitches
    let fullProgression = take numMeasures . cycle $ bassNotes
    return $ line fullProgression


-- Lower the pitch by one octave
lowerOctave :: Pitch -> Pitch
lowerOctave p = pitch (absPitch p - 12)
    
-- Function to generate a random chord progression
randomProgression :: IO [Int]
randomProgression = do
    idx <- randomRIO (0, length progressions - 1)
    return (progressions !! idx)

-- Pre-defined chord progressions represented as integer lists
progressions :: [[Int]]
progressions = [
    [0, 3, 4, 0],    -- I-IV-V-I
    [0, 4, 5, 3],    -- I-V-vi-IV
    [0, 4, 3, 0],    -- I-V-IV-I
    [0, 5, 3, 4],    -- I-vi-IV-V
    [5, 3, 0, 4],    -- vi-IV-I-V
    [0, 2, 3, 4],    -- I-iii-IV-V
    [0, 3, 1, 4]     -- I-IV-ii-V
    ]

-- Function to generate a single melody line for given time signature, number of measures, and pitch
generateMelodyLine :: TimeSignature -> Int -> Pitch -> IO (Music Pitch)
generateMelodyLine ts@(numBeats, beatValue) numMeasures pitch = do
    let scale = majorScale pitch
    phrases <- mapM (\m -> generateMelodyPhrase scale m ts) [1..numMeasures]
    return $ line $ concat phrases

-- Function to generate a melody phrase based on a scale, measure number, and time signature
generateMelodyPhrase :: [Pitch] -> Int -> TimeSignature -> IO [Music Pitch]
generateMelodyPhrase scale measureNumber (numBeats, beatValue) = do
    let measureDuration = fromIntegral numBeats * beatValueToDuration beatValue
    generatePhrase measureDuration scale

-- Function to generate a musical phrase based on a duration and a scale
generatePhrase :: Dur -> [Pitch] -> IO [Music Pitch]
generatePhrase remainingDur scale = go remainingDur []
  where
    go dur acc
      | dur <= 0 = return acc
      | otherwise = do
          pitch <- generateRandomPitch scale
          noteDur <- chooseNoteDuration dur
          let newNote = note noteDur pitch
          go (dur - noteDur) (acc ++ [newNote])

-- Function to choose a note duration based on the remaining duration
chooseNoteDuration :: Dur -> IO Dur
chooseNoteDuration remainingDur = do
    let possibleDurations = filter (<= remainingDur) [wn, hn, qn, en, sn]
    idx <- randomRIO (0, length possibleDurations - 1)
    return $ possibleDurations !! idx

-- Function to generate a random pitch from a given scale
generateRandomPitch :: [Pitch] -> IO Pitch
generateRandomPitch scale = do
    index <- randomRIO (0, length scale - 1)
    return $ scale !! index

-- Maps a musical key signature represented as a string to a corresponding Pitch. 
-- This includes the notes and octaves
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

-- Function to generate a major scale starting from a given pitch
majorScale :: Pitch -> [Pitch]
majorScale startPitch = take 8 $ iterate nextPitch startPitch
  where
    nextPitch :: Pitch -> Pitch
    nextPitch (p, o) =
      let
        nextVal = pitchClassToVal p + interval (pitchClassToVal p)
        nextP = valToPitchClass (nextVal `mod` 12)
        nextO = if nextVal >= 12 then o + 1 else o
      in (nextP, nextO)

    pitchClassToVal :: PitchClass -> Int
    pitchClassToVal C = 0
    pitchClassToVal Cs = 1
    pitchClassToVal D = 2
    pitchClassToVal Ds = 3
    pitchClassToVal E = 4
    pitchClassToVal F = 5
    pitchClassToVal Fs = 6
    pitchClassToVal G = 7
    pitchClassToVal Gs = 8
    pitchClassToVal A = 9
    pitchClassToVal As = 10
    pitchClassToVal B = 11

    valToPitchClass :: Int -> PitchClass
    valToPitchClass 0 = C
    valToPitchClass 1 = Cs
    valToPitchClass 2 = D
    valToPitchClass 3 = Ds
    valToPitchClass 4 = E
    valToPitchClass 5 = F
    valToPitchClass 6 = Fs
    valToPitchClass 7 = G
    valToPitchClass 8 = Gs
    valToPitchClass 9 = A
    valToPitchClass 10 = As
    valToPitchClass 11 = B

    interval :: Int -> Int
    interval val = majorScaleIntervals !! (val `mod` 7)

    majorScaleIntervals :: [Int]
    majorScaleIntervals = [2, 2, 1, 2, 2, 2, 1]

-- Function to convert a beat value to its corresponding duration
beatValueToDuration :: Int -> Dur
beatValueToDuration 1 = 1
beatValueToDuration 2 = 1/2
beatValueToDuration 4 = 1/4
beatValueToDuration 8 = 1/8

-- Function to parse a string into a time signature. Returns Nothing if parsing fails
parseTimeSignature :: String -> Maybe TimeSignature
parseTimeSignature str = case map T.unpack . T.splitOn (T.pack "/") . T.pack $ str of
    [num, denom] -> Just (read num, read denom)
    _            -> Nothing




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

  -- Takes a string and tries to parse it into an integer representing the octave
  -- If the string is parsed into a number then by nothing else, that number is returned, but if not there will be an error
  where
    readOctave str = 
      case reads str :: [(Int, String)] of
        [(val, "")] -> val
        _           -> error $ "Invalid octave: " ++ str

--   A simple handler function that delegates the request to a 'getHomeHandler'. It is assumed that 'getHomeHandler'
--   is defined within the 'HasHomeHandler' typeclass, which the 'master' type must be an instance of.
--   This function is typically used in web applications using the Yesod framework to handle the default route
--   associated with the home page. It returns an 'Html' response to be rendered by the browser.
getHomeR :: HasHomeHandler master => HandlerFor master Html
getHomeR = getHomeHandler