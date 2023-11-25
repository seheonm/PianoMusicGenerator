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


-- Takes a beats per minute (bpm), a time signature, and a music piece
playMusic :: Double -> TimeSignature -> Music Pitch -> IO ()
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

-- Generates a melody and a bass line, then combines them
generateMusic :: TimeSignature -> Int -> Pitch -> IO (Music Pitch)
generateMusic ts numMeasures pitch = do
    melodyLine <- generateMelodyLine ts numMeasures pitch  -- Melody line
    bassLine <- generateBassLine ts numMeasures (lowerOctave pitch)  -- Bass line an octave lower
    return (melodyLine :=: bassLine)

generateBassLine :: TimeSignature -> Int -> Pitch -> IO (Music Pitch)
generateBassLine ts numMeasures tonic = do
    let chords = chordProgression tonic
    measures <- mapM (generateBaroqueMeasureForBass ts chords) [1..numMeasures]
    return $ line measures

generateBaroqueMeasureForBass :: TimeSignature -> [Chord] -> Int -> IO (Music Pitch)
generateBaroqueMeasureForBass (numBeats, beatValue) chords measureNumber = do
    let chord = chords !! ((measureNumber - 1) `mod` length chords)
        maxMeasureDur = fromIntegral numBeats * beatValueToDuration beatValue
    generateSingleMeasure maxMeasureDur chord

generateSingleMeasure :: Dur -> Chord -> IO (Music Pitch)
generateSingleMeasure maxDur chord = go maxDur (rest 0)
  where
    go remainingDur acc
      | remainingDur <= 0 = return acc
      | otherwise = do
          pitch <- generateNoteFromChord chord
          dur <- chooseDur
          let actualDur = min dur remainingDur
          let newNote = note actualDur pitch
          go (remainingDur - actualDur) (acc :+: newNote)
    
chooseDur :: IO Dur
chooseDur = do
    isWholeNote <- randomIO  -- Randomly decide whether to use a whole note
    return $ if isWholeNote then wn else hn

generateRandomPitch :: [Pitch] -> IO Pitch
generateRandomPitch scale = do
    index <- randomRIO (0, length scale - 1)
    return $ scale !! index

beatValueToDuration :: Int -> Dur
beatValueToDuration 1 = wn
beatValueToDuration 2 = hn
beatValueToDuration 4 = qn
beatValueToDuration 8 = en
beatValueToDuration _ = error "Unsupported beat value"

-- Helper function to generate a major scale based on a given tonic
majorScale :: Pitch -> [Pitch]
majorScale (p, o) = take 8 $ iterate nextPitch (p, o)
  where
    nextPitch (p, o) = case p of
      B  -> (C, o + 1)
      E  -> (F, o)
      _  -> (succ p, o)

-- Lower the pitch by one octave
lowerOctave :: Pitch -> Pitch
lowerOctave p = pitch (absPitch p - 12)

-- type for a time signature represented as a pair of intergers
type TimeSignature = (Int, Int)

-- Parses a string to extract a time signature as a pair of integers
-- Returns Nothing if the parsing fails
parseTimeSignature :: String -> Maybe TimeSignature
parseTimeSignature str = case map T.unpack . T.splitOn (T.pack "/") . T.pack $ str of
    [num, denom] -> Just (read num, read denom)
    _            -> Nothing

type Chord = [Pitch]

chordProgression :: Pitch -> [Chord]
chordProgression tonic = [iChord, ivChord, vChord, iChord]
  where
    scale = majorScale tonic
    iChord = take 3 scale  -- Tonic chord
    ivChord = take 3 $ drop 3 scale  -- Subdominant chord
    vChord = take 3 $ drop 4 scale  -- Dominant chord

    
generateNoteFromChord :: Chord -> IO Pitch
generateNoteFromChord chord = do
    index <- randomRIO (0, length chord - 1)
    return $ chord !! index

generateMelodyLine :: TimeSignature -> Int -> Pitch -> IO (Music Pitch)
generateMelodyLine (numBeats, beatValue) numMeasures tonic = do
    let chords = chordProgression tonic
        melodyDur = beatValueToDuration beatValue  -- Duration of each beat
    measures <- mapM (\measureNum -> generateMeasureForMelody (numBeats, melodyDur) (chords !! ((measureNum - 1) `mod` length chords))) [1..numMeasures]
    return $ line measures

generateMeasureForMelody :: (Int, Dur) -> Chord -> IO (Music Pitch)
generateMeasureForMelody (numBeats, beatDur) chord = do
    notes <- replicateM numBeats (generateNoteFromChordForMelody chord >>= \p -> return $ note beatDur p)
    return $ line notes


generateBaroqueMeasureForMelody :: TimeSignature -> [Chord] -> Int -> IO (Music Pitch)
generateBaroqueMeasureForMelody (numBeats, _) chords measureNumber = do
    let chord = chords !! ((measureNumber - 1) `mod` length chords)
    pitches <- mapM (\_ -> generateNoteFromChordForMelody chord) [1..numBeats]
    let notes = map (\p -> note en p) pitches  -- Convert each pitch to an eighth note
    return $ line notes

generateNoteFromChordForMelody :: Chord -> IO Pitch
generateNoteFromChordForMelody chord = do
    index <- randomRIO (0, length chord - 1)
    return $ chord !! index


-- Generate counterpoint by harmonizing melody and bass
generateCounterpoint :: Music Pitch -> IO (Music Pitch)
generateCounterpoint melody = do
    -- Extract pitches and durations from the melody
    let (melodyPitches, melodyDurations) = unzip $ extractMelody melody
    -- Generate bass pitches based on melody pitches
    bassPitches <- mapM harmonizeWithMelody melodyPitches
    -- Combine bass pitches with melody durations to create bass line
    let bassLine = zipWith note melodyDurations bassPitches
    return $ line bassLine

-- Function to harmonize bass note with melody note
harmonizeWithMelody :: Pitch -> IO Pitch
harmonizeWithMelody melodyPitch = do
    interval <- randomRIO (1, 3) :: IO Int
    let bassPitch = case interval of
                      1 -> transposePitch (-12) melodyPitch  -- Octave below
                      2 -> transposePitch (-7) melodyPitch   -- Perfect fifth below
                      3 -> transposePitch (-3) melodyPitch   -- Minor third below
    return bassPitch

-- Extended chord progression function
extendedChordProgression :: Pitch -> [Chord]
extendedChordProgression tonic = 
    [iChord, iiChord, iiiChord, ivChord, vChord, viChord, viiDimChord, iChord]
  where
    scale = majorScale tonic
    -- Basic chords
    iChord = take 3 scale  -- Tonic chord
    ivChord = take 3 $ drop 3 scale  -- Subdominant chord
    vChord = take 3 $ drop 4 scale  -- Dominant chord

    -- Extended chords
    iiChord = take 3 $ drop 1 scale  -- Supertonic chord
    iiiChord = take 3 $ drop 2 scale  -- Mediant chord
    viChord = take 3 $ drop 5 scale  -- Submediant chord
    viiDimChord = take 3 $ drop 6 scale  -- Leading tone chord, diminished

transposePitch :: Int -> Pitch -> Pitch
transposePitch n (pc, oct) = pitch (absPitch (pc, oct) + n)

extractMelody :: Music Pitch -> [(Pitch, Dur)]
extractMelody m = case m of
    Prim (Note d p) -> [(p, d)]
    Prim (Rest d)   -> []
    m1 :+: m2       -> extractMelody m1 ++ extractMelody m2
    m1 :=: m2       -> merge (extractMelody m1) (extractMelody m2)
    Modify _ m'     -> extractMelody m'
    where
        merge :: [(Pitch, Dur)] -> [(Pitch, Dur)] -> [(Pitch, Dur)]
        merge xs [] = xs
        merge [] ys = ys
        merge ((p1, d1):xs) ((p2, d2):ys)
            | d1 < d2   = (p1, d1) : merge xs ((p2, d2 - d1):ys)
            | d1 > d2   = (p2, d2) : merge ((p1, d1 - d2):xs) ys
            | otherwise = (p1, d1) : merge xs ys




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

--   A simple handler function that delegates the request to a 'getHomeHandler'. It is assumed that 'getHomeHandler'
--   is defined within the 'HasHomeHandler' typeclass, which the 'master' type must be an instance of.
--   This function is typically used in web applications using the Yesod framework to handle the default route
--   associated with the home page. It returns an 'Html' response to be rendered by the browser.
getHomeR :: HasHomeHandler master => HandlerFor master Html
getHomeR = getHomeHandler