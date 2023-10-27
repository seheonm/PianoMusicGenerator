{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
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




playMusic :: Double -> TimeSignature -> Music Pitch -> IO ()
playMusic bpm timeSignature m = play $ tempo (toRational (bpm / 120)) m

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

noteValueToDuration :: Int -> Dur
noteValueToDuration 1 = wn
noteValueToDuration 2 = hn
noteValueToDuration 4 = qn
noteValueToDuration 8 = en
noteValueToDuration 16 = sn
noteValueToDuration _ = error "Unsupported note value"

getGenerateMusicR :: Handler Html
getGenerateMusicR = do
    mbKeySignature <- lookupGetParam "keySignature"
    mbBpm <- lookupGetParam "bpm"
    
    mbTimeSignature <- lookupGetParam "timeSignature"
    let timeSignature = fromMaybe (4,4) (mbTimeSignature >>= parseTimeSignature . T.unpack)
        numMeasures = 2

        keySignature = fromMaybe "C" (fmap T.unpack mbKeySignature)
        bpm = fromMaybe 120 (mbBpm >>= readMaybe . T.unpack)  -- Default to 120 if not present or invalid
    
    case keySignatureToPitch keySignature of
        Just pitch -> do
            music <- liftIO $ generateMusic timeSignature numMeasures pitch
            _ <- liftIO $ forkIO $ playMusic (fromIntegral bpm) timeSignature music
            return [shamlet|Music Played|]
        Nothing -> return [shamlet|Invalid Key Signature|]
    


generateMusic :: TimeSignature -> Int -> Pitch -> IO (Music Pitch)
generateMusic (numBeats, beatValue) numMeasures pitch@(pc, oct) = do
  let dur = noteValueToDuration beatValue
      scale = majorScale pitch  
      generateRandomPitch :: IO Pitch
      generateRandomPitch = do
        index <- randomRIO (0, length scale - 1)
        return $ scale !! index
      generateMeasure :: IO (Music Pitch)
      generateMeasure = do
        notes <- mapM (\_ -> generateRandomPitch >>= \p -> return $ note dur p) [1..numBeats]
        return $ line notes
  measures <- mapM (\_ -> generateMeasure) [1..numMeasures]
  return $ line measures


type TimeSignature = (Int, Int)

parseTimeSignature :: String -> Maybe TimeSignature
parseTimeSignature str = case map T.unpack . T.splitOn (T.pack "/") . T.pack $ str of
    [num, denom] -> Just (read num, read denom)
    _            -> Nothing

majorScale :: Pitch -> [Pitch]
majorScale (p, o) = take 8 $ iterate (nextPitch) (p, o)
  where
    nextPitch :: Pitch -> Pitch
    nextPitch (p, o) = case p of
      B  -> (C, o + 1)
      E  -> (F, o)
      _  -> (succ p, o)

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