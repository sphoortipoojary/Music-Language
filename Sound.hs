module Sound where
import Data.WAVE
import Data.Int (Int32)
import Data.List.Split (splitOn)


--Samples per second
samplesPS = 16000
bitrate = 32

--Wave file header
header = WAVEHeader 1 samplesPS bitrate Nothing

--Sine wav generator
sound :: Double  -- | Frequency
      -> Int -- | Samples per second
      -> Double -- | Length in seconds
      -> Int32 -- | Volume
      -> [Int32]
sound freq samples len volume = take (round $ len * (fromIntegral samples)) $
                         map (round . (* fromIntegral volume)) $
                         map sin [0.0, (freq * 2 * pi / (fromIntegral samples))..]

--Wrapping elements in the list
samples :: Double -> Double -> [[Int32]]
samples n d = map (:[]) $ sound n samplesPS d (maxBound `div` 2)


--waveData  n d = WAVE header (samples n d)

--Creating wave file
makeWavFile :: WAVE -> IO ()
makeWavFile wav = putWAVEFile "temp.wav" wav
