
module Music(
  Music(..),
  play,
  showParsedExp,
  runFile
)  where


import Sound
import Data.WAVE
import Data.Map (Map)
import Data.Int (Int32)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Control.Monad.Except

data Music = Melody Pitch Octave Duration
          | Sequence Music Music
          | Parallel Music Music

  deriving(Show)

data Pitch = C
           | Cs
           | Df
           | D
           | Ds
           | Ef
           | E
           | F
           | Fs
           | Gf
           | G
           | Gs
           | Af
           | A
           | As
           | Bf
           | B
           | S
  deriving(Eq,Ord,Show)

type Octave = Double
type Duration = Double

a4=440 --base note frequency in Hz
dur=0.5 --duration

--File parser
fileP :: GenParser Char st Music
fileP = do
    prog <- exprP
    eof
    return prog

--Line parser
exprP = do
    spaces
    e <- singlenote <|> exprP'
    spaces
    rest <- optionMaybe exprP
    return (case rest of
      Nothing  -> e
      Just e'  -> Sequence e e')

--Parse single whole note
singlenote = do
  t <- termP
  return t

--Parse '[]'
exprP' = do
    char '['
    t1 <- terP'
    char ']'
    return t1

--Parse inside '[]'
terP' = do
    t1    <- termP
    spaces
    rest1 <- optionMaybe terP'
    return (case rest1 of
	  Nothing  -> t1
	  Just t1' -> Sequence t1 t1')

--Parsing rest/note
termP = do
    ch  <-  termRest <|> termNote
    return ch

--Parsing Rest term
termRest = do
    ch <- oneOf "Ss"
    newline <|> char ' '
    return $ Melody (transOp (ch:[])) (0.0) (dur/2.0 )

--Parsing Note term
termNote = do
    ch <- many1 letter
    dig <- digit
    return $ Melody (transOp ch) (read [dig]::Double) (dur/2.0)

--Parsing pitch type
transOp s = case s of
      "C"  -> C
      "Cs" -> Cs
      "Df" -> Df
      "D"  -> D
      "Ds" -> Ds
      "Ef" -> Ef
      "E"  -> E
      "F"  -> F
      "Fs" -> Fs
      "Gf" -> Gf
      "G"  -> G
      "Gs" -> Gs
      "Af" -> Af
      "A"  -> A
      "As" -> As
      "Bf" -> Bf
      "B"  -> B
      "S"  -> S

--Add corresponding elements of 2 lists for parallel sounds
parallelListAdder::[[Int32]]->[[Int32]] ->[[Int32]]
parallelListAdder [] [] = []
parallelListAdder x [] = x
parallelListAdder [] x = x
parallelListAdder ((x:[]):xs) ((y:[]):ys) = ((x+y):[]):(foo xs ys)

--Creating list of sound data
makeList :: Double -> Double -> [[Int32]]
makeList n d = samples n d

--Create samples from parsed data
makeData (Melody p o d)= makeList (noteToFreq p o) d
makeData (Sequence m1 m2)= foldr (:) (makeData m2) (makeData m1)
makeData (Parallel m1 m2)= parallelListAdder (makeData m2) (makeData m1)

--Create wav file
play(Melody p o d)= makeWavFile(WAVE header (makeData(Melody p o d)))
play(Sequence m1 m2)= makeWavFile(WAVE header (makeData(Sequence m1 m2)))
play(Parallel m1 m2)=makeWavFile(WAVE header (makeData(Parallel m1 m2)))

--Use pitch and octave to calculate frequency
noteToFreq :: Pitch -> Octave -> Double
noteToFreq note octave = if (note == S) then 0.0
                         else
                             if octave >= -1 && octave < 10
                                  then if n /= 15.0
                                         then (1.0595 ** (n + (12.0 * (octave - 4.0)))) * a4
                                       else error $ "Invalid Note "
                             else error $ "Invalid octave: "
                                  where n = case note of
                                             C  -> -9.0
                                             Cs -> -8.0
                                             Df -> -8.0
                                             D  -> -7.0
                                             Ds -> -6.0
                                             Ef -> -6.0
                                             E  -> -5.0
                                             F  -> -4.0
                                             Fs -> -3.0
                                             Gf -> -3.0
                                             G  -> -2.0
                                             Gs -> -1.0
                                             Af -> -1.0
                                             A  -> 0.0
                                             As -> 1.0
                                             Bf -> 1.0
                                             B  -> 2.0
                                             _  -> 15.0

--Print parsed expression
showParsedExp fileName = do
   p <- parseFromFile fileP fileName
   case p of
     Left parseErr -> print parseErr
     Right exp -> print exp

--Calls play function to play melody
runFile fileName = do
   p <- parseFromFile fileP fileName
   case p of
     Left parseErr -> print parseErr
     Right exp -> play exp


main :: IO ()
main = do
play (Melody Cs 4 1)    --plays Cs 4 for 1 sec
--play (Sequence (Melody Cs 4 1) (Melody E 4 1))  --plays Cs4 and E4 seq
--play (Sequence (Melody C 4 1) (Sequence (Melody D 4 1) (Melody E 4 1)))   --Complex seq
--play (Parallel (Melody C 4 1) (Melody E 4 1))  --plays C4 and E4 parallely
