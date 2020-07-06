-- help from video: https://www.youtube.com/watch?v=FYTZkE5BZ-0&t=326s
module MusicBase where
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Foldable
import System.Process
import Data.Char
import Data.List
-- Music is a function of time to air movement over some specified time
data Music = Music (Float -> Float) Float

type Wave = (Float -> Float -> Music)

sampleRate :: Float
sampleRate = 48000.0
tempFile = "/tmp/.TEMP_HASKELL_FILE.raw"

play :: Music -> IO()
play m = do
    saveRaw m tempFile
    runCommand $ "play -b 32 -e floating-point -r " ++ (show .round $ sampleRate) ++ " " ++ tempFile
    return ()


saveRaw :: Music -> String -> IO ()
saveRaw music file = B.writeFile file $ toByteString music

save :: Music -> String -> IO ()
save music filename = 
    let
        rate = show . round $ sampleRate
-- sox -r 48000 -b 32 -e floating-point test.raw -r 48k test.wav
        soxCommand = "sox " ++ "-r "++ rate ++ " -c 1 -b 32 -e floating-point " ++ tempFile ++" -r " ++ rate ++ " "++ filename
    in
    do
        saveRaw music tempFile    
        runCommand $ soxCommand 
        return ()

wait :: Float -> Music
wait t = Music (\_->0) t

square :: Wave
square freq time = Music (\t -> sum . take lim . map (f t) $ [0..]) time
    where
        lim = 10
        f t n = (sin $ freq*t*2*pi*(2*n+1))/(2*n+1)

sine :: Wave
sine freq time = Music (\t -> sin (t*2*pi*freq)) time

getTime :: Music -> Float
getTime (Music _ t) = t

addThen :: Music -> Music -> Music
addThen (Music a t1) (Music b t2) =
    Music 
    (\t -> if t < t1 then
            a t
        else 
            (a t) + (b $ t - t1))
    $ t1 + t2

andThen :: Music -> Music -> Music
andThen m1 m2 =
    addThen (noContinue m1) m2

strike :: Music -> Music
strike m =
    let
        time = getTime m
    in
      (Music (\t -> exp $ -4*time*t) time) * m

pulse :: Music -> Music
pulse m@(Music f time) =
    noContinue $ (sine (1/(2*time)) time) * m

noContinue :: Music -> Music
noContinue m@(Music f time) = 
    Music 
    (\t -> if t < time then
                f t
            else 
                0)
    time

constMus :: Float -> Music
constMus i = Music (\t -> i) 0

average :: [Music] -> Music
average ms = (/(fromIntegral . length $ ms)) @. (sum ms)

(@.) :: (Float -> Float) -> Music -> Music
(@.) f (Music f2 time) = Music  (f . f2) time
infixr 0 @.

(.@) :: Music -> (Float -> Float) -> Music
(.@) (Music f1 time) f2 = Music ( f1 . f2) time
infixr 0 .@

(.@.) :: Music -> Music -> Music
(.@.) (Music f1 time) (Music f2 _) = Music ( f1 . f2 ) time
infixr 0 .@.

toByteString (Music f time) = 
    let
        maxM = 0.9999
        minM = -1*maxM
        bound x | x > maxM = maxM
                | x < minM = minM
                | otherwise = x
    in
        B.toLazyByteString $ fold $ map (B.floatLE . bound . f) [0.0,1/sampleRate .. time]

instance Num Music where
    (+) (Music a time1) (Music b time2) = Music (\t -> (a t) + (b t)) $ max time1 time2
    (*) (Music a time1) (Music b time2) = Music (\t -> (a t) * (b t)) $ max time1 time2
    negate m = negate @. m
    abs m = abs @. m
    fromInteger = constMus . fromInteger
    signum (Music f t) = constMus $ signum $ f 0

instance Show Music where
    show = show . toByteString

barsToMusic :: (a -> Float -> Music) -> [([a],Float)] -> Music
barsToMusic f bars = 
    foldl (addThen) (constMus 0) .
    map (\(c,time) -> average . map (\n -> f n time) $ c) $
    bars

