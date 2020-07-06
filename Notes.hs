module Notes where

{- Notes 
 - Sharps have an 's' after them
 - Flats  have a  'b' after Them
 -}
data NotePre = C | Cs | Db | D | Ds | Eb | E | F | Fs | Gb | G | Gs | Ab | A | As | Bb | B | N
--             deriveing Eq

halfStepsFromA C  = -9
halfStepsFromA Cs = -8
halfStepsFromA Db = -8
halfStepsFromA D  = -7
halfStepsFromA Ds = -6
halfStepsFromA Eb = -6
halfStepsFromA E  = -5
halfStepsFromA F  = -4
halfStepsFromA Fs = -3
halfStepsFromA Gb = -3
halfStepsFromA G  = -2
halfStepsFromA Gs = -1
halfStepsFromA Ab = -1
halfStepsFromA A  =  0
halfStepsFromA As =  1
halfStepsFromA Bb =  1
halfStepsFromA B  =  2
halfStepsFromA N  =  0

type Cord = [Note]
data Note = Note NotePre Int
-- DataFrom: https://pages.mtu.edu/~suits/NoteFreqCalcs.html
freq :: Note -> Float
freq (Note N _) = 0
freq (Note A 4) = 440
freq (Note a n) = (freq $ Note A 4)*(2.0**(n'/12))
        where  
              n' :: Float 
              n' = fromIntegral $ (n-4)*12+(halfStepsFromA a)

rest = Note N 0
