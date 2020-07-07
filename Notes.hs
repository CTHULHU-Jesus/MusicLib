module Notes where

{- Notes 
 - Sharps have an 's' after them
 - Flats  have a  'b' after Them
 -}
data NotePre = C | Cs | Db | D | Ds | Eb | E | F | Fs | Gb | G | Gs | Ab | A | As | Bb | B | N
--             deriveing Eq

instance Eq NotePre where
    (==) C  C  = True
    (==) Cs Cs = True
    (==) Cs Db = True
    (==) Db Cs = True
    (==) Db Db = True
    (==) D  D  = True
    (==) Ds Ds = True
    (==) Ds Eb = True
    (==) Eb Ds = True
    (==) Eb Eb = True
    (==) E  E  = True
    (==) F  F  = True
    (==) Fs Fs = True
    (==) Fs Gb = True
    (==) Gb Fs = True
    (==) Gb Gb = True
    (==) G  G  = True
    (==) Gs Gs = True
    (==) Gs Ab = True
    (==) Ab Gs = True
    (==) Ab Ab = True
    (==) A  A  = True
    (==) As As = True
    (==) As Bb = True
    (==) Bb As = True
    (==) Bb Bb = True
    (==) B  B  = True
    (==) N  N  = True
    (==) _  _  = False

instance Enum NotePre where
    fromEnum n | n == C  = 0
               | n == Cs = 1
                 | n == D  = 2
               | n == Ds = 3
               | n == E  = 4
               | n == F  = 5
               | n == Fs = 6
               | n == G  = 7
               | n == Gs = 8
               | n == A  = 9
               | n == As = 10
               | n == B  = 11
               | n == N  = 0

    toEnum i = case i `mod` 12 of
        0  -> C
        1  -> Cs
        2  -> D
        3  -> Ds
        4  -> E
        5  -> F
        6  -> Fs
        7  -> G
        8  -> Gs
        9  -> A
        10 -> As
        11 -> B

halfStepsFromA n  = (fromEnum n) - (fromEnum A)
-- halfStepsFromA C  = -9
-- halfStepsFromA Cs = -8
-- halfStepsFromA Db = -8
-- halfStepsFromA D  = -7
-- halfStepsFromA Ds = -6
-- halfStepsFromA Eb = -6
-- halfStepsFromA E  = -5
-- halfStepsFromA F  = -4
-- halfStepsFromA Fs = -3
-- halfStepsFromA Gb = -3
-- halfStepsFromA G  = -2
-- halfStepsFromA Gs = -1
-- halfStepsFromA Ab = -1
-- halfStepsFromA A  =  0
-- halfStepsFromA As =  1
-- halfStepsFromA Bb =  1
-- halfStepsFromA B  =  2
-- halfStepsFromA N  =   0

data Note = Note NotePre Int

instance Eq Note where
    (==) (Note n1 i1) (Note n2 i2) = (n1 == n2) && (i1 == i2)

instance Enum Note where
    fromEnum (Note n i) = (fromEnum n) + 12*i
    toEnum i = Note  (toEnum $ mod i 12) (i `div` 12)

-- DataFrom: https://pages.mtu.edu/~suits/NoteFreqCalcs.html
freq :: Note -> Float
freq (Note N _) = 0
freq (Note A 4) = 440
freq (Note a n) = (freq $ Note A 4)*(2.0**(n'/12))
        where  
              n' :: Float 
              n' = fromIntegral $ (n-4)*12+(halfStepsFromA a)

rest = Note N 0

majorScale :: Note -> Int -> Note
majorScale root@(Note n i) spec =
	let
		selector = spec `mod` 9
		whole :: Int -> Note -> Note
		whole i = foldl (\a b -> a . b) (\x->x) . take (2*i) $ [succ | x <- [0..]]
	in
		case selector of
			0 -> root
			1 -> whole 1 root
			2 -> whole 2 root
			3 -> succ . whole 2 $ root
			4 -> succ . whole 3 $ root
			5 -> succ . whole 4 $ root
			6 -> succ . whole 5 $ root
			7 -> whole 7 $ root
			8 -> succ . whole 7 $ root
