module Main where
import MusicLib
import MusicBase
import Notes
import Synth
-- cover of marry had a little lamb
bpm = 85
beat = bpm/60
half = beat/2
quat = beat/4
eath = beat/8
sixh = beat/16


rightHand :: [(Cord,Float)]
rightHand = 
    [([Note E 4],quat),
     ([Note D 4],quat),
     ([Note C 4],quat),
     ([Note D 4],quat),
     ([Note E 4],quat),
     ([Note E 4],quat),
     ([Note E 4],half),
     ([rest],half),
     ([Note D 4],quat),
     ([Note D 4],quat),
     ([Note D 4],half),
     ([rest],half),
     ([Note E 4],quat),
     ([Note G 4],quat),
     ([Note G 4],half),
     ([rest],half),
     ([Note E 4],quat),
     ([Note D 4],quat),
     ([Note C 4],quat),
     ([Note D 4],quat),
     ([Note E 4],quat),
     ([Note E 4],quat),
     ([Note E 4],quat),
     ([Note E 4],quat),
     ([Note D 4],quat),
     ([Note D 4],quat),
     ([Note E 4],quat),
     ([Note D 4],quat),
     ([Note C 4],beat)]

leftHand = 
    [([rest],beat),
     ([rest],beat),
     ([Note G 3, Note F 3], beat),
     ([Note G 3, Note D 3], beat),
     ([rest],beat),
     ([rest],beat),
     ([Note G 3,Note F 3], beat),
     ([Note G 3,Note D 3], beat)]

 
song :: Music
song = (*0.5) @. average . map (barsToMusic (\n t -> peano sine (freq n) t)) $ [rightHand,leftHand]

main :: IO ()
main = play song
