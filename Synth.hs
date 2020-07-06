module Synth where

import MusicBase
import Notes

type Synth = (Wave -> Float -> Float -> Music)

maxPitch = 20000.0
minPitch = 20.0

peano :: Synth
peano wave fq len =
    let
        merge :: Float -> Music -> Music
        merge c m = (c*) @. m
    in
    strike $ (/2) @. sum $ zipWith merge 
		[0.25,0.5,0.125,0.125]
		[wave (fq*n) len|n<-[1..10]]
