# MusicLib


A music lib written in haskell

# Expectations 
* I want to explore and document the building blocks of sound up from basic sin waves to a song
* The project will be run on linux as this is what I am most comfortable with.
* This is a research project and performance is not the focus.

# What was learned
## What is sound
Sound is a phenomenon in which pressure disturbances propagate through an elastic material medium.<sup>[Wikipedia](https://en.wikipedia.org/wiki/Sound)</sup> These pressure disturbances propagate through air as waves of pressure. The range of human hearing is 20Hz to 20,000 Hz<sup>[Wikipedia](https://en.wikipedia.org/wiki/Hearing_range#Humans)</sup>. 1Hz is 1 wave of pressure per second (why we use the symbol "Hz" to represent this unit is a question for the etymologists and will not be covered here).
To model these waves of pressure we will use sine waves (the same ones from math class). Our first project is to model a wave at 1Hz. If you remember from school the period (the time it takes to repeat peak to peak) of $sin(t)$ wave is 2π (where t represents time). To change the period to 1Hz instead of 2πHz we change $sin(t)$ to $sin(t2\pi)$. In fact for any desired frequency $f$Hz the corresponding sine wave is $sin(t2\pi f)$.
### Implementation
#### boiler plate
* First we decide how we will output our sound. The easiest output for us to use is a [raw audio file](https://en.wikipedia.org/wiki/Raw_audio_format). all other formats require us to care a lot more about the structure of a sound file. Raw audio files are just a list of numbers that represent the pressure of our wave at a time. The numbers in our list are bounded by 1 and -1, That is to say no number in our list can be grater than 1 nor less than -1. The one downside of this approach is that the time step between our numbers is not specified in our file. This means that whenever we play our file we must provide our audio players our musics sample rate.
We will be using a Linux tool called [sox](https://linux.die.net/man/1/sox) to save and play our audio files. Sox provides terminal commands that we can use like this.
```bash
	# convers file1 (raw) to file2 (wav) 
        sox -r $samplerate -c 1 -b 32 -e floating-point " $file1 -r $samplerate $file2
	# plays file at samplerate
    play -b 32 -e floating-point -r $sampleRate $file
	```
#### Haskell	
* Now we get to define our types. I have taken the bold philosophical stance that music is some sound played over some time. Feel free to disagree with me, but please keep reading. So we define music as having these two components, first the sound represented by a wave function and secondly the time this sound will be played for. A wave is a function that takes in a frequency to be played at and a time to be played for and returns music.
	```haskell
data Music = Music (Float -> Float) Float
type Wave = (Float -> Float -> Music)
	```
With these types we can define our sine wave
	```haskell
sine :: Wave
sine freq time = Music (\t -> sin (t*2*pi*freq)) time
	```
We can also define some other classic waves
	```haskell

square :: Wave
square freq time = Music (\t -> sum . take lim . map (f t) $ [0..]) time
    where
        lim = 10
        f t n = (sin $ freq*t*2*pi*(2*n+1))/(2*n+1)

	```
Now it would be helpful if we could play some music $m1$ and then play some other music $m2$ after it.
	```haskell
--- play m1 first and then m2
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

--- Takes music and returns music that ends after its specified time
noContinue :: Music -> Music
noContinue m@(Music f time) = 
    Music 
    (\t -> if t < time then
                f t
            else 
                0)
    time


	```
Deffining music and waves like functions means we can change the volume by multiplying by a number (i.e. for some wave $w$ $2*w$ is twice as loud and $\frac{1}{2}*w$ is half as lound). This property lets us define these helpful functions.
	```haskell

-- returns m with m's volume decreasing exponetialy. 
strike :: Music -> Music
strike m =
    let
        time = getTime m
    in
      (Music (\t -> exp $ -4*time*t) time) * m
-- retunrs m with m's volume being pulsed once
pulse :: Music -> Music
pulse m@(Music _f time) =
    noContinue $ (sine (1/(2*time)) time) * m

	```
By now you may have noticed that we dont have the ability to play any of our music yet.
@TODO@ finish
## What is a musical note

# Running the project
## Dependencies
* Haskell
* sox
## Build and Run
* With your volume on you should be able to hear a (poor) rendition of marry had a little lamb.
```
> make
Build profile: -w ghc-8.10.7 -O1
In order, the following will be built (use -v for more details):
 - MusicLib-0.1.0.0 (lib) (file README.md changed)
Preprocessing library for MusicLib-0.1.0.0..
Building library for MusicLib-0.1.0.0..
> cabal repl
*MusicBase> import Main
*MusicBase Main> play song

/tmp/.TEMP_HASKELL_FILE.raw:

 File Size: 2.58M     Bit Rate: 1.54M
  Encoding: F.P. PCM      
  Channels: 1 @ 25-bit   
Samplerate: 48000Hz      
Replaygain: off         
  Duration: 00:00:13.46  

In:100%  00:00:13.46 [00:00:00.00] Out:646k  [      |      ]        Clip:0    
Done.

*MusicBase Main> :quit
Leaving GHCi.
>
```
