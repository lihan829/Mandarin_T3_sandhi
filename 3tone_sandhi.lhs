> {-|
> Compling2 Project: A String Transducer of Mandarin Third Tone sandhi process
> Li Han May 20
>
> This code is based on our homework exercise ``gdfst" but I changed some part in it.
> The project aims to take a string of underlying Mandarin tone and present the surface form after the third tone process.
> The Third tone sandhi process is: /3+3/ |-> [23]
> -}



The first step is to construct a DFST. This defition is from the Jeff's homework without any change.

> type Delta q b = q -> b -> q
> type Omega q b v = q -> b -> v
> type Final q v = q -> v

> data DFST q b v = T { states :: [q]
>                     , sigma  :: [b]
>                     , start  :: q
>                     , prefix :: v
>                     , delta  :: (Delta q b)
>                     , omega  :: (Omega q b v)
>                     , final  :: (Final q v)
>                     }



As what we have in the previou homework, there will be a process function 

> process :: DFST q b [a] -> (q,[a]) -> [b] -> (q,[a])
> process dfst (state0, prefix) [] = (state0, prefix ++ (final dfst state0))
> process dfst (state0, prefix) (x:xs) = process dfst (((delta dfst) state0 x), prefix ++ (omega dfst state0 x)) xs


The transducer function is not the exactly same as the one from the previous homework.
Unlike post nasal voicing, the third tone sandhi process is a regressive assimilation. 
For this case, the transducer first reverse the string and then process and finally reverse the output again. 
(I've tried to avoid using reverse function or add an empty string omega output, but it didn't work out)

> transduce :: DFST q b [a] -> [b] -> [a]
> transduce dfst [] = []
> transduce dfst xs  = reverse (snd (process dfst (start dfst, prefix dfst) (reverse xs)))


The next step is to define a transducer that changes 33 tone into 23.
In this transducer, the alphabet is four tones in Chinese Mandarin 1,2,3,4. Some papers uses H,L,R,F but there's no big differece. 

> alphabet :: [Char]
> alphabet = "1234"

> tSD :: DFST Int Char [Char]
> tSD = T { states = [0,1]
>         , sigma  = alphabet
>         , start  = 0
>         , prefix = ""
>         , delta  = dSD
>         , omega  = oSD
>         , final  = (\_ -> "")
>         }

The delta function will have two states: if the process function encounters a 3 tone, it goes to q1. Otherwise it stays in the q0.
In the q1, if a second 3 tone occurs, it goes back to the q0; otherwise it stays in the q0

> dSD :: Delta Int Char
> dSD 0 b | b == '3'  = 1
>         | otherwise = 0
> dSD 1 b | b == '3'     = 1
>         | otherwise = 0

The omega function aims to transduce the second 3 tone into a 2 tone.
NOTE: since the whole input string has been reversed, so the 3 tone that gets changed is actually the first 3 tone in the UR.
This will not change the result because the output of transduce will be reversed again and will bring strings back to the orginal order. 

> oSD :: Omega Int Char [Char]
> oSD 0 b  = [b]
> oSD 1 b | b == '3'      = "2"
>         | otherwise     = [b]

This part aims to make this code more user-friendly (hopefully..) To use this code, a user will:
1. load this file in GHCI
2. input ``sandhi" and type any tone or tones combinations after the prompt "Please input any tones (1,2,3,4), for example, 123"

> sandhi :: IO ()
> sandhi = do
>      putStrLn "Please input any tones (1,2,3,4), for example, 123"
>      tones <- getLine
>      putStrLn $ "The surface form of " ++ tones ++ " is " ++ (transduce tSD tones)

In addition, there is also a set of tones for testing

> testTones :: [String]
> testTones = ["3","33","333","3323","1231"]

map (transduce tSD) testTones   == ["3","23","223","2323","1231"]
