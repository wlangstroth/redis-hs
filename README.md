# redis-hs

A simple Redis driver in Haskell using Data.Text for consistent UTF8 handling.

## Install

    cabal install redis-hs

## Use

Make sure to include

    Database.Redis

and try the following:

    con <- connect localhost defaultPort
    _ <- select con 0
    _ <- itemSet con "greek" "ἐστίν"
    tester <- itemGet con "greek"
    putStrLn $ show tester

## Acknowledgements

Much  of  the  code  in  this  library is  inspired  by  the  work  of  Anders
Conbere   and  Alexander   Bogdanov   (author  of   the   [redis  library   on
Hackage](http://hackage.haskell.org/package/redis)).

The test suite was created with the kind help of Greg Collins and Doug
Beardsley (whose Snap Framework test suite format I followed).
