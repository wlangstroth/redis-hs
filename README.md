# redis-hs

A simple Redis driver in Haskell using Data.Text for consistent UTF8 handling.
Incomplete, and not for production use. To the best of my knowledge,
[hedis](http://hackage.haskell.org/package/hedis) is the best supported of redis
clients in Haskell.

## Install

    cabal install redis-hs

## Use

After installing, check out the `examples` directory, or look at the unit tests.

## Acknowledgements

Much  of  the  code  in  this  library is  inspired  by  the  work  of  Anders
Conbere   and  Alexander   Bogdanov   (author  of   the   [redis  library   on
Hackage](http://hackage.haskell.org/package/redis)).

The test suite was created with the kind help of Greg Collins and Doug
Beardsley (whose Snap Framework test suite format I followed).
