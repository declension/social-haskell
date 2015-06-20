# social-haskell

[![Build Status](https://drone.io/github.com/declension/social-haskell/status.png)](https://drone.io/github.com/declension/social-haskell/latest)


### Background
Essentially, this is an exploratory piece / 'remix' using the same specification as the original CM challenge,
as [solved in Java 8 here](https://github.com/declension/cm-challenge).

Unsurpisingly, this brought to my attention the real-world differences between writing a stateful,
modular app with I/O in modern Java and modern (but amateur) Haskell.. 

### Tech
* Haskell + various standard libraries
* Cabal for building.
* The awesomeness of [Parsec](https://hackage.haskell.org/package/parsec) for combinatorial parsing... and free error diagnosis! (kinda).
* The very promising seeming [Hspec](http://hspec.github.io/) for Behavioural tests.
* Drone.io as a buildchain to run the build and test.

### TODO
 * Use a State monad to handle the parse-execute loop state?
 * Use Applicative style within testing? Or something at least... 
 the current skeleton integration tests could undoubtedly be made much more concise.

### License
For completeness, this should be included I guess, but hasn't. 
Until something more formal (and permissive) strikes me, all code is &copy; Declension Systems Ltd, 2015.
