# cabal dependencies

Megaparsec
pretty-show
terminal-size
System.Directory
System.Exit
Control.Monad.State.Strict
Contorl.Monad.Reader

## How to use

1. cd src/syphonDesigns/boilerplate
2. yarn (downloads and initializes Syphon depencies)
3. Place programs in src/Source.sy, optionally also a ".sunit" file for unit tests
4. cd to src folder, ghci Main
5. main (parser, type system and transpiler will run, outputting App.js into the boilerplate direcoty)
6. Go to src/syphonDesigns/boilerplate
7. yarn start (for dev mode) or yarn build (to build the program)
