# Syphon

A functional, minimalist MVU based language for creating Desktop GUI apps powered by Electron and React. Heavily influenced by Haskell and Elm, with some new quirks mixed in.

## Features

- **Zero boilerplate** 🚫

👾 Just plug 'n' play! No main function, only update and view. Ideal for smaller projects. Example of a complete program:
	
```
--IncDecExample.sy

import Color as C

alias State = Int

type Event = Inc | Dec

init :: State
init = 0

update :: State -> Event -> State
update s e = match e with 
    Inc -> s + 1
    Dec -> s - 1

-- Multiline syntax sugar: "[]<<" can be read as "populate this list with all the expressions on the lines below with one level of indentation greater"
view :: State -> Widget
view s = Column []<<
    Button (Text "+") Inc
    --Optional structures, that can be omitted as arguments and fields take on default values unless overridden
    Text #{bold = True, color = C.grey {-other fields not overridden e.g. size = 10 -}} (toString s)
    Button (Text "-") Dec
```

Minimalist syntax and zero boilerplate means programs are generally very small and easy to reason about. Here is a simple calculator program, the logic should be clear:

```
--Calculator.sy

type State = {
    display :: String 
    operator :: Operator 
    operand1 :: Int
}

alias Operator = Int -> Int -> Int

type Event
    = Clear
    | Eq
    | OpPressed Operator
    | DigitPressed Int

init :: State
init = State {display = "", operator = (+), operand = 0}

update :: State -> Event -> State
update s e = match e with
    Clear -> init
    Eq -> State (toString $ s.operator s.operand1 $ parseInt display) (+) 0
    OpPressed op -> State "" op (parseInt s.display)
    DigitPressed i -> {s | display = s.display ++ (toString i)}
    
view :: State -> Element 
view s = Column []<<
    Text s.display
    Row $ (map mkDigitButton [7..9]) ++ (Button "x" (OpPress (*)))
    Row $ (map mkDigitButton [4..6]) ++ (Button "/" (OpPress (/)))
    Row $ (map mkDigitButton [1..3]) ++ (Button "-" (OpPress (-)))
    Row [mkDigitButton 0, Button "+" (OpPressed (+)), Button "AC" Clear, Button "=" Eq]
    where
        mkDigitButton i = Button (toString i) (DigitPressed i)
```

- **Type Inference**

	-Type system based on Hindley-Milner Algorithm W

- **Helpful static analysis** 🔎

- **Cross platform** 💻

	- CLI transpiler written in Haskell 
	- Produces React based Electron apps
	
- **Functional purity** 😇

	- Explicit state management improves referential transparency and reduces the likelihood of illegal states
	- Side effects are only permitted from view and update
	- Ideal environment for beginners to learn functional programming applied to GUIs
- **Quirky syntax features** 🍭

- **Performance** 🚀

	- Widgets automatically detect if they need to be re-rendered based on the part of the state they display and checking it for changes
	- Harness the power of the React Virtual DOM algorithm

- **A labour of love** 💜

	- My final year project at The University of Southampton. My dissertation on Syphon can be found **here**
	- Currently searching for a grad scheme in the UK and open to offers of employment :)

## Usage

- Clone or download (**insert link**) the repo:
- cd into the ```syphon``` directory
- Run ```./make``` and the makefile will call ghc to produce the ```syphon``` binary
- Run ```./syphon``` and the interactive CLI tool will start

## Etymology

- The MVU architectural pattern is based on a unidirectional flow of data. From Wikipedia:

	> A siphon (from Ancient Greek: σίφων, "pipe, tube", also spelled nonetymologically syphon) is any of a wide variety of devices that involve the flow of liquids through tubes

## Future goals

- An alternative implementation targetting a different platform such as creating **Flutter smartphone apps**
	- The syntax is designed to be fairly general on purpose with this future goal in mind!
- Lazy evaluation language primitive
