# Syphon

A functional, minimalist MVU based language for creating Desktop GUI apps powered by Electron and React. Heavily influenced by Haskell and Elm, with some new quirks mixed in.

## Features

- **Zero boilerplate** 🚫

👾 Just plug 'n' play! No main function, only update and view. Ideal for smaller projects. Example of a complete program:
	
```
alias State = Int

type Event = Inc | Dec

init :: State
init = 0

update :: State -> Event -> State
update s e = match e with
	Inc -> s + 1
	Dec -> s - 1

view :: State -> Widget
view s = Column []<<
	Button (Text "+") Inc
	Text (toString s)
	Button (Text "-") Dec
```

Minimalist syntax and zero boilerplate means programs are generally very small and easy to reason about. Here is a simple calculator program, the logic should be clear:

```
alias Operator = Int -> Int -> Int

type State = 
	{
	display :: String,
	operator :: Operator,
	operand1 :: Int
	}

type Event = Clear | Eq | OpPress Operator | Digit Int

--Use plus as a "dummy" opterator
init :: State
init = State "" (+) 0

update :: State -> Event -> State
update s e = match e with
	Clear -> init
	Eq -> State (calc s) (+) 0
	OpPress op -> State "" op $ parseInt s.display
	Digit i -> {s | display = append s.display (toString i)}
	where
		calc st = toString (st.operator st.operand1 (parseInt st.display))

view :: State -> Widget
view s = Column []<<
	Container #{dim = (275,35), bgColor = Grey} $ Text s.display
	Row $ (map mkDigitButton [7..9]) ++ [Button (Text "X") (OpPress (*))]
	Row $ (map mkDigitButton [4..6]) ++ [Button (Text "/") (OpPress (/))]
	Row $ (map mkDigitButton [1..3]) ++ [Button (Text "-") (OpPress (-))]
	Row [mkDigitButton 0, Button (Text "AC") Clear, Button (Text "=") Eq, 
		Button (Text "+") (OpPress (+))]
	where
		mkDigitButton i = Button (Text $ toString i) (Digit i)
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
