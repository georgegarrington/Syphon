import React, { useEffect } from "react"
import {Record} from "immutable"
import {Nothing, Just, randomGenBetween,$listGen, mkRandomGen, $error} from "./lib/Hardwired/Core"
import {_Column,_Row} from "./lib/GUI/Layout"
import {_Button} from "./lib/GUI/Button"
import {saveIcon, explosion, mine} from "./lib/GUI/Icons"
import {_Panel,_Container,_Graphic} from "./lib/GUI/Misc"
export const Context = React.createContext()

import * as L from "./lib/Data/List"

class State extends Record({grid:undefined,bombCoords:undefined,gameState:undefined,gen:undefined}){}

const _State = grid => bombCoords => gameState => gen => 
	new State({grid: grid, bombCoords: bombCoords, gameState: gameState, gen: gen})

class Hidden {}
class Revealed {constructor(arg0){this.val0=arg0}}
class Flagged {}
class Exploded {}
class Playing {}
class Won {}
class Lost {}
class Loading {}
class LeftClick {constructor(arg0,arg1){this.val0=arg0,this.val1=arg1}}
const _LeftClick = arg0 => arg1 => new LeftClick(arg0,arg1)
class RightClick {constructor(arg0,arg1){this.val0=arg0,this.val1=arg1}}
const _RightClick = arg0 => arg1 => new RightClick(arg0,arg1)
class Reset {}
class SeedRecieved{constructor(arg0){this.val0=arg0}}
const _SeedRecieved = arg0 => new SeedRecieved(arg0)

export var $dispatchRef = undefined

//REQUEST SEED METHOD, generates a random integer between 0-1000 impurely
//responseHandler takes an int and returns an event type containing the seed
export const requestSeed = responseHandler => {
	const impureRandomInt = Math.floor(Math.random() * 1000)
	$dispatchRef(responseHandler(impureRandomInt))
}

export const init = _State(L.replicateGrid(10)(new Hidden()))(new L.$EmptyList())(new Loading())(new Nothing())

export const update = (s,e) => {

	console.log("Message recieved! It is:")
	console.log(e)

	if(e instanceof SeedRecieved){
		console.log("inside seedrecieved")
		const initGen = mkRandomGen(e.val0)
		const [ranCoords, newGen] = genBombCoords(initGen)(10)
		const newState = s.set("gen", new Just(newGen)).set("bombCoords", ranCoords).set("gameState", new Playing())
		console.log("new state is:")
		console.log(newState)
		return newState
	}
	if(e instanceof LeftClick){
		console.log("inside left click")
		const y = e.val0
		const x = e.val1
		const elem = L.index2D(s.grid)(y)(x)
		if(elem instanceof Hidden){
			console.log("inside hidden")
			const result = bombOrCount(s.bombCoords)([y,x])
			console.log("got bomb count, it is: ", result)
			if(result instanceof Nothing){
				console.log("inside nothing")
				return s.set("grid", L.replace2D(s.grid)(y)(x)(new Exploded())).set("gameState", new Lost())
			}
			if(result instanceof Just){
				console.log("inside just")
				const newGrid = L.replace2D(s.grid)(y)(x)(new Revealed(result.val0))
				console.log("checking if won...")
				if(checkWon(newGrid)){
					return s.set("grid", L.replace2D(s.grid)(y)(x)(new Revealed(result.val0))).set("gameState", new Won())
				}
				if(true){
					return s.set("grid", L.replace2D(s.grid)(y)(x)(new Revealed(result.val0)))
				}
			}
		}
		if(elem instanceof Flagged){
			return s
		}
		if(elem instanceof Revealed){
			return s
		}
		$error("Uncovered case in match expression!")
	}
	if(e instanceof RightClick){

		console.log("inside flagged")

		const y = e.val0
		const x = e.val1
		const elem = L.index2D(s.grid)(y)(x)

		if(elem instanceof Hidden){
			return s.set("grid", L.replace2D(s.grid)(y)(x)(new Flagged()))
		}
		if(elem instanceof Flagged){
			return s.set("grid", L.replace2D(s.grid)(y)(x)(new Hidden()))
		}
		if(elem instanceof Revealed){
			return s
		}
		$error("Unhandled case in match expression!")

	}
	if(e instanceof Reset){
		console.log("inside reset")

		const gen = s.gen.val0
		console.log("gen is:")
		console.log(gen)
		const [ranCoords, newGen] = genBombCoords(s.gen.val0)(10)
		const newState = s.set("gen", new Just(newGen)).set("bombCoords", ranCoords).set("gameState", new Playing()).set("grid",L.replicateGrid(10)(new Hidden()))
		return newState
	}
	return s

}

const genBombCoords = gen => count => {
	return genBombCoordsHelper(gen)(count)(new L.$EmptyList())
}

const genBombCoordsHelper = gen => count => acc => {
	const [y,fstgen] = randomGenBetween(0)(9)(gen)
	const [x,sndgen] = randomGenBetween(0)(9)(fstgen)
	const ranCoord = [y,x]
	if(count === 0){
		return [acc,gen]
	}
	if(true){
		if(containsCoord(acc)(ranCoord)){
			return genBombCoordsHelper(sndgen)(count)(acc)
		}
		if(true){
			return genBombCoordsHelper(sndgen)(count - 1)(L.$_Cons(ranCoord)(acc))
		}
	}
}

const containsCoord = list => target => {
	if(list instanceof L.$EmptyList){
		return false
	}
	if(true){
		const [y,x] = L.index(list)(0)
		const rest = L.drop(1)(list)
		const [qy,qx] = target
		if(qy == y && qx == x){
			return true
		}
		if(true){
			return containsCoord(rest)(target)
		}
	}
}

const bombOrCount = bombCoords => coord => {
	
	if(bombCoords instanceof L.$EmptyList){
		console.log("insi")
		return new Just(0)
	}
	
	console.log("inside bomb or count")
	const [bombY,bombX] = L.index(bombCoords)(0)
	const bombs = L.drop(1)(bombCoords)
	const [coordY,coordX] = coord
	const diffY = Math.abs(bombY - coordY)
	const diffX = Math.abs(bombX - coordX)
	const plusMaybe = x => mayb => {
		if(mayb instanceof Nothing){
			return new Nothing()
		}
		if(mayb instanceof Just){
			const y = mayb.val0
			return new Just(x + y)
		}
	}

	if(diffY == 0 && diffX == 0){
		return new Nothing()
	}
	if(diffY <= 1 && diffX <= 1){
		return plusMaybe(1)(bombOrCount(bombs)(coord))
	}
	if(true){
		return bombOrCount(bombs)(coord)
	}

}

const checkWon = grid => {
	const isRevealed = cell => cell instanceof Revealed
	return (result => result == 90)(L.length(L.concat(L.filter2D(isRevealed)(grid))))
}

export const View = () => {
	
	const [s, $dispatch] = React.useReducer(update,init)

	$dispatchRef = $dispatch

	//initEffect :O
	useEffect(() => requestSeed(_SeedRecieved), [])

	//initEffect :O
	/*
	useEffect(() => {
		console.log("value of $dispatch:")
		console.log($dispatch)
		$dispatchRef = $dispatch
		console.log("new value of $dispatchRef:")
		console.log($dispatchRef)
		requestSeed(_SeedRecieved)
	}, []) */

	useEffect(() => console.log("I should be showing with every re-render"))

	/*
	return _Column(L.js2syphon([
		_Button("reset icon here")(new Reset())
	]))*/ 
	
	
	//return _Row(myTestFn(s.gameState)(0)(0)(s.cellStates))
	
	return <Context.Provider value = {$dispatch}>{
		_Column(
			L.$_Cons(
				_Button({})("RESET")(new Reset())
			)(
			L.map(y => _Row(makeButtonList(s.gameState)(y)(0)(L.index(s.grid)(y))(s.bombCoords)))($listGen(0,9)))
		)
	}</Context.Provider>
	
}

//L.map(y => _Row(makeButtonList(s.gameState)(y)(0)(L.index(s.grid)(y))))($listGen(0,9)))
//mineCoords is OPTIONAL delete after testing is done
export const makeButtonList = gameState => y => x => cellStates => mineCoords => {
	
	if(cellStates instanceof L.$EmptyList){
		return L.$_EmptyList()
	}
	if(true){

		const cellState = L.index(cellStates)(0)

		return L.$_Cons(
			(() => {
				if(gameState instanceof Loading){
					//return _Container({dim : [30,30], bgColor : "#e0e0e0"})(<div style = {{backgroundColor : "blue"}}>a</div>)
					return _Graphic({dim : [30,30], bgColor : "#e0e0e0"})(explosion)
				}
				if(gameState instanceof Won){
					return _Panel({dim : [30,30], bgColor : "green"})
				}
				if(gameState instanceof Lost){
					return _Container({dim : [30,30], bgColor: "red"})(
						(() => {
							if(cellState instanceof Exploded){
								_Graphic({dim : [30,30], bgColor : "#e0e0e0"})(explosion)
							}
							if(true){
								_Container({dim : [30,30], bgColor: cellState instanceof Hidden ? "grey" : "#e0e0e0"})
									(cellState instanceof Revealed ? (cellState.val0 === 0 ? undefined : <div color = "red">cellState.val0</div>) : undefined)
							}
						})()
					)
				}
				if(gameState instanceof Playing){
					if(containsCoord(mineCoords)([y,x])){
						//JUST FOR DEMONSTRATION NEEDS TO BE REMOVED LATER, MINES MUST BE HIDDEN
						return _Button({dim : [30,30], bgColor : "black", onRightClick : _RightClick(y)(x), style: 0})(_Graphic(mine))(_LeftClick(y)(x))
					}
					if(cellState instanceof Flagged){
						return _Button({dim : [30,30], bgColor : "red", onRightClick : _RightClick(y)(x), style: 0})(undefined)(undefined)
					}
					if(cellState instanceof Revealed){
						return _Button({dim : [30,30], bgColor : "#e0e0e0"})(cellState.val0)(_LeftClick(y)(x))
					}
					//hidden
					else {
						return _Button({dim : [30,30], bgColor : "grey", onRightClick : _RightClick(y)(x), style: 0})(undefined)(_LeftClick(y)(x))
					}
				}
				/*
				if(gameState instanceof Loading){
					return _Panel({dim : [30,30], bgColor = "blue"})
				}
				if(true){
					return _Panel({dim : [30,30]})
				}*/
			})()
		)(makeButtonList(gameState)(y)(x + 1)(L.drop(1)(cellStates))(mineCoords))
	}

}

/*
const makeButtonList = gameState => y => x => cellStates => {
	const disabled = gameState instanceof Playing
	const cellState = index(cellStates)(0)
	const states = drop(1)(cellStates)
	return x == 10 ? L.$_EmptyList() : L.$_Cons(
		(() => {

			return _Box({backgroundColor : "blue", width : 30, height : 30})

			
			if(cellState instanceof Hidden){

			}
			if(cellState instanceof Revealed){

			}
			if(cellState instanceof Flagged){

			}
			if(cellState instanceof Exploded){

			} 

		})()
	)(
		makeButtonList(gameState)(y)(x + 1)(states)
	)
}*/