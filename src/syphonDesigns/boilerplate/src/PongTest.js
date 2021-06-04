import React, { useEffect } from "react"
import {_Panel, _Container, _Graphic} from "./lib/GUI/Misc"
import {_Row, _Column, Row, Column} from "./lib/GUI/Layout"
//import { Context } from "./App"

export const Context = React.createContext()

class Stationary {}
class GoingUp {}
class GoingDown {}

class Tick {}
class PlayerOneDown {}
class PlayerOneUp {}
class PlayerOneRelease{}
class PlayerTwoDown {}
class PlayerTwoUp {}
class PlayerTwoRelease {}

const initState = {
	leftPaddleState : new Stationary(),
	rightPaddleState : new Stationary(),
	count : 0
}

const update = (state,event) => {

	console.log("Recieved a message. It is:")
	console.log(event)

	if(event instanceof Tick){
		return {...state, count : state.count + 1}
	}
	if(event instanceof PlayerOneUp){
		console.log("player one up recieved")
		return {...state, leftPaddleState : new GoingUp()}
	}
	if(event instanceof PlayerOneDown){
		console.log("player one down recieved")
		return {...state, leftPaddleState : new GoingDown()}
	}
	if(event instanceof PlayerOneRelease){
		console.log("player one release recieved")
		return {...state, leftPaddleState : new Stationary()}
	}

	//fallback case
	return state

}

export const View = () => {

	const [state, $dispatch] = React.useReducer(update, initState)
	//We give it an initial state
	

	useEffect(() => {
		document.addEventListener("keyup", e => {
			console.log("key released")
			switch(e.code){
				case "ArrowUp": $dispatch(new PlayerOneRelease()); break;//$dispatch(new PlayerOneRelease()); break;
				case "ArrowDown": $dispatch(new PlayerOneRelease()); break; //$dispatch(new PlayerOneRelease()); break;
				default: console.log("The code was: ", e.code)
			}
		})

		function downHandler(e){
			if(e.code === "ArrowUp") $dispatch(new PlayerOneUp())
		}

		document.addEventListener("keydown", downHandler)

		/*
		document.addEventListener("keydown", e => {
			console.log("key pressed")
			if(e.code === "ArrowUp") {$dispatch(new PlayerOneUp())}
			
			switch(e.code){
				case "ArrowUp": $dispatch(new PlayerOneUp()); break; //$dispatch(new PlayerOneUp()); break;
				case "ArrowDown": $dispatch(new PlayerOneDown()); break;//$dispatch(new PlayerOneDown()); break;
			}
		}) */
	}, [])
	
	useEffect(() => {
		const $time = setInterval(() => $dispatch(new Tick()),1000)
		return () => {clearInterval($time)}
	}, [])

	return <Context.Provider value = {$dispatch}><Row>
		{_Container({dim : [100,30]})(stateToString(state.leftPaddleState))}
		{_Container({dim : [30,30]})(state.count)}
		{_Container({dim : [100,30]})(stateToString(state.rightPaddleState))}
	</Row></Context.Provider>
}

const stateToString = state => {
	if(state instanceof Stationary){
		return "Stationary"
	}
	if(state instanceof GoingUp){
		return "GoingUp"
	}
	if(state instanceof GoingDown){
		return "GoingDown"
	}
	else {
		console.log("State wasnt as expected.")
		console.log(state)
		return state
	}
}


/*

const [leftState, setLeftState] = React.useState(new Stationary())
const [rightState, setRightState] = React.useState(new Stationary())
const [count, setCount] = React.useState(0)

*/