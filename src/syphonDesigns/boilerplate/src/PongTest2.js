import React, { useEffect } from "react"
import {_Panel, _Container, _Graphic} from "./lib/GUI/Misc"
import {_Row, _Column, Row, Column} from "./lib/GUI/Layout"
import {KeyChar, ArrowUp, ArrowDown, $keyExprToCode} from "./lib/Hardwired/Core"
import { stat } from "fs"
import { eventNames } from "process"
import { _Button } from "./lib/GUI/Button"
import { ToggleOffRounded, ToggleOnRounded } from "@material-ui/icons"

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
class Toggled {}

const initState = {
	leftPaddleState : new Stationary(),
	rightPaddleState : new Stationary(),
	count : 0,
	paused : false
}

const update = (state,event) => {

	console.log("Recieved a message. It is:")
	console.log(event)

	if(event instanceof Toggled){
		const newState = {...state, paused : !state.paused}
		console.log("Value of toggled is now: ", newState.paused)
		return newState
	}
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
	if(event instanceof PlayerTwoUp){
		return {...state, rightPaddleState : new GoingUp()}
	}
	if(event instanceof PlayerTwoDown){
		return {...state, rightPaddleState : new GoingDown()}
	}
	if(event instanceof PlayerTwoRelease){
		return {...state, rightPaddleState : new Stationary()}
	}

	//fallback case
	return state

}

/*
useEffect(() => {
		document.addEventListener("keyup", e => {
			console.log("key released")
			switch(e.code){
				case "ArrowUp": $dispatch(new PlayerOneRelease()); break;//$dispatch(new PlayerOneRelease()); break;
				case "ArrowDown": $dispatch(new PlayerOneRelease()); break; //$dispatch(new PlayerOneRelease()); break;
			}
		})
		document.addEventListener("keydown", e => {
			console.log("key pressed")
			switch(e.code){
				case "ArrowUp": $dispatch(new PlayerOneUp()); break; //$dispatch(new PlayerOneUp()); break;
				case "ArrowDown": $dispatch(new PlayerOneDown()); break;//$dispatch(new PlayerOneDown()); break;
			}
		})
		
	}, [])
*/

export const onPassed = (interval,event,dispatchRef,timeRef) => {
	timeRef = setInterval(() => dispatchRef(event),interval)
}
export const onKeyPress = (key,event,dispatchRef) => {
	console.log("onKeyPress called")
	document.addEventListener("keydown", e => {
		if(e.code == key) dispatchRef(event)
	})
}
export const onKeyRelease = (key,event,dispatchRef) => {
	document.addEventListener("keyup", e => {
		if(e.code == key) dispatchRef(event)
	})
}
export const onMouseMove = (responseHandler,dispatchRef) => {
	document.addEventListener("mousemove", e => dispatchRef(responseHandler(e.pageY)(e.pageX)))
}



export const View = () => {

	const [state, $dispatch] = React.useReducer(update, initState)
	//We give it an initial state
	


	/*
	useEffect(() => {
		function handler(e){
			switch(e.code){
				case "ArrowUp": $dispatch(new PlayerOneUp()); break;
				case "ArrowDown": $dispatch(new PlayerOneDown()) ; break;
				case "KeyW": $dispatch(new PlayerTwoUp()); break;
				case "KeyS": $dispatch(new PlayerTwoDown()); break;
			}
		}
		if(!state.paused) {
			document.addEventListener("keydown", handler)
		}
		return () => document.removeEventListener("keydown", handler)
	}, [state.paused])

	useEffect(() => {
		function handler(e){
			switch(e.code){
				case "ArrowUp": $dispatch(new PlayerOneRelease()); break;
				case "ArrowDown": $dispatch(new PlayerOneRelease()) ; break;
				case "KeyW": $dispatch(new PlayerTwoRelease()); break;
				case "KeyS": $dispatch(new PlayerTwoRelease()); break;
			}
		}
		if(!state.paused) {
			document.addEventListener("keyup", handler)
		}
		return () => document.removeEventListener("keyup", handler)
	}, [state.paused])*/

	useEffect(() => {
		function handler(e){
			switch(e.code){
				case "ArrowUp": $dispatch(new PlayerOneUp()); break;
				case "ArrowDown": $dispatch(new PlayerOneDown()) ; break;
				case "KeyW": $dispatch(new PlayerTwoUp()); break;
				case "KeyS": $dispatch(new PlayerTwoDown()); break;
			}
		}
		if(!state.paused) {document.addEventListener("keydown", handler)}
		return () => document.removeEventListener("keydown", handler)
	}, [state.paused])

	/*

	function $handler(e){
		if(e.code == keyExprToCode(<FIRST ONE>)){$dispatch(<EVENT>)}
		if(e.code == keyExprToCode(<SECOND ONE>){$dispatch(<EVENT>)})
	}

	*/

	useEffect(() => {
		function handler(e){
			if(e.code == $keyExprToCode(new ArrowUp())){$dispatch(new PlayerOneRelease())}
			if(e.code == $keyExprToCode(new ArrowDown())){$dispatch(new PlayerOneRelease())}
			if(e.code == $keyExprToCode(new KeyChar('w'))){$dispatch(new PlayerTwoRelease())}
			if(e.code == $keyExprToCode(new KeyChar('s'))){$dispatch(new PlayerTwoRelease())}
			/*
			switch(e.code){
				case "ArrowUp": $dispatch(new PlayerOneRelease()); break;
				case "ArrowDown": $dispatch(new PlayerOneRelease()) ; break;
				case "KeyW": $dispatch(new PlayerTwoRelease()); break;
				case "KeyS": $dispatch(new PlayerTwoRelease()); break;
			}*/
		}
		if(!state.paused) {document.addEventListener("keyup", handler)}
		return () => document.removeEventListener("keyup", handler)
	}, [state.paused])

	useEffect(() => {
		let $time = undefined
		if(!state.paused){$time = setInterval(() => $dispatch(new Tick()), 1000)}
		return () => clearInterval($time)
		/*
		let $time = undefined
		if(!state.paused) {onPassed(1000, new Tick(), $dispatch, $time)}
		return () => clearInterval($time) */
	}, [state.paused])

	return <Context.Provider value = {$dispatch}><Row>
		{_Container({dim : [100,30]})(stateToString(state.leftPaddleState))}
		{_Container({dim : [30,30]})(state.count)}
		{_Container({dim : [100,30]})(stateToString(state.rightPaddleState))}
		{_Button({})("click me")(new Toggled())}
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