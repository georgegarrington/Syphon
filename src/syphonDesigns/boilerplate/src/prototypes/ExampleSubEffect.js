import {useReducer, createContext, useEffect} from "react"
//import {Record} from "immutable"
import {Button, _Button} from "../lib/GUI/Button"
import {_Row, _Column} from "../lib/GUI/Layout"
import {_Text} from "../lib/GUI/Text"
import * as L from "../lib/Data/List"
import {calc, safeDiv, _add, _sub, _mul, _State, Clear,
_Clear, Eq, _Eq, OpPressed, _OpPressed, DigitPressed, _DigitPressed, useTimePass} from "./CalcHelpers"

//If you want to construct a record by specifying named fields then we use the "new" syntax
//const init = new State(display = "", operator = safeDiv, operand = 0)

//Alternatively you can also construct the record using the curried syntax like so:
const init = _State(" ")(safeDiv)(0)

const update = (state, event) => {

	console.log("Event message recieved: " + event.constructor.name)
	//console.log("The value in the event is: " + event.val0)
	console.log("Old state: " + state)

	//Again, example of how a match expression looks like in Javascript :)
	const updated = event instanceof Clear ? init :
	event instanceof Eq ? _State(String(calc(state)))(safeDiv)(0) :
	event instanceof OpPressed ? _State("")(event.val0)(parseInt(state.display)) :
	event instanceof DigitPressed ? state.set("display", state.display + event.val0) : 
	state

	console.log("Updated state: " + updated)
	return updated

}

export const Context = createContext()

var dispatchRef = undefined

const timeSubscription = interval => event => {
	setInterval(() => $dispatchRef(event),interval)
}

var $time = undefined

//Needs to be in the same file
const onPassed = (interval, event) => {
	$time = setInterval(() => $dispatchRef(event), interval)
}

export const subscribe = $arg0 => {
	return onPassed(1000)(_DigitPressed(9))
}

const useSyphon = (updateFn, initState, effectFn) => {

	const [state, dispatch] = React.useReducer(updateFn, initState)

	const dispatchMod = event => {
		effectFn(state, event)
		dispatch(event)
	}

	return [state, dispatchMod]

}

//arg0 is state, arg1 is event
export const effect = (arg0, arg1) => {

	console.log("Inside the effect function woohoo!")
	console.log("Value of the display is: " + arg0.display)
	console.log("And the event was: " + (arg1 instanceof DigitPressed))

}

//The only side effecting function
export const View = () => {

	//useReducer implements the MVU pattern, state is the state value that will be updated 
	const [s, $dispatch] = useSyphon(update, init, effect)

	$dispatchRef = $dispatch

	useEffect(() => {
		$time = undefined

		subscribe(s)

		//$time = onTimePass(1000, _DigitPressed(9))
		return () => {
			clearInterval($time)
		}
	})

	//The "where" definitions from the flow function are always at the top of the JS function
	const mkDigitButton = arg0 => {
		//console.log("mkDigitButton called with val: " + arg0)
		//return <Button onClick = {() => dispatch()}></Button>
		return _Button(String(arg0))(
			_DigitPressed(String(arg0))
			//() => console.log()
		)
	}

	const mkOpButton = arg0 => arg1 => {
		return _Button(arg0)(
			_OpPressed(arg1)
			//() => console.log()
		)
	}

	return(<Context.Provider value = {dispatch}>{

		(() => {

			if(true){

			return (

				_Column(L.js2syphon([

					//_Row(L.js2syphon([_Text(state.display)])),
			
					_Text(state.display),

					_Row(L.append(
						L.map(mkDigitButton)(L.js2syphon([7,8,9]))
					)(L._Cons(mkOpButton("*")(_mul))(L._EmptyList()))),
					
					_Row(L.append(
						L.map(mkDigitButton)(L.js2syphon([4,5,6]))
					)(L._Cons(mkOpButton("/")(safeDiv))(L._EmptyList()))),
			
					_Row(L.append(
						L.map(mkDigitButton)(L.js2syphon([1,2,3]))
					)(L._Cons(mkOpButton("-")(_sub))(L._EmptyList()))),
			
					_Row(L.js2syphon([
						_Button("C")(_Clear()),
						mkDigitButton(0),
						_Button("=")(_Eq()),
						mkOpButton("+")(_add)
					]))]))

			)}
		})()
			
		}</Context.Provider>)

}