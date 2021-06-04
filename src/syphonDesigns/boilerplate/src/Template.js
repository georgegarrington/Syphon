import React from 'react';
//import icon from '../assets/icon.svg';
import './App.global.css';
import {$_Row, $_Column} from "./lib/GUI/Layout"
import {$_Button} from "./lib/GUI/Button"
import {$_Text} from "./lib/GUI/Text"
import {$add,$sub} from "./lib/Hardwired/Math"
import dialog from "electron"
import fs from "fs"
import {$EmptyList, $_EmptyList, $Cons, $_Cons, $append, $index, $map, $reduce, $length, $drop, $take, $tail, $head,
	$repeat, $replace, $replace2D, $index2D, $singleton, $syphon2js, $js2syphon, $syphon2js2D, $js2syphon2D} from "./lib/Data/List"
	

const _Tick = () => new Tick()
class Tick {}
const _Start = () => new Start()
class Start {}
const _Stop = () => new Stop()
class Stop {}
const _Reset = () => new Reset()
class Reset {}
const _Toggle = () => new Toggle()
class Toggle {}
export class Inc{}
export const _Inc = () => {return new Inc()}
export class Dec{}
export const _Dec = () => {return new Dec()}


const init = 0

//This will always be placed in the program (I think?)
var $dispatchRef = undefined

//Subscription references must be module level globally accessible
var $timeSub = undefined

//Subscription generating function
const onPassed = interval => event => {
	$timeSub = setInterval(() => $dispatchRef(event), interval)
} 

const subscribe = state => {
	//This is how a conditional expression looks in JS
	(() => {
		//The 3rd element of the start tri-tuple is the stopped bool
		if(state[2]){
			return undefined //The JS version of NoSub
		}
		if(true){
			return onPassed(7)(_Tick())
		}
	})()
}

//Hey there

export const update = ($arg0,$arg1) => {

	const $name = "update"
	const s = $arg0
	const e = $arg1
	return (() => {
		if(e instanceof Inc){
			return ($add(s))(1)
		}
		if(e instanceof Dec){
			return ($sub(s))(1)
		}
		error("Uncovered match expression case in function:" + $name + "!")
	})()

}

export const Context = React.createContext()

//The only side effecting function
export const View = () => {

	//useReducer implements the MVU pattern, state is the state value that will be updated 
	const [$arg0, $dispatch] = React.useReducer(update, init)

	//Keep a module level reference to the dispatch function
	$dispatchRef = $dispatch

	/*
	//Specifying [] as a second argument means that this will only run after the initial render,
	so use this if an initEffect is declared
	React.useEffect(() => {

	}, []) */

	
	//Normal useEffect used if subscriptions are present

	//var str = "Nothing yet"

	/*
	React.useEffect(() => {
	
		fs.readFile("input.txt", (err,data) => {
		
			console.log("Inside the file function")
			console.log("Err is:" + err)
			console.log("Data is:" + data)
			dispatch(_TextReceived(data.toString()))
		})
	
	}, [])*/

	return (<Context.Provider value = {$dispatch}>{(() => {

		const s = $arg0
		return $_Column(($_Cons(($_Button("+"))(_Inc())))(($_Cons($_Text(String(s))))(($_Cons(($_Button("-"))(_Dec())))($_EmptyList()))))

	})()}</Context.Provider>)
	
	/*
	
	return(<Context.Provider value = {dispatch}>{

		_Column(L.js2syphon([

			//_Row(L.js2syphon([_Text(state.display)])),
			_Button(String("+"))(_Inc()),
			_Text(state),
			_Button(String("-"))(_Dec())
		
		]))
			
	}</Context.Provider>)
	
	*/

	
}

//const _Text = txt => <span>{txt}</span>

/*
export default function App() {
  return (
    <Router>
      <Switch>
        <Route path="/" component={View} />
      </Switch>
    </Router>
  );
}*/
