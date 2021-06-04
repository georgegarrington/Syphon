import React from 'react';
import { BrowserRouter as Router, Switch, Route } from 'react-router-dom';
import icon from '../assets/icon.svg';
import './App.global.css';
import {_Row, _Column} from "./lib/GUI/Layout"
import {_Button} from "./lib/GUI/Button"
import {_Text} from "./lib/GUI/Text"
import dialog from "electron"
import * as L from "./lib/Data/List"
import fs from "fs"

const init = [0, "Nothing yet"]

class Inc {}
const _Inc = () => new Inc()
class Dec {}
const _Dec = () => new Dec()

class TextReceived {constructor(arg0){this.val0 = arg0}}
const _TextReceived = str => new TextReceived(str)

const update = (state, event) => {
	if(event instanceof Inc){
		return [state[0] + 1, state[1]]
	}
	if(event instanceof Dec){
		return [state[0] - 1, state[1]]
	}
	if(event instanceof TextReceived){
		return [state[0], event.val0]
	}
}

export const Context = React.createContext()

var dispatchRef = undefined



//The only side effecting function
export const View = () => {

	console.log("Inside the view function")

	//useReducer implements the MVU pattern, state is the state value that will be updated 
	const [state, dispatch] = React.useReducer(update, init)

	//Keep a module level reference
	dispatchRef = dispatch

	//var str = "Nothing yet"

	React.useEffect(() => {
	
		fs.readFile("input.txt", (err,data) => {
		
			console.log("Inside the file function")
			console.log("Err is:" + err)
			console.log("Data is:" + data)
			dispatch(_TextReceived(data.toString()))
		})
	
	}, [])

	return (<Context.Provider value = {dispatch}>{
		_Column(L.js2syphon([
		
			_Button(state[0])(_Inc()),
			_Text(state[1]),
			_Button(state[0])(_Dec())
		
		]))
	}</Context.Provider>)
	
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

export default function App() {
  return (
    <Router>
      <Switch>
        <Route path="/" component={View} />
      </Switch>
    </Router>
  );
}
