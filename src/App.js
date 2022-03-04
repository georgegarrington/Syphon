import React from "react"
import {Record} from "immutable"
import {$add,$sub,$mul,$div,$mod,$and,$or,not,$eq,$nEq,$gt,$lt,$gtEq,$ltEq,append,$listGen,
	Just, Nothing, _Just, _Nothing, $Unit, $_Unit, fst, snd, $keyExprToCode, _UpArrow, _DownArrow, _LeftArrow, 
	_RightArrow, _KeyChar, Left, Right, _Left, _Right, mkRandomGen, randomGenBetween, abs, requestSeed,
	useSyphon, NoEffect, _NoEffect} from "./lib/Hardwired/Core.js"
import {_Button} from "./lib/GUI/Button.js"
import {_Text, _TextEditor, _TextField} from "./lib/GUI/Text.js"
import {_Graphic, _Container, _Panel} from "./lib/GUI/Misc.js"
import {_Canvas, _Stroke, _Oval, _Rectangle, Stroke, Oval, Rectangle} from "./lib/GUI/Canvas2.js"
import {_Row, _Column, DummyWidget, _DummyWidget} from "./lib/GUI/Layout.js"
import {$EmptyList, $_EmptyList, $Cons, $_Cons, $append, concat, $index, map, reduce, length, drop, take, tail, head,
	replace, replace2D, index2D, singleton, $syphon2js, $js2syphon, $syphon2js2D, $js2syphon2D, replicate2D, replicate, filter2D, map2D} from "./lib/Data/List.js"
import {_SaveIcon, $mkImg} from "./lib/GUI/Icons.js"
var $errLog = []
var $shouldPrint = false
const $errMsg = msg => {$errLog.push(msg);if($shouldPrint){console.log("\"" + msg + "\"")}}


export const Context = React.createContext()

class Clear{}
const _Clear = () => new Clear()
class Eq{}
const _Eq = () => new Eq()
class OpPress{constructor(arg0){this.val0 = arg0}}
const _OpPress = arg0 => new OpPress(arg0)
class Digit{constructor(arg0){this.val0 = arg0}}
const _Digit = arg0 => new Digit(arg0)

class State extends Record({display : undefined, operator : undefined, operand1 : undefined}){}
const _State = arg0 => arg1 => arg2 =>
	new State({display : arg0, operator : arg1, operand1 : arg2})

const init = _State("")($add)(0)


const update = ($arg0,$arg1) => {

	const $name = "update"
	const s = $arg0
	const e = $arg1
	const calc = (arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const st = arg0

		return String(st.operator(st.operand1)(parseInt(st.display)))
	})
	
	return (() => {
		if(e instanceof Clear){
			return init
		}
		if(e instanceof Eq){
			return _State(calc(s))($add)(0)
		}
		if(e instanceof OpPress){
			const op = e.val0
			return _State("")(op)(parseInt(s.display))
		}
		if(e instanceof Digit){
			const i = e.val0
			return s.set("display", append(s.display)(String(i)))
		}
		$error("Uncovered match expression case in function:" + $name + "!")
	})()

}

export const App = () => {

	const [state,$dispatch] = React.useReducer(update,init)
	window.$dispatchRef = $dispatch
	return (<Context.Provider value = {$dispatch}>{(() => {

		return view(state)

	})()}</Context.Provider>)
}

const view = $arg0 => {
	const $name = "view"
	const s = $arg0
	const mkDigitButton = (arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const i = arg0

		return _Button({})(_Text({})(String(i)))(_Digit(i))
	})
	
	return _Column({})($_Cons(_Container({dim : [275,35], bgColor : "#A9A9A9"})(_Text({})(s.display)))($_Cons(_Row($append(map(mkDigitButton)($listGen(7, 9)))($_Cons(_Button({})(_Text({})("X"))(_OpPress($mul)))($_EmptyList()))))($_Cons(_Row($append(map(mkDigitButton)($listGen(4, 6)))($_Cons(_Button({})(_Text({})("/"))(_OpPress($div)))($_EmptyList()))))($_Cons(_Row($append(map(mkDigitButton)($listGen(1, 3)))($_Cons(_Button({})(_Text({})("-"))(_OpPress($sub)))($_EmptyList()))))($_Cons(_Row($_Cons(mkDigitButton(0))($_Cons(_Button({})(_Text({})("AC"))(_Clear()))($_Cons(_Button({})(_Text({})("="))(_Eq()))($_Cons(_Button({})(_Text({})("+"))(_OpPress($add)))($_EmptyList()))))))($_EmptyList()))))))
	$error("Uncovered pattern case in function: view!")
}


