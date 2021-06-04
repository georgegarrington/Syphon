import React from "react"
import {Record} from "immutable"
import {$add,$sub,$mul,$div,$mod,$and,$or,not,$eq,$nEq,$gt,$lt,$gtEq,$ltEq,append,$listGen,
	Just, Nothing, _Just, _Nothing, $Unit, $_Unit, fst, snd, $keyExprToCode, _UpArrow, _DownArrow, _LeftArrow, 
	_RightArrow, _KeyChar, Left, Right, _Left, _Right, mkRandomGen, randomGenBetween, abs, requestSeed,
	readFile, writeFile, useSyphon, NoEffect, _NoEffect} from "./lib/Hardwired/Core.js"
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

class Inc{}
const _Inc = () => new Inc()
class Dec{}
const _Dec = () => new Dec()

const init = 0

const update = ($arg0,$arg1) => {
	const $name = "update"
	const s = $arg0
	const e = $arg1
	return (() => {
		if(e instanceof Inc){
			return $add(s)(1)
		}
		if(e instanceof Dec){
			return $sub(s)(1)
		}
		$error("Uncovered match expression case in function:" + $name + "!")
	})()
}

export const App = () => {
	const [state,$dispatch] = React.useReducer(update,init)
	window.$dispatchRef = $dispatch
	return view(state)
}

const view = $arg0 => {
	const $name = "view"
	const s = $arg0
	return _Column({})($_Cons(_Button({})(_Text({})("+"))(_Inc()))
		($_Cons(_Text({})(String(s)))($_Cons(_Button({})
		(_Text({})("-"))(_Dec()))($_EmptyList()))))
	$error("Uncovered pattern case in function: view!")
}
