import prand from "pure-rand"
import {$EmptyList, $Cons} from "../Data/List.js"
import * as fs from "fs"
import {ipcMain, dialog} from "electron"
import React from "react"
//import {fs} from "electron"

/*
export const $add = x => y => x + y
export const $sub = x => y => x - y
export const $mul = x => y => x * y
export const $div = x => y => x / y
export const $mod = x => y => x % y

export const $and = arg0 => arg1 => arg0 && arg1
export const $or = arg0 => arg1 => arg0 || arg1
*/

//MATH
export const $add = x => y => x + y
export const $sub = x => y => x - y
export const $mul = x => y => x * y
export const $div = x => y => x / y
export const $mod = x => y => x % y
export const abs = n => Math.abs(n)

//BOOL
export const $and = arg0 => arg1 => arg0 && arg1
export const $or = arg0 => arg1 => arg0 || arg1
//Syphon name is the same so does not need $
export const not = arg0 => !arg0
export const $eq = arg0 => arg1 => arg0 === arg1
export const $nEq = arg0 => arg1 => arg0 !== arg1
export const $gt = arg0 => arg1 => arg0 > arg1
export const $lt = arg0 => arg1 => arg0 < arg1
export const $gtEq = arg0 => arg1 => arg0 >= arg1
export const $ltEq = arg0 => arg1 => arg0 <= arg1

//DATA TYPES
export class Nothing {}
export const _Nothing = () => new Nothing()
export class Just {constructor(arg0){this.val0 = arg0}}
export const _Just = arg0 => new Just(arg0) 
export class Left {constructor(arg0){this.val0 = arg0}}
export const _Left = arg0 => new Left(arg0)
export class Right {constructor(arg0){this.val0 = arg0}}
export const _Right = arg0 => new Right(arg0)

//Unit value
export class $Unit {}
export const $_Unit = () => new $Unit()

//RANDOM
//init a random generator from a seed
export const mkRandomGen = seed => prand.mersenne(seed)
export const randomGenBetween = lower => higher => gen => prand.uniformIntDistribution(lower,higher,gen)

export const requestSeed = responseHandler => {
	const impureGendInt = Math.floor(Math.random() * 1000)
	window.$dispatchRef(responseHandler(impureGendInt))
}

//STRING
export const append = arg0 => arg1 => arg0.concat(arg1)

//UTILITY
export class KeyChar {constructor(arg0){this.val0 = arg0}}
export const _KeyChar = arg0 => new KeyChar(arg0)
export class DownArrow {}
export const _DownArrow = () => new DownArrow()
export class UpArrow {}
export const _UpArrow = () => new UpArrow()
export class LeftArrow {}
export const _LeftArrow = () => new LeftArrow()
export class RightArrow {}
export const _RightArrow = () => new RightArrow()

export const $keyExprToCode = keyExpr => {
	if(keyExpr instanceof KeyChar){
		return "Key".concat(keyExpr.val0.toUpperCase())
	}
	if(keyExpr instanceof UpArrow){
		return "ArrowUp"
	}
	if(keyExpr instanceof DownArrow){
		return "ArrowDown"
	}
	if(keyExpr instanceof ArrowRight){
		return "ArrowRight"
	}
	if(keyExpr instanceof ArrowLeft){
		return "ArrowLeft"
	}
}

//Tuple stuff
export const fst = tup => tup[0]
export const snd = tup => tup[1]

//This will be changed into an Electron dialog message later, but just keep as this for now
export const $errMsg = msg => console.log(msg)

export const $errDialog = msg => dialog.showMessageBox(msg)

export const $listGen = (from, to, snd = undefined) => {
	const interval = snd == undefined ? 1 : Math.abs(from - snd)
	let head = new $Cons(from, new $EmptyList())
	let current = head
	//Incrementing list generator
	if(from < to){
		for(let i = from + interval; i <= to; i += interval){
			let newNode = new $Cons(i, new $EmptyList())
			current.val1 = newNode
			current = newNode
		}
	//Decrementing list generator
	} else {
		for(let i = from - interval; i >= to; i -= interval){
			let newNode = new $Cons(i, new $EmptyList())
			current.val1 = newNode
			current = newNode
		}
	}
	return head
}

export const showOpenDialog = responseHandler => errMsg => {
	dialog.showOpenDialog({
		properties: ["openFile"]
	}).then(result => {
		if(result.canceled){return}
		fs.readFile(result.filePaths[0], (err,data) => {
			if(err){console.log(err)}
			//Dispatch the result back to the update function
			window.$dispatchRef(responseHandler(data))
		})
	}).catch(err => {
		console.log("actual error is: ")
		console.log
		console.log(errMsg) //CHANGE THIS TO A DIALOG MESSAGE BOX!
	})
}

export const showSaveDialog = stringData => errMsg => {
	fs.writeFile("output.txt", stringData, err => {
		if(err !== undefined){console.log(err)}
	})
}

export const writeFile = path => data => {
	console.log("In write file")
	console.log("path is:")
	console.log(path)
	console.log("data is:")
	console.log(data)
	fs.writeFile(path, data, err => {
		if(err !== undefined){console.log(err)}
	})
}

export const readFile = path => responseHandler => {
	console.log("Inside read file")
	fs.readFile(path, "utf-8", (err, data) => {
		if(err){console.log(err)}
		window.$dispatchRef(responseHandler(data))
	})
}

export const useSyphon = (updateFn,initState,effectFn) => {
	const [state, dispatch] = React.useReducer(updateFn, initState)
	//First send the event to the effec tfunction and then the update function
	const effectfulDispatch = event => {
		effectFn(state, event)
		dispatch(event)
	}
	return [state, effectfulDispatch]
}

export class NoEffect {}
export const _NoEffect = () => new NoEffect()