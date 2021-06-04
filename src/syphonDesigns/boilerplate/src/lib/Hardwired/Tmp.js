import prand from "pure-rand"
import {$EmptyList, $Cons} from "../Data/List"
import {dialog} from "electron"

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

//STRING
export const append = arg0 => arg1 => arg0.concat(arg1)

//UTILITY
export class KeyChar {constructor(arg0){this.val0 = arg0}}
export const _KeyChar = arg0 => new KeyChar(arg0)
export class DownArrow {}
export const _DownArrow = () => new DownArrow()
export class UpArrow {}
export const _UpArrow = () => new UpArrow()
export class ArrowLeft {}
export const _ArrowLeft = () => new ArrowLeft()
export class ArrowRight {}
export const _ArrowRight = () => new ArrowRight()

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

export const sayHello = () => console.log("hello :)")