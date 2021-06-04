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


export const Context = React.createContext()

class LeftPlayerUp{}
const _LeftPlayerUp = () => new LeftPlayerUp()
class LeftPlayerDown{}
const _LeftPlayerDown = () => new LeftPlayerDown()
class LeftPlayerRelease{}
const _LeftPlayerRelease = () => new LeftPlayerRelease()
class RightPlayerUp{}
const _RightPlayerUp = () => new RightPlayerUp()
class RightPlayerDown{}
const _RightPlayerDown = () => new RightPlayerDown()
class RightPlayerRelease{}
const _RightPlayerRelease = () => new RightPlayerRelease()
class Pause{}
const _Pause = () => new Pause()
class Tick{}
const _Tick = () => new Tick()
class LeftPlayer{}
const _LeftPlayer = () => new LeftPlayer()
class RightPlayer{}
const _RightPlayer = () => new RightPlayer()
class Top{}
const _Top = () => new Top()
class Bottom{}
const _Bottom = () => new Bottom()
class Lft{}
const _Lft = () => new Lft()
class Rght{}
const _Rght = () => new Rght()
class Playing{}
const _Playing = () => new Playing()
class Paused{}
const _Paused = () => new Paused()
class Loading{}
const _Loading = () => new Loading()
class NE{}
const _NE = () => new NE()
class SE{}
const _SE = () => new SE()
class SW{}
const _SW = () => new SW()
class NW{}
const _NW = () => new NW()
class Stationary{}
const _Stationary = () => new Stationary()
class GoingUp{}
const _GoingUp = () => new GoingUp()
class GoingDown{}
const _GoingDown = () => new GoingDown()

class State extends Record({leftPaddleTop : undefined, rightPaddleTop : undefined, leftPaddleState : undefined, rightPaddleState : undefined, leftScore : undefined, rightScore : undefined, ballY : undefined, ballX : undefined, ballDir : undefined, state : undefined, gen : undefined}){}
const _State = arg0 => arg1 => arg2 => arg3 => arg4 => arg5 => arg6 => arg7 => arg8 => arg9 => arg10 =>
	new State({leftPaddleTop : arg0, rightPaddleTop : arg1, leftPaddleState : arg2, rightPaddleState : arg3, leftScore : arg4, rightScore : arg5, ballY : arg6, ballX : arg7, ballDir : arg8, state : arg9, gen : arg10})

const init = _State(0)(0)(_Stationary())(_Stationary())(0)(0)(400)(400)(_NW())(_Playing())(mkRandomGen(0))


const update = ($arg0,$arg1) => {

	const $name = "update"
	const s = $arg0
	const e = $arg1
	return (() => {
		if(e instanceof Tick){
			return handleTick(s)
		}
		if(e instanceof LeftPlayerUp){
			return s.set("leftPaddleState", _GoingUp())
		}
		if(e instanceof LeftPlayerDown){
			return s.set("leftPaddleState", _GoingDown())
		}
		if(e instanceof LeftPlayerRelease){
			return s.set("leftPaddleState", _Stationary())
		}
		if(e instanceof RightPlayerUp){
			return s.set("rightPaddleState", _GoingUp())
		}
		if(e instanceof RightPlayerDown){
			return s.set("rightPaddleState", _GoingDown())
		}
		if(e instanceof RightPlayerRelease){
			return s.set("rightPaddleState", _Stationary())
		}
		if(e instanceof Pause){
			return s.set("state", _Paused())
		}
		$error("Uncovered match expression case in function:" + $name + "!")
	})()

}

export const App = () => {

	const [state,$dispatch] = React.useReducer(update,init)
	window.$dispatchRef = $dispatch
	React.useEffect(() => {
		let $time = undefined
		if(true){$time = setInterval(() => $dispatch(_Tick()),tickFreq)}
		return () => clearInterval($time)
	},[])
	React.useEffect(() => {
		function $handler(e){
			if(e.code === $keyExprToCode(_KeyChar('s'))){$dispatch(_LeftPlayerDown())}
			if(e.code === $keyExprToCode(_KeyChar('w'))){$dispatch(_LeftPlayerUp())}
			if(e.code === $keyExprToCode(_DownArrow())){$dispatch(_RightPlayerDown())}
			if(e.code === $keyExprToCode(_UpArrow())){$dispatch(_RightPlayerUp())}
		}
		if((true)){document.addEventListener("keydown",$handler)}
		return () => document.removeEventListener("keydown",$handler)
	},[])
	React.useEffect(() => {
		function $handler(e){
			if(e.code === $keyExprToCode(_KeyChar('s'))){$dispatch(_LeftPlayerRelease())}
			if(e.code === $keyExprToCode(_KeyChar('w'))){$dispatch(_LeftPlayerRelease())}
			if(e.code === $keyExprToCode(_DownArrow())){$dispatch(_RightPlayerRelease())}
			if(e.code === $keyExprToCode(_UpArrow())){$dispatch(_RightPlayerRelease())}
		}
		if((true)){document.addEventListener("keyup",$handler)}
		return () => document.removeEventListener("keyup",$handler)
	},[])
	return (<Context.Provider value = {$dispatch}>{(() => {

		return view(state)

	})()}</Context.Provider>)
}

const view = $arg0 => {
	const $name = "view"
	const s = $arg0
	return _Column({bgColor : "#000000"})($_Cons(_Row($_Cons(_Text({})(String(s.leftScore)))($_Cons(_Text({})(String(s.rightScore)))($_EmptyList()))))($_Cons(_Canvas({bgColor : "#000000"})(canvasWidth)(canvasHeight)($_Cons(_Oval("#FFFFFF")(s.ballY)(s.ballX)(ballDiam)(ballDiam))($_Cons(_Rectangle("#FFFFFF")(s.leftPaddleTop)(0)(paddleWidth)(paddleHeight))($_Cons(_Rectangle("#FFFFFF")(s.rightPaddleTop)($sub(canvasWidth)(paddleWidth))(paddleWidth)(paddleHeight))($_EmptyList())))))($_EmptyList())))
	$error("Uncovered pattern case in function: view!")
}

const handleTick = $arg0 => {
	const $name = "handleTick"
	const s = $arg0
	const goUp = (arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const val = arg0

		return (() => {
					if($lt($sub(val)(paddleSpeed))(0)){
						return 0
					}
					if(true){
						return $sub(val)(paddleSpeed)
					}
					$error("Uncovered case in a conditional expression!")
			})()

	})
	
	const goDown = (arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const val = arg0

		return (() => {
					if($gt($add($add(val)(paddleSpeed))(paddleHeight))(canvasHeight)){
						return $sub(canvasHeight)(paddleHeight)
					}
					if(true){
						return $add(val)(paddleSpeed)
					}
					$error("Uncovered case in a conditional expression!")
			})()

	})
	
	const calcNewTop = (arg0 => arg1 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const paddleState = arg0
		const top = arg1

		return (() => {
				if(paddleState instanceof GoingUp){
					return goUp(top)
				}
				if(paddleState instanceof GoingDown){
					return goDown(top)
				}
				if(paddleState instanceof Stationary){
					return top
				}
				$error("Uncovered match expression case in function:" + $name + "!")
			})()

	})
	
	const leftTop = calcNewTop(s.leftPaddleState)(s.leftPaddleTop)
	const rightTop = calcNewTop(s.rightPaddleState)(s.rightPaddleTop)
	const uncheckedY = (() => {
		if(s.ballDir instanceof NE){
			return [$sub(s.ballY)(1),$add(s.ballX)(1)]
		}
		if(s.ballDir instanceof SE){
			return [$add(s.ballY)(1),$add(s.ballX)(1)]
		}
		if(s.ballDir instanceof SW){
			return [$add(s.ballY)(1),$sub(s.ballX)(1)]
		}
		if(s.ballDir instanceof NW){
			return [$sub(s.ballY)(1),$sub(s.ballX)(1)]
		}
		$error("Uncovered match expression case in function:" + $name + "!")
	})()
[0]
const uncheckedX = (() => {
		if(s.ballDir instanceof NE){
			return [$sub(s.ballY)(1),$add(s.ballX)(1)]
		}
		if(s.ballDir instanceof SE){
			return [$add(s.ballY)(1),$add(s.ballX)(1)]
		}
		if(s.ballDir instanceof SW){
			return [$add(s.ballY)(1),$sub(s.ballX)(1)]
		}
		if(s.ballDir instanceof NW){
			return [$sub(s.ballY)(1),$sub(s.ballX)(1)]
		}
		$error("Uncovered match expression case in function:" + $name + "!")
	})()
[1]
	const leftBottom = $add(leftTop)(paddleHeight)
	const rightBottom = $add(rightTop)(paddleHeight)
	const bounce = (arg0 => arg1 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const wall = arg0
		const dir = arg1

		return (() => {
				if(([wall,dir][0] instanceof Top) && ([wall,dir][1] instanceof NE)){
					return [_SE(),0,uncheckedX]
				}
				if(([wall,dir][0] instanceof Top) && ([wall,dir][1] instanceof NW)){
					return [_SW(),0,uncheckedX]
				}
				if(([wall,dir][0] instanceof Bottom) && ([wall,dir][1] instanceof SE)){
					return [_NE(),$sub(canvasHeight)(ballDiam),uncheckedX]
				}
				if(([wall,dir][0] instanceof Bottom) && ([wall,dir][1] instanceof SW)){
					return [_NW(),$sub(canvasHeight)(ballDiam),uncheckedX]
				}
				if(([wall,dir][0] instanceof Lft) && ([wall,dir][1] instanceof NW)){
					return [_NE(),uncheckedY,paddleWidth]
				}
				if(([wall,dir][0] instanceof Lft) && ([wall,dir][1] instanceof SW)){
					return [_SE(),uncheckedY,paddleWidth]
				}
				if(([wall,dir][0] instanceof Rght) && ([wall,dir][1] instanceof NE)){
					return [_NW(),uncheckedY,$sub($sub(canvasWidth)(ballDiam))(paddleWidth)]
				}
				if(([wall,dir][0] instanceof Rght) && ([wall,dir][1] instanceof SE)){
					return [_SW(),uncheckedY,$sub($sub(canvasWidth)(ballDiam))(paddleWidth)]
				}
				$error("Uncovered match expression case in function:" + $name + "!")
			})()

	})
	
	const ballTop = s.ballY
	const ballBottom = $add(s.ballY)(ballDiam)
	const ballLeft = s.ballX
	const ballRight = $add(s.ballX)(ballDiam)
	const between = (arg0 => arg1 => arg2 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const val = arg0
		const lowerBound = arg1
		const upperBound = arg2

		return $and($lt(val)(upperBound))($gt(val)(lowerBound))
	})
	
	const iscLeftPad = $and($lt(ballLeft)(paddleWidth))($or(between(ballTop)(leftTop)(leftBottom))(between(ballBottom)(leftTop)(leftBottom)))
	const iscRightPad = $and($gt(ballRight)($sub(canvasWidth)(paddleWidth)))($or(between(ballTop)(rightTop)(rightBottom))(between(ballBottom)(rightTop)(rightBottom)))
	const iscTop = $lt(ballTop)(0)
	const iscBot = $gt(ballBottom)(canvasHeight)
	const iscLeftWall = $lt(ballLeft)(0)
	const iscRightWall = $gt(ballRight)(canvasWidth)
	const scoreOrNewBallInfo = (() => {
			if(iscLeftPad){
				return _Right(bounce(_Lft())(s.ballDir))
			}
			if(iscRightPad){
				return _Right(bounce(_Rght())(s.ballDir))
			}
			if(iscTop){
				return _Right(bounce(_Top())(s.ballDir))
			}
			if(iscBot){
				return _Right(bounce(_Bottom())(s.ballDir))
			}
			if(iscLeftWall){
				return _Left(_RightPlayer())
			}
			if(iscRightWall){
				return _Left(_LeftPlayer())
			}
			if(true){
				return _Right([s.ballDir,uncheckedY,uncheckedX])
			}
			$error("Uncovered case in a conditional expression!")
	})()

	const opposite = (arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const dir = arg0

		return (() => {
				if(dir instanceof NW){
					return _SE()
				}
				if(dir instanceof SE){
					return _NW()
				}
				if(dir instanceof NE){
					return _SW()
				}
				if(dir instanceof SW){
					return _NE()
				}
				$error("Uncovered match expression case in function:" + $name + "!")
			})()

	})
	
	return (() => {
		if(scoreOrNewBallInfo instanceof Right){
			const newDir = scoreOrNewBallInfo.val0[0]
			const newY = scoreOrNewBallInfo.val0[1]
			const newX = scoreOrNewBallInfo.val0[2]
			return s.mergeDeep({ballDir : newDir, ballY : newY, ballX : newX, leftPaddleTop : leftTop, rightPaddleTop : rightTop})
		}
		if((scoreOrNewBallInfo instanceof Left) && (scoreOrNewBallInfo.val0 instanceof RightPlayer)){
			return s.mergeDeep({rightScore : $add(s.rightScore)(1), ballY : 400, ballX : 400, ballDir : opposite(s.ballDir)})
		}
		if((scoreOrNewBallInfo instanceof Left) && (scoreOrNewBallInfo.val0 instanceof LeftPlayer)){
			return s.mergeDeep({leftScore : $add(s.leftScore)(1), ballY : 400, ballX : 400, ballDir : opposite(s.ballDir)})
		}
		$error("Uncovered match expression case in function:" + $name + "!")
	})()

	$error("Uncovered pattern case in function: handleTick!")
}

const tickFreq = 5

const paddleSpeed = 2

const ballSpeed = 1

const paddleHeight = 200

const paddleWidth = 40

const ballDiam = 20

const canvasHeight = 500

const canvasWidth = 800

