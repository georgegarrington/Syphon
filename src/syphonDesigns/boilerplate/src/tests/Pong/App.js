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

class State extends Record({leftPaddleTop : undefined, rightPaddleTop : undefined, leftPaddleState : undefined, rightPaddleState : undefined, leftScore : undefined, rightScore : undefined, ballY : undefined, ballX : undefined, ballDir : undefined, state : undefined}){}
const _State = arg0 => arg1 => arg2 => arg3 => arg4 => arg5 => arg6 => arg7 => arg8 => arg9 =>
	new State({leftPaddleTop : arg0, rightPaddleTop : arg1, leftPaddleState : arg2, rightPaddleState : arg3, leftScore : arg4, rightScore : arg5, ballY : arg6, ballX : arg7, ballDir : arg8, state : arg9})

const update = $arg0 => $arg1 => {
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

	$error("Uncovered pattern case in function: update!")
}

const init = _State(0)(0)(_Stationary())(_Stationary())(0)(0)(400)(400)(_NW())(_Playing())

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
				if(dir instanceof NE){
					return _SW()
				}
				if(dir instanceof SE){
					return _NE()
				}
				if(dir instanceof SW){
					return _NE()
				}
				if(dir instanceof NW){
					return _SE()
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
		if(scoreOrNewBallInfo instanceof Left){
			const RightPlayer = scoreOrNewBallInfo.val0
			return s.mergeDeep({rightScore : $add(s.rightScore)(1), ballY : 400, ballX : 400, ballDir : opposite(s.ballDir)})
		}
		if(scoreOrNewBallInfo instanceof Left){
			const LeftPlayer = scoreOrNewBallInfo.val0
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


function $runSUnit(){
	let $successes = 0
	let $failures = 0
	const $isFailure = res => res === undefined || Number.isNaN(res) || res === null
	const $sep = () => console.log("--------------------------------------------------")
	function $checkSatisfies(result, pred){
		if(pred(result)){
			console.log("\tSUCCESS: result satisfied predicate.")
			$successes++
		} else {
			console.log("\tFAILURE: result did not satisfy predicate.")
			$failures++
		}
	}
	function $verify(result, ptrnPred, err = undefined){
		if(err != undefined){
			const actualErr = $errLog.pop()
			if(actualErr === err){
				console.log("\tSUCCESS: failed with expected message.")
				$successes++
			} else {
				console.log("\tFAILURE: function did not fail with expected message.")
				console.log("\tActual error message: \"" + actualErr + "\"")
				$failures++
			}
		} else {
			if(ptrnPred(result)){
				console.log("\tSUCCESS: function returned expected value.")
				$successes++
			} else {
				console.log("\tFAILURE: function did not return expected value.")
				console.log("\tActual return value:")
				console.log(result)
				$failures++
			}
		}
	}
	const templateState = _State(0)(0)(_Stationary())(_Stationary())(0)(0)(0)(0)(_NW())(_Playing())
	const leftPlayerGoingUp = templateState.set("leftPaddleState", _GoingUp())
	const rightPlayerGoingUp = templateState.set("rightPaddleState", _GoingUp())
	$sep()
	console.log("RUNNING 7 TESTS FOR FUNCTION: update")
	$sep()
	console.log("1:")
	console.log("\tInputs = templateState LeftPlayerUp, Expecting satisfaction of: : \state -> state.leftPaddleState is GoingUp")
	$checkSatisfies(update(templateState)(_LeftPlayerUp()),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const state = arg0

		return state.leftPaddleState instanceof GoingUp
	})
	)
	console.log("2:")
	console.log("\tInputs = templateState LeftPlayerDown, Expecting satisfaction of: : \state -> state.leftPaddleState is GoingDown")
	$checkSatisfies(update(templateState)(_LeftPlayerDown()),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const state = arg0

		return state.leftPaddleState instanceof GoingDown
	})
	)
	console.log("3:")
	console.log("\tInputs = templateState RightPlayerUp, Expecting satisfaction of: : \state -> state.rightPaddleState is GoingUp")
	$checkSatisfies(update(templateState)(_RightPlayerUp()),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const state = arg0

		return state.rightPaddleState instanceof GoingUp
	})
	)
	console.log("4:")
	console.log("\tInputs = templateState RightPlayerDown, Expecting satisfaction of: : \state -> state.rightPaddleState is GoingDown")
	$checkSatisfies(update(templateState)(_RightPlayerDown()),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const state = arg0

		return state.rightPaddleState instanceof GoingDown
	})
	)
	console.log("5:")
	console.log("\tInputs = leftPlayerGoingUp LeftPlayerRelease, Expecting satisfaction of: : \state -> state.leftPaddleState is Stationary")
	$checkSatisfies(update(leftPlayerGoingUp)(_LeftPlayerRelease()),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const state = arg0

		return state.leftPaddleState instanceof Stationary
	})
	)
	console.log("6:")
	console.log("\tInputs = rightPlayerGoingUp RightPlayerRelease, Expecting satisfaction of: : \state -> state.rightPaddleState is Stationary")
	$checkSatisfies(update(rightPlayerGoingUp)(_RightPlayerRelease()),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const state = arg0

		return state.rightPaddleState instanceof Stationary
	})
	)
	console.log("7:")
	console.log("\tInputs = templateState Pause, Expecting satisfaction of: : \st -> st.state is Paused")
	$checkSatisfies(update(templateState)(_Pause()),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const st = arg0

		return st.state instanceof Paused
	})
	)
	$sep()
	console.log("RUNNING 49 TESTS FOR FUNCTION: handleTick")
	$sep()
	const leftAtTop = leftPlayerGoingUp
	const rightAtTop = rightPlayerGoingUp
	const leftLegalUp = leftPlayerGoingUp.set("leftPaddleTop", 200)
	const rightLegalUp = rightPlayerGoingUp.set("rightPaddleTop", 200)
	const leftLegalDown = leftLegalUp.set("leftPaddleState", _GoingDown())
	const rightLegalDown = rightLegalUp.set("rightPaddleState", _GoingDown())
	const leftAtBottom = leftLegalDown.set("leftPaddleTop", $sub(canvasHeight)(paddleHeight))
	const rightAtBottom = rightLegalDown.set("rightPaddleTop", $sub(canvasHeight)(paddleHeight))
	console.log("9:")
	console.log("\tInputs = leftAtTop, Expecting satisfaction of: : \st -> (&&)")
	$checkSatisfies(handleTick(leftAtTop),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const st = arg0

		return $eq($and(st.leftPaddleTop))(leftAtTop.leftPaddleTop(st.leftPaddleState instanceof GoingUp))
	})
	)
	console.log("10:")
	console.log("\tInputs = rightAtTop, Expecting satisfaction of: : \st -> (&&)")
	$checkSatisfies(handleTick(rightAtTop),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const st = arg0

		return $eq($and(st.rightPaddleTop))(rightAtTop.rightPaddleTop(st.rightPaddleState instanceof GoingUp))
	})
	)
	console.log("11:")
	console.log("\tInputs = leftLegalUp, Expecting satisfaction of: : \st -> ")
	$checkSatisfies(handleTick(leftLegalUp),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const st = arg0

		return $eq($sub(leftLegalUp.leftPaddleTop)(paddleSpeed))(st.leftPaddleTop)
	})
	)
	console.log("12:")
	console.log("\tInputs = leftLegalDown, Expecting satisfaction of: : \st -> ")
	$checkSatisfies(handleTick(leftLegalDown),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const st = arg0

		return $eq($add(leftLegalDown.leftPaddleTop)(paddleSpeed))(st.leftPaddleTop)
	})
	)
	console.log("13:")
	console.log("\tInputs = rightLegalUp, Expecting satisfaction of: : \st ->")
	$checkSatisfies(handleTick(rightLegalUp),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const st = arg0

		return $eq($sub(rightLegalUp.rightPaddleTop)(paddleSpeed))(st.rightPaddleTop)
	})
	)
	console.log("14:")
	console.log("\tInputs = rightLegalDown, Expecting satisfaction of: : \st ->")
	$checkSatisfies(handleTick(rightLegalDown),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const st = arg0

		return $eq($add(rightLegalDown.rightPaddleTop)(paddleSpeed))(st.rightPaddleTop)
	})
	)
	console.log("15:")
	console.log("\tInputs = leftAtBottom, Expecting satisfaction of: : \st -> (&&)")
	$checkSatisfies(handleTick(leftAtBottom),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const st = arg0

		return $eq($and(st.leftPaddleTop))(leftAtBottom.leftPaddleTop(st.leftPaddleState instanceof GoingDown))
	})
	)
	console.log("16:")
	console.log("\tInputs = rightAtBottom, Expecting satisfaction of: : \st -> (&&)")
	$checkSatisfies(handleTick(rightAtBottom),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const st = arg0

		return $eq($and(st.rightPaddleTop))(rigthAtBottom.rightPaddleTop(st.rightPaddleState instanceof GoingDown))
	})
	)
	const iscTopNW = templateState.set("dir", _NW())
	const iscTopNE = templateState.set("dir", _NE())
	const iscBotSE = templateState.set("dir", _SE())
	const iscBotSW = templateState.set("dir", _SW())
	const iscLeftPadNW = leftLegalUp.set("dir", _NW())
	const iscLeftPadSW = leftLegalUp.set("dir", _SW())
	const iscRightPadNE = rightLegalUp.set("dir", _NE())
	const iscRightPadSE = rightLegalUp.set("dir", _SE())
	const iscLeftWallNW = leftLegalUp.set("dir", _NW())
	const iscLeftWallSW = leftLegalUp.set("dir", _SW())
	const iscRightWallNE = rightLegalUp.set("dir", _NE())
	const iscRightWallSE = rightLegalUp.set("dir", _SE())
	console.log("29:")
	console.log("\tInputs = iscTopNW, Expecting satisfaction of: : \st -> (st.ballX == iscTopNW.ballX && st.ballY == 0) && st.dir is SW")
	$checkSatisfies(handleTick(iscTopNW),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const st = arg0

		return $and($and($eq(st.ballX)(iscTopNW.ballX))($eq(st.ballY)(0)))(st.dir instanceof SW)
	})
	)
	console.log("30:")
	console.log("\tInputs = iscTopNE, Expecting satisfaction of: : \st -> (st.ballX == iscTopNW.ballX && st.ballY == 0) && st.dir is SE")
	$checkSatisfies(handleTick(iscTopNE),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const st = arg0

		return $and($and($eq(st.ballX)(iscTopNW.ballX))($eq(st.ballY)(0)))(st.dir instanceof SE)
	})
	)
	console.log("31:")
	console.log("\tInputs = iscBotSE, Expecting satisfaction of: : \st -> (st.ballX == iscBotSE.ballX && st.ballY == (canvasHeight - ballDiam)) && st.dir is NE")
	$checkSatisfies(handleTick(iscBotSE),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const st = arg0

		return $and($and($eq(st.ballX)(iscBotSE.ballX))($eq(st.ballY)($sub(canvasHeight)(ballDiam))))(st.dir instanceof NE)
	})
	)
	console.log("32:")
	console.log("\tInputs = iscBotSW, Expecting satisfaction of: : \st -> (st.ballX == iscBotSW.ballX && st.ballY == (canvasHeight - ballDiam)) && st.dir is NW")
	$checkSatisfies(handleTick(iscBotSW),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const st = arg0

		return $and($and($eq(st.ballX)(iscBotSW.ballX))($eq(st.ballY)($sub(canvasHeight)(ballDiam))))(st.dir instanceof NW)
	})
	)
	console.log("33:")
	console.log("\tInputs = iscLeftPadNW, Expecting satisfaction of: : \st -> (st.ballY == iscLeftPadNW.ballY && st.ballX == ballDiam) && st.dir is NE")
	$checkSatisfies(handleTick(iscLeftPadNW),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const st = arg0

		return $and($and($eq(st.ballY)(iscLeftPadNW.ballY))($eq(st.ballX)(ballDiam)))(st.dir instanceof NE)
	})
	)
	console.log("34:")
	console.log("\tInputs = iscLeftPadSW, Expecting satisfaction of: : \st -> (st.ballY == iscLeftPadSW.ballY && st.ballX == ballDiam) && st.dir is SE")
	$checkSatisfies(handleTick(iscLeftPadSW),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const st = arg0

		return $and($and($eq(st.ballY)(iscLeftPadSW.ballY))($eq(st.ballX)(ballDiam)))(st.dir instanceof SE)
	})
	)
	console.log("35:")
	console.log("\tInputs = iscRightPadNE, Expecting satisfaction of: : \st -> (st.ballY == iscRightPadNE.ballY && st.ballX == (canvasWidth - ballDiam)) && st.dir is NW")
	$checkSatisfies(handleTick(iscRightPadNE),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const st = arg0

		return $and($and($eq(st.ballY)(iscRightPadNE.ballY))($eq(st.ballX)($sub(canvasWidth)(ballDiam))))(st.dir instanceof NW)
	})
	)
	console.log("36:")
	console.log("\tInputs = iscRightPadSE, Expecting satisfaction of: : \st -> (st.ballY == iscRightPadSE.ballY && st.ballX == (canvasWidth - ballDiam)) && st.dir is SW")
	$checkSatisfies(handleTick(iscRightPadSE),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const st = arg0

		return $and($and($eq(st.ballY)(iscRightPadSE.ballY))($eq(st.ballX)($sub(canvasWidth)(ballDiam))))(st.dir instanceof SW)
	})
	)
	console.log("37:")
	console.log("\tInputs = iscLeftWallNW, Expecting satisfaction of: : \st -> st.rightScore + 1 == iscLeftWallNW.rightScore")
	$checkSatisfies(handleTick(iscLeftWallNW),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const st = arg0

		return $eq($add(st.rightScore)(1))(iscLeftWallNW.rightScore)
	})
	)
	console.log("38:")
	console.log("\tInputs = iscLeftWallSW, Expecting satisfaction of: : \st -> st.rightScore + 1 == iscLeftWallSW.rightScore")
	$checkSatisfies(handleTick(iscLeftWallSW),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const st = arg0

		return $eq($add(st.rightScore)(1))(iscLeftWallSW.rightScore)
	})
	)
	console.log("39:")
	console.log("\tInputs = iscRightWallNE, Expecting satisfaction of: : \st -> st.leftScore + 1 == iscRightWallNW.leftScore")
	$checkSatisfies(handleTick(iscRightWallNE),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const st = arg0

		return $eq($add(st.leftScore)(1))(iscRightWallNW.leftScore)
	})
	)
	console.log("40:")
	console.log("\tInputs = iscRightWallSE, Expecting satisfaction of: : \st -> st.leftScore + 1 == iscRightWallSE.leftScore")
	$checkSatisfies(handleTick(iscRightWallSE),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const st = arg0

		return $eq($add(st.leftScore)(1))(iscRightWallSE.leftScore)
	})
	)
	const inCenterNW = templateState.mergeDeep({ballY : 400, ballX : 400})
	const inCenterNE = inCenterNW.set("dir", _NE())
	const inCenterSE = inCenterNW.set("dir", _SE())
	const inCenterSW = inCenterNW.set("dir", _SW())
	const calcBallTick = (arg0 => arg1 => arg2 => {
	if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
	const oldY = arg0
	const oldX = arg1
	const dir = arg2

	return (() => {
			if(dir instanceof NW){
				return [$sub(oldY)(ballSpeed),$sub(oldX)(ballSpeed)]
			}
			if(dir instanceof NE){
				return [$sub(oldY)(ballSpeed),$add(oldX)(ballSpeed)]
			}
			if(dir instanceof SE){
				return [$add(oldY)(ballSpeed),$add(oldX)(ballSpeed)]
			}
			if(dir instanceof SW){
				return [$add(oldY)(ballSpeed),$sub(oldX)(ballSpeed)]
			}
			$error("Uncovered match expression case in function:" + $name + "!")
		})()

})

	console.log("46:")
	console.log("\tInputs = inCenterNW, Expecting satisfaction of: : \st -> let")
	$checkSatisfies(handleTick(inCenterNW),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const st = arg0

		return (() => {
			if(!(true)){$error("Uncovered pattern in a let expression in function:", $name, "!!"); return}
				const expectedY = calcBallTick(inCenterNW.ballY)(inCenterNW.ballX)[0]
				const expectedX = calcBallTick(inCenterNW.ballY)(inCenterNW.ballX)[1]

				return $and($eq(st.ballY)(expectedY))($eq(st.ballX)(expectedX))
			})()
			
	})
	)
	console.log("47:")
	console.log("\tInputs = inCenterNE, Expecting satisfaction of: : \st -> let")
	$checkSatisfies(handleTick(inCenterNE),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const st = arg0

		return (() => {
			if(!(true)){$error("Uncovered pattern in a let expression in function:", $name, "!!"); return}
				const expectedY = calcBallTick(inCenterNE.ballY)(inCenterNE.ballX)[0]
				const expectedX = calcBallTick(inCenterNE.ballY)(inCenterNE.ballX)[1]

				return $and($eq(st.ballY)(expectedY))($eq(st.ballX)(expectedX))
			})()
			
	})
	)
	console.log("48:")
	console.log("\tInputs = inCenterSE, Expecting satisfaction of: : \st -> let")
	$checkSatisfies(handleTick(inCenterSE),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const st = arg0

		return (() => {
			if(!(true)){$error("Uncovered pattern in a let expression in function:", $name, "!!"); return}
				const expectedY = calcBallTick(inCenterSE.ballY)(inCenterSE.ballX)[0]
				const expectedX = calcBallTick(inCenterSE.ballY)(inCenterSE.ballX)[1]

				return $and($eq(st.ballY)(expectedY))($eq(st.ballX)(expectedX))
			})()
			
	})
	)
	console.log("49:")
	console.log("\tInputs = inCenterSW, Expecting satisfaction of: : \st -> let")
	$checkSatisfies(handleTick(inCenterSW),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const st = arg0

		return (() => {
			if(!(true)){$error("Uncovered pattern in a let expression in function:", $name, "!!"); return}
				const expectedY = calcBallTick(inCenterSW.ballY)(inCenterSW.ballX)[0]
				const expectedX = calcBallTick(inCenterSW.ballY)(inCenterSW.ballX)[1]

				return $and($eq(st.ballY)(expectedY))($eq(st.ballX)(expectedX))
			})()
			
	})
	)
	$sep()
	console.log("TESTING COMPLETE")
	$sep()
	console.log(($successes + $failures) + " total tests ran")
	console.log($successes + " successes")
	console.log($failures + " failures\n")
}