const {Record} = require("immutable")

const append = arg0 => arg1 => arg0.concat(arg1)
const $add = x => y => x + y
const $sub = x => y => x - y
const $mul = x => y => x * y
const $div = x => y => x / y
const $mod = x => y => x % y

var $errLog = []
var $shouldPrint = false
const $errMsg = msg => {$errLog.push(msg);if($shouldPrint){console.log("\"" + msg + "\"")}}

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

const update = $arg0 => $arg1 => {
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

	$error("Uncovered pattern case in function: update!")
}

const init = _State("")($add)(0)


function $runSUnit(){
	let $successes = 0
	let $failures = 0
	const $isFailure = res => res === undefined || Number.isNaN(res) || res === null
	const $sep = () => console.log("--------------------------------------------------")
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
	$sep()
	console.log("RUNNING 7 TESTS FOR FUNCTION: update")
	$sep()
	console.log("1:")
	console.log("\tInputs = (update (State \"\" (+) 0) Clear) Clear, Expected return value: (State \"\" fn 0)")
	$verify(update(update(_State("")($add)(0))(_Clear()))(_Clear()),res => (res.display === "") && (res.operand1 === 0))
	console.log("2:")
	console.log("\tInputs = (State \"1\" (+) 2) Eq, Expected return value: (State \"3\" fn 0)")
	$verify(update(_State("1")($add)(2))(_Eq()),res => (res.display === "3") && (res.operand1 === 0))
	console.log("3:")
	console.log("\tInputs = (State \"1\" (-) 2) Eq, Expected return value: (State \"1\" fn 0)")
	$verify(update(_State("1")($sub)(2))(_Eq()),res => (res.display === "1") && (res.operand1 === 0))
	console.log("4:")
	console.log("\tInputs = (State \"3\" (*) 4) Eq, Expected return value: (State \"12\" fn 0)")
	$verify(update(_State("3")($mul)(4))(_Eq()),res => (res.display === "12") && (res.operand1 === 0))
	console.log("5:")
	console.log("\tInputs = (State \"5\" (/) 5) Eq, Expected return value: (State \"1\" fn 0)")
	$verify(update(_State("5")($div)(5))(_Eq()),res => (res.display === "1") && (res.operand1 === 0))
	console.log("6:")
	console.log("\tInputs = (State \"1\" (+) 0) (Digit 2), Expected return value: (State \"12\" fn 0) ")
	$verify(update(_State("1")($add)(0))(_Digit(2)),res => (res.display === "12") && (res.operand1 === 0))
	console.log("7:")
	console.log("\tInputs = (update (State \"1\" (+) 0) (Digit 2)) (OpPress (+)), Expected return value: (State \"\" fn 12)")
	$verify(update(update(_State("1")($add)(0))(_Digit(2)))(_OpPress($add)),res => (res.display === "") && (res.operand1 === 12))
	$sep()
	console.log("TESTING COMPLETE")
	$sep()
	console.log(($successes + $failures) + " total tests ran")
	console.log($successes + " successes")
	console.log($failures + " failures\n")
}

$runSUnit()
