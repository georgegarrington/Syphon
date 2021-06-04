const {Record} = require("immutable")

var $errLog = []
var $shouldPrint = false
const $errMsg = msg => {$errLog.push(msg);if($shouldPrint){console.log(msg)}}


class PixelEntered{constructor(arg0, arg1){this.val0 = arg0; this.val1 = arg1}}
const _PixelEntered = arg0 => arg1 => new PixelEntered(arg0, arg1)
class MousePressed{}
const _MousePressed = () => new MousePressed()
class MouseReleased{}
const _MouseReleased = () => new MouseReleased()
class ColorChanged{constructor(arg0){this.val0 = arg0}}
const _ColorChanged = arg0 => new ColorChanged(arg0)
class Dummy{}
const _Dummy = () => new Dummy()
class Clear{}
const _Clear = () => new Clear()
class Undo{}
const _Undo = () => new Undo()

class State extends Record({dragging : undefined, color : undefined, strokes : undefined, undidStrokes : undefined}){}
const _State = arg0 => arg1 => arg2 => arg3 =>
	new State({dragging : arg0, color : arg1, strokes : arg2, undidStrokes : arg3})

class $EmptyList {}; const $_EmptyList = () => {return new $EmptyList()}
//The element in the list node, and a pointer to the next node in the list
class $Cons{constructor(arg0, arg1){this.val0 = arg0; this.val1 = arg1}}
const $_Cons = arg0 => arg1 => {return new $Cons(arg0, arg1)}

class Stroke {
	constructor(color,thickness,coordTuples){
		this.val0 = color
		this.val1 = thickness
		this.val2 = coordTuples
	}
}

const weight = 3

const _Stroke = arg0 => arg1 => arg2 => new Stroke(arg0,arg1,arg2)

const $index = arg0 => arg1 => {
	if(arg0 instanceof $EmptyList){
		return $errMsg("(!!) called on an empty list!")
	}
	if(arg1 < 0){
		return $errMsg("(!!) called with a negative index!")
	}
	if(arg1 == 0) {
		return arg0.val0
	} else {
		return $index(arg0.val1)(arg1 - 1)
	}
}

const length = arg0 => {
	if(arg0 instanceof $EmptyList){
		return 0
	} else {
		return 1 + length(arg0.val1)
	}
}

const drop = arg0 => arg1 => {
	if(arg0 > length(arg1)){
		return $errMsg("tried to drop too much from a list!")
	}
	if(arg0 < 0){
		return $errMsg("drop called with a negative amount!")
	}
	if(arg0 === 0){
		return arg1
	} else {
		return drop(arg0 - 1)(arg1.val1)
	}
}


const take = $arg0 => $arg1 => {
	const $name = "take"
	if($arg0 === 0){
		const xs = $arg1
		return $_EmptyList()
	}
	if(length($arg1) >= 1){
		const n = $arg0
		const x = $index($arg1)(0)
		const xs = drop(1)($arg1)
		return (() => {
				if($lt(n)(0)){
					return $errMsg("take called with a negative amount!")
				}
				if($gt(n)(length(xs))){
					return $errMsg("tried to take too much from a list!")
				}
				if(true){
					return $_Cons(x)(take($sub(n)(1))(xs))
				}
				$error("Uncovered case in a conditional expression!")
		})()
	}
	$error("Uncovered pattern case in function: take!")
}

const update = $arg0 => $arg1 => {
	const $name = "update"
	const s = $arg0
	const e = $arg1
	const addPointToStrokes = (arg0 => arg1 => arg2 => {
		if(!(($index(arg2)(0) instanceof Stroke) && (length(arg2) >= 1))){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const y = arg0
		const x = arg1
		const c = $index(arg2)(0).val0
		const weight = $index(arg2)(0).val1
		const tups = $index(arg2)(0).val2
		const strokes = drop(1)(arg2)

		return $_Cons(_Stroke(c)(weight)($_Cons([y,x])(tups)))(strokes)
	})
	
	const updateColor = (arg0 => arg1 => {
		if(!(($index(arg1)(0) instanceof Stroke) && (length(arg1) >= 1))){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const c = arg0
		const weight = $index(arg1)(0).val1
		const tups = $index(arg1)(0).val2
		const strokes = drop(1)(arg1)

		return $_Cons(_Stroke(c)(weight)(tups))(strokes)
	})
	
	const safeDrop = (arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const list = arg0

		return (() => {
				if(length(list) === 1){
					const empty = $index(list)(0)
					return $_Cons(empty)($_EmptyList())
				}
				if(length(list) >= 2){
					const empty = $index(list)(0)
					const rest = drop(2)(list)
					return $_Cons(empty)(rest)
				}
				$error("Uncovered match expression case in function:" + $name + "!")
			})()

	})
	
	return (() => {
		if(e instanceof PixelEntered){
			const y = e.val0
			const x = e.val1
			return s.set("strokes", addPointToStrokes(y)(x)(s.strokes))
		}
		if(e instanceof MousePressed){
			return s.set("dragging", true)
		}
		if(e instanceof MouseReleased){
			return s.mergeDeep({dragging : false, strokes : $_Cons(emptyStroke(s.color))(s.strokes)})
		}
		if(e instanceof ColorChanged){
			const c = e.val0
			return s.mergeDeep({strokes : updateColor(c)(s.strokes), color : c})
		}
		if(e instanceof Clear){
			return init
		}
		if(e instanceof Undo){
			return s.set("strokes", safeDrop(s.strokes))
		}
		if(e instanceof Dummy){
			return s
		}
		$error("Uncovered match expression case in function:" + $name + "!")
	})()

	$error("Uncovered pattern case in function: update!")
}


const emptyStroke = $arg0 => {
	const $name = "emptyStroke"
	const c = $arg0
	return _Stroke(c)(weight)($_EmptyList())
	$error("Uncovered pattern case in function: emptyStroke!")
}

const init = _State(false)("#000000")($_Cons(emptyStroke("#000000"))($_EmptyList()))($_EmptyList())


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
	console.log("RUNNING 1 TESTS FOR FUNCTION: emptyStroke")
	$sep()
	console.log("1:")
	console.log("\tInputs = Black, Expected return value: Stroke \"#000000\" 3 []")
	$verify(emptyStroke("#000000"),res => (res instanceof Stroke) && (res.val0 === "#000000") && (res.val1 === 3) && (res.val2 instanceof $EmptyList))
	$sep()
	console.log("RUNNING 8 TESTS FOR FUNCTION: update")
	$sep()
	console.log("1:")
	console.log("\tInputs = (State False Black [Stroke Black 3 []] []) (PixelEntered 1 2), Expected return value: = State False \"#000000\" [Stroke \"#000000\" 3 [(1,2)]] []")
	$verify(update(_State(false)("#000000")($_Cons(_Stroke("#000000")(3)($_EmptyList()))($_EmptyList()))($_EmptyList()))(_PixelEntered(1)(2)),res => (!res.dragging) && (res.color === "#000000") && ($index(res.strokes)(0) instanceof Stroke) && ($index(res.strokes)(0).val0 === "#000000") && ($index(res.strokes)(0).val1 === 3) && ($index($index(res.strokes)(0).val2)(0)[0] === 1) && ($index($index(res.strokes)(0).val2)(0)[1] === 2) && (length($index(res.strokes)(0).val2) === 1) && (length(res.strokes) === 1) && (res.undidStrokes instanceof $EmptyList))
	console.log("2:")
	console.log("\tInputs = (State False Black [Stroke Black 3 []] []) MousePressed, Expected return value: = State True \"#000000\" [Stroke \"#000000\" 3 []] []")
	$verify(update(_State(false)("#000000")($_Cons(_Stroke("#000000")(3)($_EmptyList()))($_EmptyList()))($_EmptyList()))(_MousePressed()),res => (res.dragging) && (res.color === "#000000") && ($index(res.strokes)(0) instanceof Stroke) && ($index(res.strokes)(0).val0 === "#000000") && ($index(res.strokes)(0).val1 === 3) && ($index(res.strokes)(0).val2 instanceof $EmptyList) && (length(res.strokes) === 1) && (res.undidStrokes instanceof $EmptyList))
	console.log("3:")
	console.log("\tInputs = (State True Black [Stroke Black 3 [(1,2),(3,4)]] []) MouseReleased, Expected return value: = State False \"#000000\" [Stroke \"#000000\" 3 [], Stroke Black 3 [(1,2),(3,4)]] []")
	$verify(update(_State(true)("#000000")($_Cons(_Stroke("#000000")(3)($_Cons([1,2])($_Cons([3,4])($_EmptyList()))))($_EmptyList()))($_EmptyList()))(_MouseReleased()),res => (!res.dragging) && (res.color === "#000000") && ($index(res.strokes)(0) instanceof Stroke) && ($index(res.strokes)(0).val0 === "#000000") && ($index(res.strokes)(0).val1 === 3) && ($index(res.strokes)(0).val2 instanceof $EmptyList) && ($index(res.strokes)(1) instanceof Stroke) && ($index(res.strokes)(1).val1 === 3) && ($index($index(res.strokes)(1).val2)(0)[0] === 1) && ($index($index(res.strokes)(1).val2)(0)[1] === 2) && ($index($index(res.strokes)(1).val2)(1)[0] === 3) && ($index($index(res.strokes)(1).val2)(1)[1] === 4) && (length($index(res.strokes)(1).val2) === 2) && (length(res.strokes) === 2) && (res.undidStrokes instanceof $EmptyList))
	console.log("4:")
	console.log("\tInputs = (State True Black [Stroke Black 3 []] []) (ColorChanged Blue), Expected return value: = State True \"#0000FF\" [Stroke \"#0000FF\" 3 []] []")
	$verify(update(_State(true)("#000000")($_Cons(_Stroke("#000000")(3)($_EmptyList()))($_EmptyList()))($_EmptyList()))(_ColorChanged("#0000FF")),res => (res.dragging) && (res.color === "#0000FF") && ($index(res.strokes)(0) instanceof Stroke) && ($index(res.strokes)(0).val0 === "#0000FF") && ($index(res.strokes)(0).val1 === 3) && ($index(res.strokes)(0).val2 instanceof $EmptyList) && (length(res.strokes) === 1) && (res.undidStrokes instanceof $EmptyList))
	console.log("5:")
	console.log("\tInputs = (State True Blue [Stroke Blue 3 [(1,2)],Stroke Blue 3 [(3,4)]] []) Clear, Expected return value: = State False \"#000000\" [Stroke \"#000000\" 3 []] []")
	$verify(update(_State(true)("#0000FF")($_Cons(_Stroke("#0000FF")(3)($_Cons([1,2])($_EmptyList())))($_Cons(_Stroke("#0000FF")(3)($_Cons([3,4])($_EmptyList())))($_EmptyList())))($_EmptyList()))(_Clear()),res => (!res.dragging) && (res.color === "#000000") && ($index(res.strokes)(0) instanceof Stroke) && ($index(res.strokes)(0).val0 === "#000000") && ($index(res.strokes)(0).val1 === 3) && ($index(res.strokes)(0).val2 instanceof $EmptyList) && (length(res.strokes) === 1) && (res.undidStrokes instanceof $EmptyList))
	console.log("6:")
	console.log("\tInputs = (State True Black [Stroke Blue 3 [(1,2)],Stroke Blue 3 [(3,4)]] []) Undo, Expected return value: = State True \"#000000\" [Stroke \"#0000FF\" 3 [(1,2)]] []")
	$verify(update(_State(true)("#000000")($_Cons(_Stroke("#0000FF")(3)($_Cons([1,2])($_EmptyList())))($_Cons(_Stroke("#0000FF")(3)($_Cons([3,4])($_EmptyList())))($_EmptyList())))($_EmptyList()))(_Undo()),res => (res.dragging) && (res.color === "#000000") && ($index(res.strokes)(0) instanceof Stroke) && ($index(res.strokes)(0).val0 === "#0000FF") && ($index(res.strokes)(0).val1 === 3) && ($index($index(res.strokes)(0).val2)(0)[0] === 1) && ($index($index(res.strokes)(0).val2)(0)[1] === 2) && (length($index(res.strokes)(0).val2) === 1) && (length(res.strokes) === 1) && (res.undidStrokes instanceof $EmptyList))
	console.log("7:")
	console.log("\tInputs = (State True Black [Stroke Black 3 []] []) Undo, Expected return value: = State True \"#000000\" [Stroke \"#000000\" 3 []] []")
	$verify(update(_State(true)("#000000")($_Cons(_Stroke("#000000")(3)($_EmptyList()))($_EmptyList()))($_EmptyList()))(_Undo()),res => (res.dragging) && (res.color === "#000000") && ($index(res.strokes)(0) instanceof Stroke) && ($index(res.strokes)(0).val0 === "#000000") && ($index(res.strokes)(0).val1 === 3) && ($index(res.strokes)(0).val2 instanceof $EmptyList) && (length(res.strokes) === 1) && (res.undidStrokes instanceof $EmptyList))
	console.log("8:")
	console.log("\tInputs = (State True Black [Stroke Black 3 []] []) Dummy, Expected return value: = State True \"#000000\" [Stroke \"#000000\" 3 []] []")
	$verify(update(_State(true)("#000000")($_Cons(_Stroke("#000000")(3)($_EmptyList()))($_EmptyList()))($_EmptyList()))(_Dummy()),res => (res.dragging) && (res.color === "#000000") && ($index(res.strokes)(0) instanceof Stroke) && ($index(res.strokes)(0).val0 === "#000000") && ($index(res.strokes)(0).val1 === 3) && ($index(res.strokes)(0).val2 instanceof $EmptyList) && (length(res.strokes) === 1) && (res.undidStrokes instanceof $EmptyList))
	$sep()
	console.log("TESTING COMPLETE")
	$sep()
	console.log(($successes + $failures) + " total tests ran")
	console.log($successes + " successes")
	console.log($failures + " failures")
}

$runSUnit()