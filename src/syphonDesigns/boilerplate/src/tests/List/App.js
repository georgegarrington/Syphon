import React from "react"
import {$add,$sub,$mul,$div,$mod,$and,$or,not,$eq,$nEq,$gt,$lt,$gtEq,$ltEq,$listGen,
	Just, Nothing, _Just, _Nothing, $Unit, $_Unit, fst, snd, $keyExprToCode, _UpArrow, _DownArrow, _LeftArrow,
	_RightArrow, _KeyChar, Left, Right, _Left, _Right} from "../../lib/Hardwired/Core.js"
import {$EmptyList, $_EmptyList, $Cons, $_Cons, $append,
	 singleton, $syphon2js, $js2syphon, $syphon2js2D, $js2syphon2D} from "../../lib/Data/List.js"

var $errLog = []
var $shouldPrint = false
const $errMsg = msg => {$errLog.push(msg);if($shouldPrint){console.log("\"" + msg + "\"")}}

const id = x => x

export const length = arg0 => {
	if(arg0 instanceof $EmptyList){
		return 0
	}
	else {
		return 1 + length(arg0.val1)
	}
}

export const $index = arg0 => arg1 => {
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

export const drop = arg0 => arg1 => {
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


export const replicateGrid = $arg0 => $arg1 => {
	const $name = "replicateGrid"
	const n = $arg0
	const thing = $arg1
	return replicate(n)(replicate(n)(thing))
	$error("Uncovered pattern case in function: replicateGrid!")
}

export const replicate = $arg0 => $arg1 => {
	const $name = "replicate"
	const n = $arg0
	const thing = $arg1
	return (() => {
			if($eq(n)(0)){
				return $_EmptyList()
			}
			if($lt(n)(0)){
				return $errMsg("replicate called with a negative amount!")
			}
			if(true){
				return $_Cons(thing)(replicate($sub(n)(1))(thing))
			}
			$error("Uncovered case in a conditional expression!")
	})()

	$error("Uncovered pattern case in function: replicate!")
}

export const concat = $arg0 => {
	const $name = "concat"
	if($arg0 instanceof $EmptyList){
		return $_EmptyList()
	}
	if(true){
		const xss = $arg0
		return reduce($append)(xss)
	}
	$error("Uncovered pattern case in function: concat!")
}

export const reduce = $arg0 => $arg1 => {
	const $name = "reduce"
	if($arg1 instanceof $EmptyList){
		return $errMsg("reduce called on an empty list!")
	}
	if(length($arg1) === 1){
		const x = $index($arg1)(0)
		return x
	}
	if(length($arg1) >= 2){
		const f = $arg0
		const x1 = $index($arg1)(0)
		const x2 = $index($arg1)(1)
		const xs = drop(2)($arg1)
		return reduce(f)($_Cons(f(x1)(x2))(xs))
	}
	$error("Uncovered pattern case in function: reduce!")
}

export const allSame = $arg0 => {
	const $name = "allSame"
	const xs = $arg0
	return and(map((arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const x = arg0

		return $eq(x)(head(xs))
	})
	)(xs))
	$error("Uncovered pattern case in function: allSame!")
}

export const or = $arg0 => {
	const $name = "or"
	if($arg0 instanceof $EmptyList){
		return false
	}
	if(length($arg0) >= 1){
		const x = $index($arg0)(0)
		const xs = drop(1)($arg0)
		return $or(x)(or(xs))
	}
	$error("Uncovered pattern case in function: or!")
}

export const and = $arg0 => {
	const $name = "and"
	if($arg0 instanceof $EmptyList){
		return true
	}
	if(length($arg0) >= 1){
		const x = $index($arg0)(0)
		const xs = drop(1)($arg0)
		return $and(x)(and(xs))
	}
	$error("Uncovered pattern case in function: and!")
}

export const filter2D = $arg0 => $arg1 => {
	const $name = "filter2D"
	const pred = $arg0
	const matrix = $arg1
	return map(filter(pred))(matrix)
	$error("Uncovered pattern case in function: filter2D!")
}

export const index2D = $arg0 => $arg1 => $arg2 => {
	const $name = "index2D"
	const list2D = $arg0
	const y = $arg1
	const x = $arg2
	return (() => {
			if(isEmpty(list2D)){
				return $errMsg("index2D called with an empty matrix!")
			}
			if($or($lt(y)(0))($lt(x)(0))){
				return $errMsg("index2D called with a negative index!")
			}
			if($gtEq(y)(length(list2D))){
				return $errMsg("illegal y index in index2D, too large!")
			}
			if($gtEq(x)(length($index(list2D)(y)))){
				return $errMsg("illegal x index in index2D, too large!")
			}
			if(true){
				return $index($index(list2D)(y))(x)
			}
			$error("Uncovered case in a conditional expression!")
	})()

	$error("Uncovered pattern case in function: index2D!")
}

export const replace2D = $arg0 => $arg1 => $arg2 => $arg3 => {
	const $name = "replace2D"
	const list2D = $arg0
	const y = $arg1
	const x = $arg2
	const elem = $arg3
	return (() => {
			if(isEmpty(list2D)){
				return $errMsg("replace2D called with an empty matrix!")
			}
			if($or($lt(y)(0))($lt(x)(0))){
				return $errMsg("replace2D called with a negative index!")
			}
			if($gtEq(y)(length(list2D))){
				return $errMsg("illegal y index in replace2D, too large!")
			}
			if($gtEq(x)(length($index(list2D)(y)))){
				return $errMsg("illegal x index in replace2D, too large!")
			}
			if(true){
				return $append($append(take(y)(list2D))($_Cons(replace($index(list2D)(y))(x)(elem))($_EmptyList())))(drop($add(y)(1))(list2D))
			}
			$error("Uncovered case in a conditional expression!")
	})()

	$error("Uncovered pattern case in function: replace2D!")
}

export const map2D = $arg0 => $arg1 => {
	const $name = "map2D"
	const f = $arg0
	const matrix = $arg1
	return map(map(f))(matrix)
	$error("Uncovered pattern case in function: map2D!")
}

export const replace = $arg0 => $arg1 => $arg2 => {
	const $name = "replace"
	const xs = $arg0
	const i = $arg1
	const x = $arg2
	return (() => {
			if(isEmpty(xs)){
				return $errMsg("replace called on an empty list!")
			}
			if($lt(i)(0)){
				return $errMsg("replace called with a negative index!")
			}
			if($gt(i)(length(xs))){
				return $errMsg("illegal index in replace, too large!")
			}
			if(true){
				return $append($append(take(i)(xs))($_Cons(x)($_EmptyList())))(drop($add(i)(1))(xs))
			}
			$error("Uncovered case in a conditional expression!")
	})()

	$error("Uncovered pattern case in function: replace!")
}

export const filter = $arg0 => $arg1 => {
	const $name = "filter"
	if($arg1 instanceof $EmptyList){
		return $_EmptyList()
	}
	if(length($arg1) >= 1){
		const pred = $arg0
		const x = $index($arg1)(0)
		const xs = drop(1)($arg1)
		return (() => {
				if(pred(x)){
					return $_Cons(x)(filter(pred)(xs))
				}
				if(true){
					return filter(pred)(xs)
				}
				$error("Uncovered case in a conditional expression!")
		})()

	}
	$error("Uncovered pattern case in function: filter!")
}

export const isEmpty = $arg0 => {
	const $name = "isEmpty"
	if($arg0 instanceof $EmptyList){
		return true
	}
	if(true){
		return false
	}
	$error("Uncovered pattern case in function: isEmpty!")
}

export const head = $arg0 => {
	const $name = "head"
	if(length($arg0) >= 1){
		const x = $index($arg0)(0)
		const xs = drop(1)($arg0)
		return x
	}
	if($arg0 instanceof $EmptyList){
		return $errMsg("tried to get head of an empty list!")
	}
	$error("Uncovered pattern case in function: head!")
}

export const tail = $arg0 => {
	const $name = "tail"
	if(length($arg0) >= 1){
		const x = $index($arg0)(0)
		const xs = drop(1)($arg0)
		return xs
	}
	if($arg0 instanceof $EmptyList){
		return $errMsg("tail called on an empty list!")
	}
	$error("Uncovered pattern case in function: tail!")
}

export const take = $arg0 => $arg1 => {
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

export const map = $arg0 => $arg1 => {
	const $name = "map"
	if($arg1 instanceof $EmptyList){
		return $_EmptyList()
	}
	if(length($arg1) >= 1){
		const f = $arg0
		const x = $index($arg1)(0)
		const xs = drop(1)($arg1)
		return $_Cons(f(x))(map(f)(xs))
	}
	$error("Uncovered pattern case in function: map!")
}

export const append = $arg0 => $arg1 => {
	const $name = "append"
	if($arg0 instanceof $EmptyList){
		const ys = $arg1
		return ys
	}
	if(length($arg0) >= 1){
		const x = $index($arg0)(0)
		const xs = drop(1)($arg0)
		const ys = $arg1
		return $_Cons(x)(append(xs)(ys))
	}
	$error("Uncovered pattern case in function: append!")
}


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
	console.log("RUNNING 5 TESTS FOR FUNCTION: append")
	$sep()
	console.log("1:")
	console.log("\tInputs = [] [], Expected return value: []")
	$verify(append($_EmptyList())($_EmptyList()),res => res instanceof $EmptyList)
	console.log("2:")
	console.log("\tInputs = [1] [], Expected return value: [1]")
	$verify(append($_Cons(1)($_EmptyList()))($_EmptyList()),res => ($index(res)(0) === 1) && (length(res) === 1))
	console.log("3:")
	console.log("\tInputs = [] [1], Expected return value: [1]")
	$verify(append($_EmptyList())($_Cons(1)($_EmptyList())),res => ($index(res)(0) === 1) && (length(res) === 1))
	console.log("4:")
	console.log("\tInputs = [1] [2], Expected return value: [1,2]")
	$verify(append($_Cons(1)($_EmptyList()))($_Cons(2)($_EmptyList())),res => ($index(res)(0) === 1) && ($index(res)(1) === 2) && (length(res) === 2))
	console.log("5:")
	console.log("\tInputs = [[]] [[]], Expected return value: [[],[]]")
	$verify(append($_Cons($_EmptyList())($_EmptyList()))($_Cons($_EmptyList())($_EmptyList())),res => ($index(res)(0) instanceof $EmptyList) && ($index(res)(1) instanceof $EmptyList) && (length(res) === 2))
	$sep()
	console.log("RUNNING 3 TESTS FOR FUNCTION: map")
	$sep()
	console.log("1:")
	console.log("\tInputs = id [], Expected return value: []")
	$verify(map(id)($_EmptyList()),res => res instanceof $EmptyList)
	console.log("2:")
	console.log("\tInputs = (\\x -> x + 1) [1,2], Expected return value: [2,3]")
	$verify(map((arg0 => {
			if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
			const x = arg0

			return $add(x)(1)
		})
		)($_Cons(1)($_Cons(2)($_EmptyList()))),res => ($index(res)(0) === 2) && ($index(res)(1) === 3) && (length(res) === 2))
	console.log("3:")
	console.log("\tInputs = length [[1,2],[]], Expected return value: [2,0]")
	$verify(map(length)($_Cons($_Cons(1)($_Cons(2)($_EmptyList())))($_Cons($_EmptyList())($_EmptyList()))),res => ($index(res)(0) === 2) && ($index(res)(1) === 0) && (length(res) === 2))
	$sep()
	console.log("RUNNING 4 TESTS FOR FUNCTION: $index")
	$sep()
	console.log("1:")
	console.log("\tInputs = [] 0, Expected error message: \"(!!) called on an empty list!\"")
	$verify($index($_EmptyList())(0),undefined, "(!!) called on an empty list!")
	console.log("2:")
	console.log("\tInputs = [1] (-1), Expected error message: \"(!!) called with a negative index!\"")
	$verify($index($_Cons(1)($_EmptyList()))(-1),undefined, "(!!) called with a negative index!")
	console.log("3:")
	console.log("\tInputs = [1] 0, Expected return value: 1")
	$verify($index($_Cons(1)($_EmptyList()))(0),res => res === 1)
	console.log("4:")
	console.log("\tInputs = [4,5,6] 2, Expected return value: 6")
	$verify($index($_Cons(4)($_Cons(5)($_Cons(6)($_EmptyList()))))(2),res => res === 6)
	$sep()
	console.log("RUNNING 4 TESTS FOR FUNCTION: drop")
	$sep()
	console.log("1:")
	console.log("\tInputs = (-1) [], Expected error message: \"drop called with a negative amount!\"")
	$verify(drop(-1)($_EmptyList()),undefined, "drop called with a negative amount!")
	console.log("2:")
	console.log("\tInputs = 3 [1,2], Expected error message: \"tried to drop too much from a list!\"")
	$verify(drop(3)($_Cons(1)($_Cons(2)($_EmptyList()))),undefined, "tried to drop too much from a list!")
	console.log("3:")
	console.log("\tInputs = 0 [], Expected return value: []")
	$verify(drop(0)($_EmptyList()),res => res instanceof $EmptyList)
	console.log("4:")
	console.log("\tInputs = 1 [1,2], Expected return value: [2]")
	$verify(drop(1)($_Cons(1)($_Cons(2)($_EmptyList()))),res => ($index(res)(0) === 2) && (length(res) === 1))
	$sep()
	console.log("RUNNING 4 TESTS FOR FUNCTION: take")
	$sep()
	console.log("1:")
	console.log("\tInputs = (-1) [1,2], Expected error message: \"take called with a negative amount!\"")
	$verify(take(-1)($_Cons(1)($_Cons(2)($_EmptyList()))),undefined, "take called with a negative amount!")
	console.log("2:")
	console.log("\tInputs = 3 [1,2], Expected error message: \"tried to take too much from a list!\"")
	$verify(take(3)($_Cons(1)($_Cons(2)($_EmptyList()))),undefined, "tried to take too much from a list!")
	console.log("3:")
	console.log("\tInputs = 0 [], Expected return value: []")
	$verify(take(0)($_EmptyList()),res => res instanceof $EmptyList)
	console.log("4:")
	console.log("\tInputs = 1 [[],[]], Expected return value: [[]]")
	$verify(take(1)($_Cons($_EmptyList())($_Cons($_EmptyList())($_EmptyList()))),res => ($index(res)(0) instanceof $EmptyList) && (length(res) === 1))
	$sep()
	console.log("RUNNING 3 TESTS FOR FUNCTION: tail")
	$sep()
	console.log("1:")
	console.log("\tInputs = [], Expected error message: \"tail called on an empty list!\"")
	$verify(tail($_EmptyList()),undefined, "tail called on an empty list!")
	console.log("2:")
	console.log("\tInputs = [1,2,3], Expected return value: [2,3]")
	$verify(tail($_Cons(1)($_Cons(2)($_Cons(3)($_EmptyList())))),res => ($index(res)(0) === 2) && ($index(res)(1) === 3) && (length(res) === 2))
	console.log("3:")
	console.log("\tInputs = [[],[]], Expected return value: [[]]")
	$verify(tail($_Cons($_EmptyList())($_Cons($_EmptyList())($_EmptyList()))),res => ($index(res)(0) instanceof $EmptyList) && (length(res) === 1))
	$sep()
	console.log("RUNNING 3 TESTS FOR FUNCTION: head")
	$sep()
	console.log("1:")
	console.log("\tInputs = [], Expected error message: \"tried to get head of an empty list!\"")
	$verify(head($_EmptyList()),undefined, "tried to get head of an empty list!")
	console.log("2:")
	console.log("\tInputs = [1,2], Expected return value: 1")
	$verify(head($_Cons(1)($_Cons(2)($_EmptyList()))),res => res === 1)
	console.log("3:")
	console.log("\tInputs = [[],[]], Expected return value: []")
	$verify(head($_Cons($_EmptyList())($_Cons($_EmptyList())($_EmptyList()))),res => res instanceof $EmptyList)
	$sep()
	console.log("RUNNING 2 TESTS FOR FUNCTION: isEmpty")
	$sep()
	console.log("1:")
	console.log("\tInputs = [], Expected return value: True")
	$verify(isEmpty($_EmptyList()),res => res)
	console.log("2:")
	console.log("\tInputs = [1,2], Expected return value: False")
	$verify(isEmpty($_Cons(1)($_Cons(2)($_EmptyList()))),res => !res)
	$sep()
	console.log("RUNNING 2 TESTS FOR FUNCTION: filter")
	$sep()
	console.log("1:")
	console.log("\tInputs = (\\x -> x > 2) [], Expected return value: []")
	$verify(filter((arg0 => {
			if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
			const x = arg0

			return $gt(x)(2)
		})
		)($_EmptyList()),res => res instanceof $EmptyList)
	console.log("2:")
	console.log("\tInputs = (\\x -> x > 2) [3,1,4,0], Expected return value: [3,4]")
	$verify(filter((arg0 => {
			if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
			const x = arg0

			return $gt(x)(2)
		})
		)($_Cons(3)($_Cons(1)($_Cons(4)($_Cons(0)($_EmptyList()))))),res => ($index(res)(0) === 3) && ($index(res)(1) === 4) && (length(res) === 2))
	$sep()
	console.log("RUNNING 3 TESTS FOR FUNCTION: filter2D")
	$sep()
	console.log("1:")
	console.log("\tInputs = (\\x -> x > 2) [], Expected return value: []")
	$verify(filter2D((arg0 => {
			if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
			const x = arg0

			return $gt(x)(2)
		})
		)($_EmptyList()),res => res instanceof $EmptyList)
	console.log("2:")
	console.log("\tInputs = (\\x -> x > 2) [[1]], Expected return value: [[]]")
	$verify(filter2D((arg0 => {
			if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
			const x = arg0

			return $gt(x)(2)
		})
		)($_Cons($_Cons(1)($_EmptyList()))($_EmptyList())),res => ($index(res)(0) instanceof $EmptyList) && (length(res) === 1))
	console.log("3:")
	console.log("\tInputs = (\\x -> x > 2) [[1,3,0],[0,2,4]], Expected return value: [[3],[4]]")
	$verify(filter2D((arg0 => {
			if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
			const x = arg0

			return $gt(x)(2)
		})
		)($_Cons($_Cons(1)($_Cons(3)($_Cons(0)($_EmptyList()))))($_Cons($_Cons(0)($_Cons(2)($_Cons(4)($_EmptyList()))))($_EmptyList()))),res => ($index($index(res)(0))(0) === 3) && (length($index(res)(0)) === 1) && ($index($index(res)(1))(0) === 4) && (length($index(res)(1)) === 1) && (length(res) === 2))
	$sep()
	console.log("RUNNING 3 TESTS FOR FUNCTION: map2D")
	$sep()
	console.log("1:")
	console.log("\tInputs = length [], Expected return value: []")
	$verify(map2D(length)($_EmptyList()),res => res instanceof $EmptyList)
	console.log("2:")
	console.log("\tInputs = length [[[]]], Expected return value: [[0]]")
	$verify(map2D(length)($_Cons($_Cons($_EmptyList())($_EmptyList()))($_EmptyList())),res => ($index($index(res)(0))(0) === 0) && (length($index(res)(0)) === 1) && (length(res) === 1))
	console.log("3:")
	console.log("\tInputs = (\\x -> x * 2) [[1,2],[3,4]], Expected return value: [[2,4],[6,8]]")
	$verify(map2D((arg0 => {
			if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
			const x = arg0

			return $mul(x)(2)
		})
		)($_Cons($_Cons(1)($_Cons(2)($_EmptyList())))($_Cons($_Cons(3)($_Cons(4)($_EmptyList())))($_EmptyList()))),res => ($index($index(res)(0))(0) === 2) && ($index($index(res)(0))(1) === 4) && (length($index(res)(0)) === 2) && ($index($index(res)(1))(0) === 6) && ($index($index(res)(1))(1) === 8) && (length($index(res)(1)) === 2) && (length(res) === 2))
	$sep()
	console.log("RUNNING 5 TESTS FOR FUNCTION: replace")
	$sep()
	console.log("1:")
	console.log("\tInputs = [] 0 'a', Expected error message: \"replace called on an empty list!\"")
	$verify(replace($_EmptyList())(0)('a'),undefined, "replace called on an empty list!")
	console.log("2:")
	console.log("\tInputs = ['a'] (-1) 'a', Expected error message: \"replace called with a negative index!\"")
	$verify(replace($_Cons('a')($_EmptyList()))(-1)('a'),undefined, "replace called with a negative index!")
	console.log("3:")
	console.log("\tInputs = ['a','b'] 0 'c', Expected return value: ['c','b']")
	$verify(replace($_Cons('a')($_Cons('b')($_EmptyList())))(0)('c'),res => ($index(res)(0) === 'c') && ($index(res)(1) === 'b') && (length(res) === 2))
	console.log("4:")
	console.log("\tInputs = ['a','b','c'] 2 'x', Expected return value: ['a','b','x']")
	$verify(replace($_Cons('a')($_Cons('b')($_Cons('c')($_EmptyList()))))(2)('x'),res => ($index(res)(0) === 'a') && ($index(res)(1) === 'b') && ($index(res)(2) === 'x') && (length(res) === 3))
	console.log("5:")
	console.log("\tInputs = ['a','b','c'] 1 'x', Expected return value: ['a','x','c']")
	$verify(replace($_Cons('a')($_Cons('b')($_Cons('c')($_EmptyList()))))(1)('x'),res => ($index(res)(0) === 'a') && ($index(res)(1) === 'x') && ($index(res)(2) === 'c') && (length(res) === 3))
	$sep()
	console.log("RUNNING 8 TESTS FOR FUNCTION: replace2D")
	$sep()
	console.log("1:")
	console.log("\tInputs = [] 0 0 'x', Expected error message: \"replace2D called with an empty matrix!\"")
	$verify(replace2D($_EmptyList())(0)(0)('x'),undefined, "replace2D called with an empty matrix!")
	console.log("2:")
	console.log("\tInputs = [[]] 0 0 'x', Expected error message: \"illegal x index in replace2D, too large!\"")
	$verify(replace2D($_Cons($_EmptyList())($_EmptyList()))(0)(0)('x'),undefined, "illegal x index in replace2D, too large!")
	console.log("3:")
	console.log("\tInputs = [['a']] 1 0 'x', Expected error message: \"illegal y index in replace2D, too large! \"")
	$verify(replace2D($_Cons($_Cons('a')($_EmptyList()))($_EmptyList()))(1)(0)('x'),undefined, "illegal y index in replace2D, too large!")
	console.log("4:")
	console.log("\tInputs = [['a']] 0 1 'x', Expected error message: \"illegal x index in replace2D, too large!\"")
	$verify(replace2D($_Cons($_Cons('a')($_EmptyList()))($_EmptyList()))(0)(1)('x'),undefined, "illegal x index in replace2D, too large!")
	console.log("5:")
	console.log("\tInputs = [['a']] (-1) 0 'x', Expected error message: \"replace2D called with a negative index!\"")
	$verify(replace2D($_Cons($_Cons('a')($_EmptyList()))($_EmptyList()))(-1)(0)('x'),undefined, "replace2D called with a negative index!")
	console.log("6:")
	console.log("\tInputs = [['a']] 0 (-1) 'x', Expected error message: \"replace2D called with a negative index!\"")
	$verify(replace2D($_Cons($_Cons('a')($_EmptyList()))($_EmptyList()))(0)(-1)('x'),undefined, "replace2D called with a negative index!")
	console.log("7:")
	console.log("\tInputs = [['a']] 0 0 'x', Expected return value: [['x']]")
	$verify(replace2D($_Cons($_Cons('a')($_EmptyList()))($_EmptyList()))(0)(0)('x'),res => ($index($index(res)(0))(0) === 'x') && (length($index(res)(0)) === 1) && (length(res) === 1))
	console.log("8:")
	console.log("\tInputs = [['a','b'],['c','d']] 1 0 'x', Expected return value: [['a','b'],['x','d']]")
	$verify(replace2D($_Cons($_Cons('a')($_Cons('b')($_EmptyList())))($_Cons($_Cons('c')($_Cons('d')($_EmptyList())))($_EmptyList())))(1)(0)('x'),res => ($index($index(res)(0))(0) === 'a') && ($index($index(res)(0))(1) === 'b') && (length($index(res)(0)) === 2) && ($index($index(res)(1))(0) === 'x') && ($index($index(res)(1))(1) === 'd') && (length($index(res)(1)) === 2) && (length(res) === 2))
	$sep()
	console.log("RUNNING 8 TESTS FOR FUNCTION: index2D")
	$sep()
	console.log("1:")
	console.log("\tInputs = [] 0 0, Expected error message: \"index2D called with an empty matrix!\"")
	$verify(index2D($_EmptyList())(0)(0),undefined, "index2D called with an empty matrix!")
	console.log("2:")
	console.log("\tInputs = [[]] 0 0, Expected error message: \"illegal x index in index2D, too large!\"")
	$verify(index2D($_Cons($_EmptyList())($_EmptyList()))(0)(0),undefined, "illegal x index in index2D, too large!")
	console.log("3:")
	console.log("\tInputs = [['a']] 1 0, Expected error message: \"illegal y index in index2D, too large!\"")
	$verify(index2D($_Cons($_Cons('a')($_EmptyList()))($_EmptyList()))(1)(0),undefined, "illegal y index in index2D, too large!")
	console.log("4:")
	console.log("\tInputs = [['a']] 0 1, Expected error message: \"illegal x index in index2D, too large!\"")
	$verify(index2D($_Cons($_Cons('a')($_EmptyList()))($_EmptyList()))(0)(1),undefined, "illegal x index in index2D, too large!")
	console.log("5:")
	console.log("\tInputs = [['a']] (-1) 0, Expected error message: \"index2D called with a negative index!\"")
	$verify(index2D($_Cons($_Cons('a')($_EmptyList()))($_EmptyList()))(-1)(0),undefined, "index2D called with a negative index!")
	console.log("6:")
	console.log("\tInputs = [['a']] 0 (-1), Expected error message: \"index2D called with a negative index!\"")
	$verify(index2D($_Cons($_Cons('a')($_EmptyList()))($_EmptyList()))(0)(-1),undefined, "index2D called with a negative index!")
	console.log("7:")
	console.log("\tInputs = [['a']] 0 0, Expected return value: 'a'")
	$verify(index2D($_Cons($_Cons('a')($_EmptyList()))($_EmptyList()))(0)(0),res => res === 'a')
	console.log("8:")
	console.log("\tInputs = [['a','b'],['c','d']] 1 0, Expected return value: 'c'")
	$verify(index2D($_Cons($_Cons('a')($_Cons('b')($_EmptyList())))($_Cons($_Cons('c')($_Cons('d')($_EmptyList())))($_EmptyList())))(1)(0),res => res === 'c')
	$sep()
	console.log("RUNNING 3 TESTS FOR FUNCTION: reduce")
	$sep()
	console.log("1:")
	console.log("\tInputs = (+) [], Expected error message: \"reduce called on an empty list!\"")
	$verify(reduce($add)($_EmptyList()),undefined, "reduce called on an empty list!")
	console.log("2:")
	console.log("\tInputs = (+) [1], Expected return value: 1")
	$verify(reduce($add)($_Cons(1)($_EmptyList())),res => res === 1)
	console.log("3:")
	console.log("\tInputs = (+) [1,2,3], Expected return value: 6")
	$verify(reduce($add)($_Cons(1)($_Cons(2)($_Cons(3)($_EmptyList())))),res => res === 6)
	$sep()
	console.log("RUNNING 3 TESTS FOR FUNCTION: length")
	$sep()
	console.log("1:")
	console.log("\tInputs = [], Expected return value: 0")
	$verify(length($_EmptyList()),res => res === 0)
	console.log("2:")
	console.log("\tInputs = [1,2], Expected return value: 2")
	$verify(length($_Cons(1)($_Cons(2)($_EmptyList()))),res => res === 2)
	console.log("3:")
	console.log("\tInputs = [[],[]], Expected return value: 2")
	$verify(length($_Cons($_EmptyList())($_Cons($_EmptyList())($_EmptyList()))),res => res === 2)
	$sep()
	console.log("RUNNING 4 TESTS FOR FUNCTION: and")
	$sep()
	console.log("1:")
	console.log("\tInputs = [], Expected return value: True")
	$verify(and($_EmptyList()),res => res)
	console.log("2:")
	console.log("\tInputs = [False], Expected return value: False")
	$verify(and($_Cons(false)($_EmptyList())),res => !res)
	console.log("3:")
	console.log("\tInputs = [True, True], Expected return value: True")
	$verify(and($_Cons(true)($_Cons(true)($_EmptyList()))),res => res)
	console.log("4:")
	console.log("\tInputs = [True, False, True], Expected return value: False")
	$verify(and($_Cons(true)($_Cons(false)($_Cons(true)($_EmptyList())))),res => !res)
	$sep()
	console.log("RUNNING 4 TESTS FOR FUNCTION: or")
	$sep()
	console.log("1:")
	console.log("\tInputs = [], Expected return value: False")
	$verify(or($_EmptyList()),res => !res)
	console.log("2:")
	console.log("\tInputs = [False], Expected return value: False")
	$verify(or($_Cons(false)($_EmptyList())),res => !res)
	console.log("3:")
	console.log("\tInputs = [True, False], Expected return value: True")
	$verify(or($_Cons(true)($_Cons(false)($_EmptyList()))),res => res)
	console.log("4:")
	console.log("\tInputs = [True, False, False], Expected return value: True")
	$verify(or($_Cons(true)($_Cons(false)($_Cons(false)($_EmptyList())))),res => res)
	$sep()
	console.log("RUNNING 3 TESTS FOR FUNCTION: allSame")
	$sep()
	console.log("1:")
	console.log("\tInputs = [], Expected return value: True")
	$verify(allSame($_EmptyList()),res => res)
	console.log("2:")
	console.log("\tInputs = [1,1], Expected return value: True")
	$verify(allSame($_Cons(1)($_Cons(1)($_EmptyList()))),res => res)
	console.log("3:")
	console.log("\tInputs = [1,1,3,1], Expected return value: False")
	$verify(allSame($_Cons(1)($_Cons(1)($_Cons(3)($_Cons(1)($_EmptyList()))))),res => !res)
	$sep()
	console.log("RUNNING 4 TESTS FOR FUNCTION: concat")
	$sep()
	console.log("1:")
	console.log("\tInputs = [], Expected return value: []")
	$verify(concat($_EmptyList()),res => res instanceof $EmptyList)
	console.log("2:")
	console.log("\tInputs = [[]], Expected return value: []")
	$verify(concat($_Cons($_EmptyList())($_EmptyList())),res => res instanceof $EmptyList)
	console.log("3:")
	console.log("\tInputs = [[],[]], Expected return value: []")
	$verify(concat($_Cons($_EmptyList())($_Cons($_EmptyList())($_EmptyList()))),res => res instanceof $EmptyList)
	console.log("4:")
	console.log("\tInputs = [[1,2],[3,4]], Expected return value: [1,2,3,4]")
	$verify(concat($_Cons($_Cons(1)($_Cons(2)($_EmptyList())))($_Cons($_Cons(3)($_Cons(4)($_EmptyList())))($_EmptyList()))),res => ($index(res)(0) === 1) && ($index(res)(1) === 2) && ($index(res)(2) === 3) && ($index(res)(3) === 4) && (length(res) === 4))
	$sep()
	console.log("RUNNING 3 TESTS FOR FUNCTION: replicate")
	$sep()
	console.log("1:")
	console.log("\tInputs = (-1) 'a', Expected error message: \"replicate called with a negative amount!\"")
	$verify(replicate(-1)('a'),undefined, "replicate called with a negative amount!")
	console.log("2:")
	console.log("\tInputs = 0 'a', Expected return value: []")
	$verify(replicate(0)('a'),res => res instanceof $EmptyList)
	console.log("3:")
	console.log("\tInputs = 2 'a', Expected return value: ['a','a']")
	$verify(replicate(2)('a'),res => ($index(res)(0) === 'a') && ($index(res)(1) === 'a') && (length(res) === 2))
	$sep()
	console.log("RUNNING 3 TESTS FOR FUNCTION: replicateGrid")
	$sep()
	console.log("1:")
	console.log("\tInputs = (-1) 'a', Expected error message: \"replicate called with a negative amount!\"")
	$verify(replicateGrid(-1)('a'),undefined, "replicate called with a negative amount!")
	console.log("2:")
	console.log("\tInputs = 0 'a', Expected return value: []")
	$verify(replicateGrid(0)('a'),res => res instanceof $EmptyList)
	console.log("3:")
	console.log("\tInputs = 2 'a', Expected return value: [['a','a'],['a','a']]")
	$verify(replicateGrid(2)('a'),res => ($index($index(res)(0))(0) === 'a') && ($index($index(res)(0))(1) === 'a') && (length($index(res)(0)) === 2) && ($index($index(res)(1))(0) === 'a') && ($index($index(res)(1))(1) === 'a') && (length($index(res)(1)) === 2) && (length(res) === 2))
	$sep()
	console.log("TESTING COMPLETE")
	$sep()
	console.log(($successes + $failures) + " total tests ran")
	console.log($successes + " successes")
	console.log($failures + " failures\n")
}

$runSUnit()
