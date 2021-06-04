const {Record} = require("immutable")
const prand = require("pure-rand")

class $EmptyList {}; const $_EmptyList = () => {return new $EmptyList()}
//The element in the list node, and a pointer to the next node in the list
class $Cons{constructor(arg0, arg1){this.val0 = arg0; this.val1 = arg1}}
const $_Cons = arg0 => arg1 => {return new $Cons(arg0, arg1)}

const $index = arg0 => arg1 => {
	if(arg1 == 0) {
		return arg0.val0
	} else {
		return $index(arg0.val1)(arg1 - 1)
	}
}

const filter = $arg0 => $arg1 => {
	if($arg1 instanceof $EmptyList){
		return $_EmptyList()
	}
	if(true){
		const x = $index($arg1)(0)
		const xs = drop(1)($arg1)
		const pred = $arg0
		if(pred(x)){
			return $_Cons(x)(filter(pred)(xs))
		}
		if(true){
			return filter(pred)(xs)
		}
	}
}

const filter2D = $arg0 => $arg1 => {
	const pred = $arg0
	const matrix = $arg1
	return map(filter(pred))(matrix)
}

const map = arg0 => arg1 => {

	//console.log("GOING TO MAP THE FN \n")
	//console.log(arg0)
	//console.log("OVER THE LIST")
	//console.log(arg1)

	if(arg1 instanceof $EmptyList){
		return $_EmptyList()
	}
	//This should be something that we are able to check at compile time,
	//so ignore the commented bit below where we need to check the length
	else{
		//Apply the function to this node and "cons" the result to the result of the mapped list
		return $_Cons(arg0(arg1.val0))(map(arg0)(arg1.val1))
	}
	/*
	//We have a "1" cons pattern i.e. x:xs therefore needs to be at least 1 element
	//this can be generalised e.g. >= 2 for x1:x2:xs etc
	if(length(arg1) >= 1){
		return _Cons(arg0(arg1.val0))(map(arg0)(arg1.val1))
	}*/
}


const length = arg0 => {
	if(arg0 instanceof $EmptyList){
		return 0
	}
	else {
		return 1 + length(arg0.val1)
	}
}



const drop = arg0 => arg1 => {
	if(arg0 === 0){
		return arg1
	} else {
		return drop(arg0 - 1)(arg1.val1)
	}
}

const take = arg0 => arg1 => {
	if(arg0 === 0){
		return $_EmptyList()
	} else {
		return $_Cons($index(arg1)(0))(take(arg0 - 1)(drop(1)(arg1)))
	}
}


const tail = arg0 => {
	if(length(arg0) >= 1){
		return arg0.val1
	} else {
		error("Tried to get tail of an empty list!")
	}
}

const replace = arg0 => arg1 => arg2 => {
	return $append($append(take(arg1)(arg0))(singleton(arg2)))(drop(arg1 + 1)(arg0))
}


const replace2D = arg0 => arg1 => arg2 => arg3 => {

	return $append( 
		$append( take(arg1)(arg0) )( singleton(replace ($index (arg0)(arg1))(arg2)(arg3)) ) 
	)(drop (arg1 + 1)(arg0) )

}

const index2D = arg0 => arg1 => arg2 => {
	return $index($index(arg0)(arg1))(arg2)
}



const and = arg0 => {
	if(arg0 instanceof $EmptyList){
		return true
	} else {
		const x = $index(arg0)(0)
		const xs = drop(1)(arg0)
		return x && (and(xs))
	}
}

const singleton = arg0 => {
	return $_Cons(arg0)($_EmptyList())
}


const concat = $arg0 => {
	return reduce($append)($arg0)
}


const replicate2D = $arg0 => $arg1 => {
	const $name = "replicate2D"
	const n = $arg0
	const x = $arg1
	return replicate(n)(replicate(n)(x))
	$error("Uncovered pattern case in function ${$name}!")
}


const reduce = arg0 => arg1 => {
	if(length(arg1) === 1){
		return arg1.val0
	}
	else{
		return reduce(arg0)($_Cons(arg0($index(arg1)(0))($index(arg1)(1)))(drop(2)(arg1)))
	}
}



const $append = arg0 => arg1 => {
	if(arg0 instanceof $EmptyList){
		return arg1
	} else {
		return $_Cons($index(arg0)(0))($append(drop(1)(arg0))(arg1))
	}
}

const replicate = $arg0 => $arg1 => {
	const $name = "replicate"
	if($arg0 === 0){
		return $_EmptyList()
	}
	if(true){
		const n = $arg0
		const x = $arg1
		return $_Cons(x)(replicate($sub(n)(1))(x))
	}
	$error("Uncovered pattern case in function ${$name}!")
}



const $add = x => y => x + y
const $sub = x => y => x - y
const abs = arg => Math.abs(arg)
const $leq = x => y => x <= y
const $and = x => y => x && y
const $or = x => y => x || y
const $eq = x => y => x === y

class Just {constructor(arg0){this.val0 = arg0}}
const _Just = arg0 => new Just(arg0)

const mkRandomGen = seed => prand.mersenne(seed)
const randomGenBetween = lower => higher => gen => prand.uniformIntDistribution(lower,higher,gen)

class Nothing {}
const _Nothing = () => new Nothing()

var $errLog = []
var $shouldPrint = false
const $errMsg = msg => {$errLog.push(msg);if($shouldPrint){console.log("\"" + msg + "\"")}}


class LeftClick{constructor(arg0, arg1){this.val0 = arg0; this.val1 = arg1}}
const _LeftClick = arg0 => arg1 => new LeftClick(arg0, arg1)
class RightClick{constructor(arg0, arg1){this.val0 = arg0; this.val1 = arg1}}
const _RightClick = arg0 => arg1 => new RightClick(arg0, arg1)
class Reset{}
const _Reset = () => new Reset()
class SeedRecieved{constructor(arg0){this.val0 = arg0}}
const _SeedRecieved = arg0 => new SeedRecieved(arg0)
class Dummy{}
const _Dummy = () => new Dummy()
class Playing{}
const _Playing = () => new Playing()
class Won{}
const _Won = () => new Won()
class Lost{}
const _Lost = () => new Lost()
class Loading{}
const _Loading = () => new Loading()
class Hidden{}
const _Hidden = () => new Hidden()
class Revealed{constructor(arg0){this.val0 = arg0}}
const _Revealed = arg0 => new Revealed(arg0)
class Flagged{}
const _Flagged = () => new Flagged()
class Exploded{}
const _Exploded = () => new Exploded()

class State extends Record({grid : undefined, bombCoords : undefined, gameState : undefined, gen : undefined}){}
const _State = arg0 => arg1 => arg2 => arg3 =>
	new State({grid : arg0, bombCoords : arg1, gameState : arg2, gen : arg3})

const update = $arg0 => $arg1 => {
	const $name = "update"
	const s = $arg0
	const e = $arg1
	const checkWon = (arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const result = arg0

		return $eq(result)(90)
	})
	(length(concat(filter2D((arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const cellState = arg0

		return cellState instanceof Revealed
	})
	)(s.grid))))
	const genBombCoords = (arg0 => arg1 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const gen = arg0
		const count = arg1

		return genBombCoordsRec(gen)(count)($_EmptyList())
	})
	
	return (() => {
		if(e instanceof LeftClick){
			const y = e.val0
			const x = e.val1
			return (() => {
			if(index2D(s.grid)(y)(x) instanceof Hidden){
				return (() => {
				if(bombOrCount(s.bombCoords)([y,x]) instanceof Nothing){
					return s.mergeDeep({grid : replace2D(s.grid)(y)(x)(_Exploded()), gameState : _Lost()})
				}
				if(bombOrCount(s.bombCoords)([y,x]) instanceof Just){
					const count = bombOrCount(s.bombCoords)([y,x]).val0
					return (() => {
				if(!(true)){$error("Uncovered pattern in a let expression in function:", $name, "!!"); return}
					const newGrid = replace2D(s.grid)(y)(x)(_Revealed(count))

					return (() => {
							if(checkWon){
								return s.mergeDeep({grid : newGrid, gameState : _Won()})
							}
							if(true){
								return s.set("grid", newGrid)
							}
							$error("Uncovered case in a conditional expression!")
					})()

				})()
				
				}
				$error("Uncovered match expression case in function:" + $name + "!")
			})()

			}
			if(index2D(s.grid)(y)(x) instanceof Revealed){
				return s
			}
			if(index2D(s.grid)(y)(x) instanceof Flagged){
				return s
			}
			$error("Uncovered match expression case in function:" + $name + "!")
		})()

		}
		if(e instanceof RightClick){
			const y = e.val0
			const x = e.val1
			return (() => {
			if(index2D(s.grid)(y)(x) instanceof Hidden){
				return s.set("grid", replace2D(s.grid)(y)(x)(_Flagged()))
			}
			if(index2D(s.grid)(y)(x) instanceof Flagged){
				return s.set("grid", replace2D(s.grid)(y)(x)(_Hidden()))
			}
			if(index2D(s.grid)(y)(x) instanceof Revealed){
				return s
			}
			$error("Uncovered match expression case in function:" + $name + "!")
		})()

		}
		if(e instanceof Dummy){
			return s
		}
		if(e instanceof Reset){
			return (() => {
		if(!(true)){$error("Uncovered pattern in a let expression in function:", $name, "!!"); return}
			const newBombCoords = genBombCoords(s.gen)(10)[0]
			const newGen = genBombCoords(s.gen)(10)[1]

			return (() => {
			if(!(true)){$error("Uncovered pattern in a let expression in function:", $name, "!!"); return}
				const newGrid = replicate2D(10)(_Hidden())

				return s.mergeDeep({gen : newGen, bombCoords : newBombCoords, gameState : _Playing(), grid : newGrid})
			})()
			
		})()
		
		}
		if(e instanceof SeedRecieved){
			const seed = e.val0
			return (() => {
		if(!(true)){$error("Uncovered pattern in a let expression in function:", $name, "!!"); return}
			const initGen = mkRandomGen(seed)

			return (() => {
			if(!(true)){$error("Uncovered pattern in a let expression in function:", $name, "!!"); return}
				const rndmCoords = genBombCoords(initGen)(10)[0]
				const gen1 = genBombCoords(initGen)(10)[1]

				return s.mergeDeep({gen : gen1, bombCoords : rndmCoords, gameState : _Playing()})
			})()
			
		})()
		
		}
		$error("Uncovered match expression case in function:" + $name + "!")
	})()

	$error("Uncovered pattern case in function: update!")
}

const init = _State(replicate2D(10)(_Hidden()))($_EmptyList())(_Loading())(mkRandomGen(0))

const bombOrCount = $arg0 => $arg1 => {
	const $name = "bombOrCount"
	if($arg0 instanceof $EmptyList){
		return _Just(0)
	}
	if(length($arg0) >= 1){
		const y1 = $index($arg0)(0)[0]
		const x1 = $index($arg0)(0)[1]
		const coords = drop(1)($arg0)
		const y2 = $arg1[0]
		const x2 = $arg1[1]
	const diffY = abs($sub(y1)(y2))
	const diffX = abs($sub(x1)(x2))
	const plusMaybe = (arg0 => arg1 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const x = arg0
		const maybe = arg1

		return (() => {
				if(maybe instanceof Nothing){
					return _Nothing()
				}
				if(maybe instanceof Just){
					const y = maybe.val0
					return _Just($add(x)(y))
				}
				$error("Uncovered match expression case in function:" + $name + "!")
			})()

	})
	
		return (() => {
				if($and($eq(diffY)(0))($eq(diffX)(0))){
					return _Nothing()
				}
				if($and($ltEq(diffY)(1))($ltEq(diffX)(1))){
					return plusMaybe(1)(bombOrCount(coords)([y2,x2]))
				}
				if(true){
					return bombOrCount(coords)([y2,x2])
				}
				$error("Uncovered case in a conditional expression!")
		})()

	}
	$error("Uncovered pattern case in function: bombOrCount!")
}

const containsCoord = $arg0 => $arg1 => {
	const $name = "containsCoord"
	if($arg0 instanceof $EmptyList){
		return false
	}
	if(length($arg0) >= 1){
		const y1 = $index($arg0)(0)[0]
		const x1 = $index($arg0)(0)[1]
		const coords = drop(1)($arg0)
		const y2 = $arg1[0]
		const x2 = $arg1[1]
		return (() => {
				if($and($eq(y1)(y2))($eq(x1)(x2))){
					return true
				}
				if(true){
					return containsCoord(coords)([y2,x2])
				}
				$error("Uncovered case in a conditional expression!")
		})()

	}
	$error("Uncovered pattern case in function: containsCoord!")
}

const genBombCoordsRec = $arg0 => $arg1 => $arg2 => {
	const $name = "genBombCoordsRec"
	const gen = $arg0
	const count = $arg1
	const acc = $arg2
	return (() => {
			if($eq(count)(0)){
				return [acc,gen]
			}
			if(true){
				return (() => {
			if(!(true)){$error("Uncovered pattern in a let expression in function:", $name, "!!"); return}
				const y = randomGenBetween(0)(9)(gen)[0]
				const gen1 = randomGenBetween(0)(9)(gen)[1]

				return (() => {
				if(!(true)){$error("Uncovered pattern in a let expression in function:", $name, "!!"); return}
					const x = randomGenBetween(0)(9)(gen1)[0]
					const gen2 = randomGenBetween(0)(9)(gen1)[1]

					return (() => {
					if(!(true)){$error("Uncovered pattern in a let expression in function:", $name, "!!"); return}
						const ranCoord = [y,x]

						return (() => {
								if(containsCoord(acc)(ranCoord)){
									return genBombCoordsRec(gen2)(count)(acc)
								}
								if(true){
									return genBombCoordsRec(gen2)($sub(count)(1))($_Cons(ranCoord)(acc))
								}
								$error("Uncovered case in a conditional expression!")
						})()

					})()
					
				})()
				
			})()
			
			}
			$error("Uncovered case in a conditional expression!")
	})()

	$error("Uncovered pattern case in function: genBombCoordsRec!")
}


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
	$sep()
	console.log("RUNNING 3 TESTS FOR FUNCTION: genBombCoordsRec")
	$sep()
	const coordsA = genBombCoordsRec(mkRandomGen(0))(10)($_EmptyList())
	const sameTups = (arg0 => {
	if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
	const zippedTups = arg0

	return and(map((arg0 => {
			if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
			const y1 = arg0[0][0]
			const x1 = arg0[0][1]
			const y2 = arg0[1][0]
			const x2 = arg0[1][1]

			return $and($eq(y1)(y2))($eq(x1)(x2))
		})
		)(zippedTups))
})

	console.log("3:")
	console.log("\tInputs = (mkRandomGen 0) 10 [], Expecting satisfaction of: : \(bombCoords,_) -> and []<<")
	$checkSatisfies(genBombCoordsRec(mkRandomGen(0))(10)($_EmptyList()),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const bombCoords = arg0[0]

		return and($_Cons($eq(length(coordsA))(10))($_Cons($eq(length(coordsA))(length(bombCoords)))($_Cons(sameTups(zip(bombCoords)(coordsA)))($_EmptyList()))))
	})
	)
	$sep()
	console.log("RUNNING 3 TESTS FOR FUNCTION: containsCoord")
	$sep()
	console.log("1:")
	console.log("\tInputs = [(1,2),(3,4)] (3,4), Expected return value: : True")
	$verify(containsCoord($_Cons([1,2])($_Cons([3,4])($_EmptyList())))([3,4]),res => res)
	console.log("2:")
	console.log("\tInputs = [(1,2),(3,4)] (5,4), Expected return value: : False")
	$verify(containsCoord($_Cons([1,2])($_Cons([3,4])($_EmptyList())))([5,4]),res => !res)
	console.log("3:")
	console.log("\tInputs = [] (1,2), Expected return value: : False")
	$verify(containsCoord($_EmptyList())([1,2]),res => !res)
	$sep()
	console.log("RUNNING 2 TESTS FOR FUNCTION: bombOrCount")
	$sep()
	console.log("1:")
	console.log("\tInputs = [(0,0),(5,3),(6,7)] (6,7), Expected return value: : Nothing")
	$verify(bombOrCount($_Cons([0,0])($_Cons([5,3])($_Cons([6,7])($_EmptyList()))))([6,7]),res => res instanceof Nothing)
	console.log("2:")
	console.log("\tInputs = [(0,0),(0,1),(5,3),(6,7)] (1,1), Expected return value: : Just 2")
	$verify(bombOrCount($_Cons([0,0])($_Cons([0,1])($_Cons([5,3])($_Cons([6,7])($_EmptyList())))))([1,1]),res => (res instanceof Just) && (res.val0 === 2))
	const templateState = _State(replicate2D(10)(_Hidden()))($_Cons([0,0])($_EmptyList()))(_Playing())(mkRandomGen(0))
	$sep()
	console.log("RUNNING 4 TESTS FOR FUNCTION: update")
	$sep()
	console.log("1:")
	console.log("\tInputs = templateState (LeftClick 0 0), Expecting satisfaction of: : \state -> state.gameState is Lost && ((index2D s.grid 0 0) is Exploded)")
	$checkSatisfies(update(templateState)(_LeftClick(0)(0)),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const state = arg0

		return $and(state.gameState instanceof Lost)(index2D(s.grid)(0)(0) instanceof Exploded)
	})
	)
	console.log("2:")
	console.log("\tInputs = templateState (LeftClick 1 1), Expecting satisfaction of: : \state -> (index2D state.grid 1 1) is (Revealed 1)")
	$checkSatisfies(update(templateState)(_LeftClick(1)(1)),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const state = arg0

		return (index2D(state.grid)(1)(1) instanceof Revealed) && (index2D(state.grid)(1)(1).val0 === 1)
	})
	)
	console.log("3:")
	console.log("\tInputs = {templateState | grid = replace (replicate2D 10 $ Revealed 0) 0 (replicate 10 Hidden)} (LeftClick 1 1), Expecting satisfaction of: : \state -> state.gameState is Won")
	$checkSatisfies(update(templateState.set("grid", replace(replicate2D(10)(_Revealed(0)))(0)(replicate(10)(_Hidden()))))(_LeftClick(1)(1)),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const state = arg0

		return state.gameState instanceof Won
	})
	)
	console.log("4:")
	console.log("\tInputs = templateState (RightClick 1 1), Expecting satisfaction of: : \state -> (index2D state.grid 1 1) is Flagged")
	$checkSatisfies(update(templateState)(_RightClick(1)(1)),(arg0 => {
		if(!(true)){$error("Uncovered pattern in a lambda expression in function:", $name, "!!"); return}
		const state = arg0

		return index2D(state.grid)(1)(1) instanceof Flagged
	})
	)
	$sep()
	console.log("TESTING COMPLETE")
	$sep()
	console.log(($successes + $failures) + " total tests ran")
	console.log($successes + " successes")
	console.log($failures + " failures\n")
}

$runSUnit()