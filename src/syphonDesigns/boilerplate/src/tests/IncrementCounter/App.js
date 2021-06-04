var $errLog = []
var $shouldPrint = false
const $errMsg = msg => {$errLog.push(msg);if($shouldPrint){console.log("\"" + msg + "\"")}}

const $add = arg0 => arg1 => arg0 + arg1
const $sub = arg0 => arg1 => arg0 - arg1

class Inc{}
const _Inc = () => new Inc()
class Dec{}
const _Dec = () => new Dec()

const update = ($arg0, $arg1) => {
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

	$error("Uncovered pattern case in function: update!")
}



function $runSUnit(){
	let $successes = 0
	let $failures = 0
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
	console.log("RUNNING 4 TESTS FOR FUNCTION: update")
	$sep()
	console.log("1:")
	console.log("\tInputs = 0 Dec, Expected return value: (-1)")
	$verify(update(0,_Dec()),res => res === -1)
	console.log("2:")
	console.log("\tInputs = 0 Inc, Expected return value: 1")
	$verify(update(0,_Inc()),res => res === 1)
	console.log("3:")
	console.log("\tInputs = (-1) Inc, Expected return value: 0")
	$verify(update(-1,_Inc()),res => res === 0)
	console.log("4:")
	console.log("\tInputs = 1 Inc, Expected return value: 2")
	$verify(update(1,_Inc()),res => res === 2)
	$sep()
	console.log("TESTING COMPLETE")
	$sep()
	console.log(($successes + $failures) + " total tests ran")
	console.log($successes + " successes")
	console.log($failures + " failures\n")
}

$runSUnit()
