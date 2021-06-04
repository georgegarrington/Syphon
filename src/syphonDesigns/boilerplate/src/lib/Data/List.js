import { $errMsg, $sub } from "../Hardwired/Core.js";

/*
type List a = EmptyList | Cons a (List a)
*/
export class $EmptyList {}; export const $_EmptyList = () => {return new $EmptyList()}
//The element in the list node, and a pointer to the next node in the list
export class $Cons{constructor(arg0, arg1){this.val0 = arg0; this.val1 = arg1}}
export const $_Cons = arg0 => arg1 => {return new $Cons(arg0, arg1)}

/*
(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x:xs ++ ys
*/

export const $append = arg0 => arg1 => {
	if(arg0 instanceof $EmptyList){
		return arg1
	} else {
		return $_Cons($index(arg0)(0))($append(drop(1)(arg0))(arg1))
	}
}

/*
$index :: [a] -> Int -> a
$index (x:xs) i
	| i == 0 = x
	| otherwise = $index xs (i - 1)
*/
export const $index = arg0 => arg1 => {
	if(arg1 == 0) {
		return arg0.val0
	} else {
		return $index(arg0.val1)(arg1 - 1)
	}
}

/*
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = (f x):xs
*/
export const map = arg0 => arg1 => {

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

/*
reduce :: (a -> a -> a) -> [a] -> a
reduce _ [x] = x
reduce f (x1:x2:xs) = reduce f (f x1 x2):xs
*/
export const reduce = arg0 => arg1 => {
	if(length(arg1) === 1){
		return arg1.val0
	}
	else{
		return reduce(arg0)($_Cons(arg0($index(arg1)(0))($index(arg1)(1)))(drop(2)(arg1)))
	}
}

/*
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter pred (x:xs)
	| pred x = x:filter pred xs
	| otherwise = filter pred xs
*/

export const filter = $arg0 => $arg1 => {
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

/*
map2D :: (a -> b) -> [[a]] -> [[b]]
map2D f matrix = map (map f) matrix
*/

export const map2D = $arg0 => $arg1 => {
	const f = $arg0
	const matrix = $arg1
	return map(map(f))(matrix)
}

/*
filter2D :: (a -> Bool) -> [[a]] -> [[a]]
filter2D pred matrix = map (filter pred) matrix
*/

export const filter2D = $arg0 => $arg1 => {
	const pred = $arg0
	const matrix = $arg1
	return map(filter(pred))(matrix)
}


/*
length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + (length xs)
*/
export const length = arg0 => {
	if(arg0 instanceof $EmptyList){
		return 0
	}
	else {
		return 1 + length(arg0.val1)
	}
}

/*
drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop n (x:xs) = drop (n - 1) xs
*/
export const drop = arg0 => arg1 => {
	if(arg0 === 0){
		return arg1
	} else {
		return drop(arg0 - 1)(arg1.val1)
	}
}

/*
take :: Int -> [a] -> [a]
take 0 xs = []
take n (x:xs) = x:take (n - 1) xs
*/
export const take = arg0 => arg1 => {
	if(arg0 === 0){
		return $_EmptyList()
	} else {
		return $_Cons($index(arg1)(0))(take(arg0 - 1)(drop(1)(arg1)))
	}
}

/*
tail :: [a] -> [a]
tail (x:xs) = xs
tail [] = error "Tried to get tail of an empty list!"
*/
export const tail = arg0 => {
	if(length(arg0) >= 1){
		return arg0.val1
	} else {
		error("Tried to get tail of an empty list!")
	}
}

/*
head :: [a] -> a
head (x:xs) = x
head [] = error "Tried to get head of an empty list!"
*/
export const head = arg0 => {
	return arg0.val0
}

/*
isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False
*/
export const isEmpty = arg0 => {
	return arg0 instanceof $EmptyList
}

/*
replace :: [a] -> Int -> a -> [a]
replace xs i x = (take i xs) ++ [x] ++ (drop (i + 1) xs)
*/
export const replace = arg0 => arg1 => arg2 => {
	return $append($append(take(arg1)(arg0))(singleton(arg2)))(drop(arg1 + 1)(arg0))
}

/*
replace2D :: [[a]] -> Int -> Int -> a -> [[a]]
replace2D list2D x y elem = (take x list2D) ++ [replace (list2D !! x) y elem] ++ (drop (x + 1) list2D)
*/ // 2Dlist, y, x, thing
export const replace2D = arg0 => arg1 => arg2 => arg3 => {

	return $append( 
		$append( take(arg1)(arg0) )( singleton(replace ($index (arg0)(arg1))(arg2)(arg3)) ) 
	)(drop (arg1 + 1)(arg0) )

}

/*
index2D :: [[a]] -> Int -> Int -> a
index2D list2D x y = list2D !! y !! x
*/
export const index2D = arg0 => arg1 => arg2 => {
	return $index($index(arg0)(arg1))(arg2)
}

/*
and :: [a] -> Bool
and [] = True
and (x:xs) = x && (and xs)
*/
export const and = arg0 => {
	if(arg0 instanceof $EmptyList){
		return true
	} else {
		const x = $index(arg0)(0)
		const xs = drop(1)(arg0)
		return x && (and(xs))
	}
}

/*
allSame :: Eq a => [a] -> Bool
allSame xs = and $ map (\x -> x == (head xs)) xs
*/
export const allSame = arg0 => {
	const xs = arg0
	return and(map(x => x === head(xs))(xs))
}

//UTILITY/IMPURE FUNCTIONS, mostly used for pretty printing etc.

//Wraps the element in a list to produce a singleton list
export const singleton = arg0 => {
	return $_Cons(arg0)($_EmptyList())
}

/*
We need to use some contained mutation to build up a javascript 
mutable list, we are forced to use javascript lists sometimes e.g.
in the case of react if we want to give a list of child components
it must be a mutable javascript list, so we can abstract this behaviour away
and make helper functions for constructing GUI components that support
taking a siphon list of child components, and then can use this
function behind the scenes to convert our pure list to a mutable javascript list
*/
export const $syphon2js = listNode => {

	//console.log("siphon2js called on the value:")
	//console.log(listNode)	\\t\t\t\t\


	//Edge case, treat empty siphon lists as empty javascript lists
	if(listNode instanceof $EmptyList){
		return []
	}

	//console.log("first bit passed")

	let jsList = [listNode.val0]

	//console.log("second bit passed, first bit of list is:")
	//console.log(jsList)

	while(!(listNode.val1 instanceof $EmptyList)){

		listNode = listNode.val1
		jsList.push(listNode.val0)

	}

	return jsList

}

//Again, use some contained mutation to build up a mutable js list into a pure siphon list
export const $js2syphon = jsList => {

	//Start with the end node of the list and build it up backwards
	let siphonList = $_EmptyList()

	//Iterate through the jsList backwards and build up the siphon list in this manor
	for(let i = jsList.length - 1; i >= 0; i--){

		siphonList = $_Cons(jsList[i])(siphonList)

	}

	return siphonList

}

//Does the same thing but for 2D lists
export const $syphon2js2D = listNode => {

	//console.log("siphon2js called on the value:")
	//console.log(listNode)

	//Edge case, treat empty siphon lists as empty javascript lists
	if(listNode instanceof $EmptyList){
		return []
	}

	//console.log("first bit passed")

	let jsList = [syphon2js(listNode.val0)]

	//console.log("second bit passed, first bit of list is:")
	//console.log(jsList)

	while(!(listNode.val1 instanceof $EmptyList)){

		listNode = listNode.val1
		jsList.push(syphon2js(listNode.val0))//listNode.val0

	}

	return jsList

}

//Does the same thing but for 2D lists
export const $js2syphon2D = jsList => {

	//Start with the end node of the list and build it up backwards
	let siphonList = $_EmptyList()

	//Iterate through the jsList backwards and build up the siphon list in this manor
	for(let i = jsList.length - 1; i >= 0; i--){

		siphonList = $_Cons(js2syphon(jsList[i]))(siphonList)

	}

	return siphonList

}


export const concat = $arg0 => {
	return reduce($append)($arg0)
}

export const replicate = $arg0 => $arg1 => {
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

export const replicate2D = $arg0 => $arg1 => {
	const $name = "replicate2D"
	const n = $arg0
	const x = $arg1
	return replicate(n)(replicate(n)(x))
	$error("Uncovered pattern case in function ${$name}!")
}

/*
const reduced = reduce($append)(js2syphon([
	js2syphon([1,2]), js2syphon([3,4]), js2syphon([5,6])
]))

const appended = $append(js2syphon([1,2]))(js2syphon([3,4]))
const appendedAgain = $append(appended)(js2syphon([5,6]))
console.log($index(reduced)(4))*/

/*
Some utility function that has not been defined yet,
most likely it will simply just display a popup winow showing
the error message
*/
export const error = msg => {

}