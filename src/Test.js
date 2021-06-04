console.log((x => y => {return x + y})(1)(2))

class EmptyList {}
class Cons {
	constructor(arg0, arg1){
		this.val0 = arg0
		this.val1 = arg1
	}
}

export const $listGen = (from, to, snd = undefined) => {

	let interval = 1

	if(snd != undefined){
		interval = Math.abs(from - snd)
	}

	let head = new Cons(from, new EmptyList())
	let current = head

	//Incrementing list generator
	if(from < to){

		for(let i = from + interval; i <= to; i += interval){

			let newNode = new Cons(i, new EmptyList())
			current.val1 = newNode
			current = newNode

		}

	//Decrementing list generator
	} else {

		for(let i = from - interval; i >= to; i -= interval){

			let newNode = new Cons(i, new EmptyList())
			current.val1 = newNode
			current = newNode

		}

	}

	return head

}

const syphon2js = listNode => {

	//console.log("siphon2js called on the value:")
	//console.log(listNode)

	//Edge case, treat empty siphon lists as empty javascript lists
	if(listNode instanceof EmptyList){
		return []
	}

	//console.log("first bit passed")

	let jsList = [listNode.val0]

	//console.log("second bit passed, first bit of list is:")
	//console.log(jsList)

	while(!(listNode.val1 instanceof EmptyList)){

		listNode = listNode.val1
		jsList.push(listNode.val0)

	}

	return jsList

}

console.log(syphon2js($listGen(1,10)))