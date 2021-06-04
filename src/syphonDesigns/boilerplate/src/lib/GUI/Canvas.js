import React from "react"
import {EmptyList} from "../Data/List"
import {Context} from "../../App"

/*
//Everything that makes canvas3 great but even better! Try and do it so that now it draws lines between points
export const _Canvas3 = width => height => strokeNode => onMouseMove => onMouseDown => onMouseUp => {

	let ref = useRef(null)

	useEffect(() => {

		let context = ref.current.getContext("2d")

		while(!(strokeNode instanceof EmptyList)){
		
			const color = strokeNode.val0.val0
			//context.fillStyle = strokeNode.val0.val0
			context.strokeStyle = strokeNode.val0.val0

			//Now we need two tuples to draw a line so if the next node is emptylist cannot draw a line
			for(let strokeTupNode = strokeNode.val0.val1; 
				!(strokeTupNode.val1 instanceof EmptyList); 
				strokeTupNode = strokeTupNode.val1){

				//context.fillRect(strokeTupNode.val0[1], strokeTupNode.val0[0], 2, 2)
				context.beginPath()
				context.moveTo(strokeTupNode.val0[1], strokeTupNode.val0[0])
				context.lineTo(strokeTupNode.val1.val0[1], strokeTupNode.val1.val0[0])
				context.stroke()

			}

			//Look at the next stroke
			strokeNode = strokeNode.val1

		}

	})

	
	We do not care about the information contained in the mouse event 
	when the mouse is first pressed down or released, so we can discard these

	Even though this is not REALLY wildcard syntax as it is declaring
	a variable called "_" that represents the mouse event argument,
	it looks and smells like wildcard syntax and can be used the same
	which is really nice! (Cos I'm never ever going to have a name clash issue
	where I have 2 variables called "_" lol)
	
	const handleMouseUp = _ => {
		onMouseUp()
	}

	const handleMouseDown = _ => {
		onMouseDown()
	}

	const extractCoords = mouseEvent => {
		onMouseMove(mouseEvent.pageY - ref.current.offsetTop)(mouseEvent.pageX - ref.current.offsetLeft)
	}

	return(
		<canvas
			ref = {ref}
			width = {width}
			height = {height}
			onMouseMove = {extractCoords}
			onMouseUp = {handleMouseUp}
			onMouseDown = {handleMouseDown}
		/>
	)

}*/

export const _Canvas = width => height => onMouseDown => onMouseUp => onMouseMove => strokeNode => {

	let ref = React.useRef(null)

	React.useEffect(() => {

		let context = ref.current.getContext("2d")
		context.clearRect(0, 0, width, height)

		//Each val0 of the strokeNode is a stroke data type

		console.log("the value of strokeNode is: ")
		console.log(strokeNode)

		while(!(strokeNode instanceof EmptyList)){
		
			//console.log("The value of strokenode:")
			//console.log(strokeNode)

			//console.log("the value of strokenode:")
			//console.log(strokeNode)

			//console.log("The value of strokenode.val0:")
			//console.log(strokeNode.val0)
			
			//First get the colour of the stroke
			const color = strokeNode.val0.val0
			//console.log("the value of color is:")
			//console.log(color)

			//Set the colour that the stroke is going to be drawn in to the colour of the stroke
			context.fillStyle = strokeNode.val0.val0


			//console.log("The value of strokeNode.val0.val1: ")
			//console.log(strokeNode.val0.val1)

			//To get the list of tuple coordinates in the stroke this is val1 of the stroke node
			
			//console.log("while loop iteration")

			for(let strokeTupNode = strokeNode.val0.val1; 
				!(strokeTupNode instanceof EmptyList); 
				strokeTupNode = strokeTupNode.val1){

				//console.log("for loop iteration")
			
				//console.log("Going to draw a point at: " + strokeTupNode.val0[1] + " " + strokeTupNode.val0[0])
				
				context.fillRect(strokeTupNode.val0[1], strokeTupNode.val0[0], 2, 2)

			}

			//Look at the next stroke
			strokeNode = strokeNode.val1

		}

	})

	/*
	We do not care about the information contained in the mouse event 
	when the mouse is first pressed down or released, so we can discard these

	Even though this is not REALLY wildcard syntax as it is declaring
	a variable called "_" that represents the mouse event argument,
	it looks and smells like wildcard syntax and can be used the same
	which is really nice!*/

	//For now, these throw away the mouse event but need to change it to match the other one in future
	
	/*
	const handleMouseUp = _ => {
		onMouseUp()
	}

	const handleMouseDown = _ => {
		console.log("onmousedown val:")
		console.log(onMouseDown)
		onMouseDown()
	}

	const extractCoords = mouseEvent => {
		onMouseMove(mouseEvent.pageY - ref.current.offsetTop)(mouseEvent.pageX - ref.current.offsetLeft)
	}*/

	/*
	NEED TO CHANGE ONMOUSEUP AND ONMOUSEDOWN ARGS TO ALSO TAKE COORDS OF 
	MOUSE CLICKS, THEY CAN JUST USE WILDCARD PATTERN TO THROW AWAY IF THEY DON'T NEED
	*/
	return(
		<Context.Consumer>{dispatch => 
			<canvas
			ref = {ref}
			width = {width}
			height = {height}
			onMouseMove = {e => 
				dispatch(onMouseMove(e.pageY - ref.current.offsetTop)(e.pageX - ref.current.offsetLeft))
			}
			onMouseUp = {e => 
				dispatch(onMouseUp(e.pageY - ref.current.offsetTop)(e.pageX - ref.current.offsetLeft))
			}
			onMouseDown = {e => 
				dispatch(onMouseDown(e.pageY - ref.current.offsetTop)(e.pageX - ref.current.offsetLeft))
			}
			/>
		}</Context.Consumer>
	)

}

/*
export const _Canvas = width => height => pixels => onMouseMove  pixels => onMouseDown => onMouseUp => onMouseMove  => {

	let ref = useRef(null)

	useEffect(() => {

		//let canvas = ref.current
		//let context = canvas.getContext("2d")
		let context = ref.current.getContext("2d")

		//context.fillStyle = "black"
		//context.fillRect(0, 0, width, height)

		

		FOR NOW, TRY INSTEAD HAVING THE EVENT HANDLING FUNCTIONS AS JSX ATTRIBUTES
		TO THE CANVAS RATHER THAN DECLARING THEM IN THIS USEEFFECT FUNCTION,
		I DON'T KNOW IF THIS IS GOING TO WORK BUT I SUSPECT IT WILL BE BETTER
		FOR PERFORMANCE AS IT LOOKS LIKE IF WE ARE DOING IT IN THE WAY THAT IS 
		COMMENTED OUT THEN WE ARE REATTACHING NEW EVENT LISTENERS WHENEVER
		RE-RENDERING OCCURS

		

		
		const extractCoords = mouseEvent => {
			onMouseMove(mouseEvent.pageY - canvas.offsetTop)(mouseEvent.pageX - canvas.offsetLeft)
		}

		
		
		canvas.addEventListener("mouseup", _ => onMouseUp)
		canvas.addEventListener("mousedown", _ => onMouseDown)
		canvas.addEventListener("mousemove", extractCoords)
		

		
		VERSION THAT DOES THE SAME THING BUT USING LOOPS INSTEAD
		OF RECURSION, SHOULD WORK EXACTLY THE SAME IN THEORY!
		AND ALSO SHOULD BE MORE EFFICIENT DUE TO NOT ADDING MULTIPLE
		RECURSIVE FUNCTION CALLS
		
		
		let outerListNode = pixels

		for(let i = 0; !(outerListNode instanceof EmptyList); i++){

			//pixels is a 2D list therefore each element is a list
			let innerListNode = outerListNode.val0

			for(let j = 0; !(innerListNode instanceof EmptyList); j++){

				context.fillStyle = innerListNode.val0
				context.fillRect(j, i, 1, 1)

				innerListNode = innerListNode.val1

			}

			outerListNode = outerListNode.val1

		}
		

		
		impure side effecting recursive functions that go through the 2D
		syphon list and draw each pixel on the canvas in the colour
		that is specified in each cell representing the pixels
		
		const drawRow = (innerListNode, yIndex, xIndex) => {
			
			//If the end of the inner list has been reached then the row has been drawn
			if(innerListNode instanceof EmptyList){
				return
			} else {
				context.fillStyle(innerListNode.val0)
				context.fillRect(xIndex, yIndex, 1, 1)
				drawRow(innerListNode.val1, yIndex, xIndex + 1)
			}

		}

		const drawPixels = (listNode, yIndex) => {
			
			//If the the end of the 2D list has been reached then all pixels have been drawn
			if(listNode instanceof EmptyList){
				return
			} else {
				//The pixels are a 2d list, so draw the row of each inner list then move onto drawing the next row 
				drawRow(listNode.val0, yIndex, 0)
				drawPixels(listNode.val1, yIndex + 1)				
			}

		}

		//The first y index is 0
		drawPixels(pixels, 0)

	})

	
	onMouseMove is expected to be a function that takes the y/x
	coordinates of the mouse event and sends an event message to the update function
	
	//ref.current gets the current reference to the canvas
	const extractCoords = mouseEvent => {
		onMouseMove(mouseEvent.pageY - ref.current.offsetTop)(mouseEvent.pageX - ref.current.offsetLeft)
	}


	
	We do not care about the information contained in the mouse event 
	when the mouse is first pressed down or released, so we can discard these

	Even though this is not REALLY wildcard syntax as it is declaring
	a variable called "_" that represents the mouse event argument,
	it looks and smells like wildcard syntax and can be used the same
	which is really nice! (Cos I'm never ever going to have a name clash issue
	where I have 2 variables called "_" lol)
	
	const handleMouseUp = _ => {
		onMouseUp()
	}

	const handleMouseDown = _ => {
		onMouseDown()
	}

	return(
		<canvas
			ref = {ref}
			width = {width}
			height = {height}
			onMouseMove = {extractCoords}
			//onMouseUp = {handleMouseUp}
			//onMouseDown = {handleMouseDown}
		/>
	)
	
}  */