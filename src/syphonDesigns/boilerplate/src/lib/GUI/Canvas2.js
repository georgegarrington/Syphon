import React from "react"
//Needs to be changed later
//import {Context} from "../../CanvasTest"
import {Context} from "../../App"
import {$EmptyList, length} from "../Data/List"

export class Point {
	constructor(color, thickness, y, x){
		this.val0 = color
		this.val1 = thickness
		this.val2 = y
		this.val3 = x
	}
}

export const _Point = arg0 => arg1 => arg2 => arg3 => new Point(arg0,arg1,arg2,arg3)

export class Oval {
	constructor(color,yPos,xPos,width,height){
		this.val0 = color
		this.val1 = yPos
		this.val2 = xPos
		this.val3 = width
		this.val4 = height
	}
}

export const _Oval = arg0 => arg1 => arg2 => arg3 => arg4 => new Oval(arg0,arg1,arg2,arg3,arg4)

export class Rectangle {
	constructor(color,yPos,xPos,width,height){
		this.val0 = color
		this.val1 = yPos
		this.val2 = xPos
		this.val3 = width
		this.val4 = height
	}
}

export const _Rectangle = arg0 => arg1 => arg2 => arg3 => arg4 => new Rectangle(arg0,arg1,arg2,arg3,arg4)

export class Stroke {
	constructor(color,thickness,coordTuples){
		this.val0 = color
		this.val1 = thickness
		this.val2 = coordTuples
	}
}

export const _Stroke = arg0 => arg1 => arg2 => new Stroke(arg0,arg1,arg2)

export const _Canvas = optional => width => height => shapeListNode => {

	const defaultOptional = {
		bgColor : "white",
		onMouseMoved : undefined,
		onMousePressed : undefined,
		onMouseReleased : undefined
	}
	const finalOpt = Object.assign(defaultOptional,optional)

	//useRef hook so that the canvas does not have to re-render
	let ref = React.useRef(null)

	React.useEffect(() => {

		let context = ref.current.getContext("2d")

		//clear the pixels
		context.clearRect(0,0,width,height)
		context.fillStyle = finalOpt.bgColor
		context.fillRect(0,0,width,height)
		
		let pointer = shapeListNode

		while(!(pointer instanceof $EmptyList)){

			const shape = pointer.val0
			const color = shape.val0
			context.fillStyle = color
			
			//INTEGRATE OPTIONAL SHAPE PROPERTIES WITH THESE LINE PROPERTIES
			//context.lineWidth = 3
			//context.strokeStyle = "green"

			if(shape instanceof Stroke){

				//The list of coordinate tuples 
				let coordPointer = shape.val2

				while(!(coordPointer instanceof $EmptyList)){

					//We draw lines between 2 points, so we have clearly reached the end now
					if(coordPointer.val1 instanceof $EmptyList){
						break
					}

					//If there are no tuples in the stroke then dont draw it, look at the next shape
					if(length(coordPointer) == 0){
						pointer = pointer.val1
						break;
					}


					context.strokeStyle = shape.val0
					context.lineWidth = shape.val1

					const [point1Y, point1X] = coordPointer.val0
					const [point2Y, point2X] = coordPointer.val1.val0

					context.beginPath()
					context.moveTo(point1X, point1Y)
					context.lineTo(point2X, point2Y)
					context.stroke()

					//Look at the next tuple coordinate in the "cons" list
					coordPointer = coordPointer.val1

				}

			}

			if(shape instanceof Oval){


				/*
				this.val0 = color
				this.val1 = yPos
				this.val2 = xPos
				this.val3 = width
				this.val4 = height
				*/
				const yPos = shape.val1
				const xPos = shape.val2
				const width = shape.val3 / 2
				const height = shape.val4 / 2
				const yCenter = Math.floor((yPos + height) / 2)
				const xCenter = Math.floor((xPos + width) / 2)


				context.beginPath()
				context.ellipse(xPos + width, yPos + height, width, height, 0, 0, 2 * Math.PI)
				context.fill()
				context.stroke()
				
				/*
				//So that we can revert back to having no scale after
				context.save()
				context.translate(xCenter, yCenter)
				context.scale(xScale, yScale)
				context.beginPath()
				context.arc(1,1,1,0,2 * Math.PI, false)
				context.closePath()
				//So that the scale is no longer applied
				context.restore()
				context.stroke()*/


			}
			if(shape instanceof Rectangle){
				context.beginPath()
				context.rect(shape.val2,shape.val1,shape.val3,shape.val4)
				context.fill()
				context.stroke()
			}

			if(shape instanceof Point){
				context.beginPath()
				context.rect(shape.val3, shape.val2, shape.val1, shape.val1)
				context.fill()
				context.stroke()
			}

			//Look at the next element that is in the cons chain (a linked list)
			pointer = pointer.val1

		}

	})

	const feedCoords = (handler,dispatchRef) => e => {
		if(handler === undefined) return
		dispatchRef(handler(e.pageY - ref.current.offsetTop)(e.pageX - ref.current.offsetLeft))
	}

	return(
		<Context.Consumer>{dispatch => 
			<canvas
			ref = {ref}
			width = {width}
			height = {height}
			onMouseMove = {feedCoords(finalOpt.onMouseMoved,dispatch)}
			onMouseUp = {feedCoords(finalOpt.onMouseReleased,dispatch)}
			onMouseDown = {feedCoords(finalOpt.onMousePressed,dispatch)}
			/>
		}</Context.Consumer>
	)

}