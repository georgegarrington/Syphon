import React from "react"
import {_Canvas, Rectangle, Oval, Stroke, Point} from "./lib/GUI/Canvas2"
import * as L from "./lib/Data/List"
import * as C from "./lib/Hardwired/Core"
//import { _Canvas } from "./lib/GUI/Canvas"

export const Context = React.createContext()

class PixelEntered{constructor(arg0,arg1){this.val0 = arg0, this.val1 = arg1}}
const _PixelEntered = arg0 => arg1 => new PixelEntered(arg0,arg1)

const init = L.js2syphon([new Rectangle("red",0,0,100,100), new Rectangle("green", 150,150,50,50), 
	new Oval("blue", 200, 200, 100, 50), new Stroke("blue", 10, L.js2syphon([ [50,50],[50,100],[100,100],[100,150]]))])

const update = (state,event) => {

	if(event instanceof PixelEntered){

		const newPoint = new Point("black", 3, event.val0, event.val1)
		return L.$_Cons(newPoint)(state)

	} else {

	//Doesnt do anything for the moment
	return state
	
	}

}

export const View = () => {
	const [state, $dispatch] = React.useReducer(update, init)
	return <Context.Provider value = {$dispatch}>
		{_Canvas({bgColor : "black", onMouseReleased: _PixelEntered})(900)(500)(state)}
	</Context.Provider>
}