import React from "react"
import {Box} from "@material-ui/core"
import { Context } from "../../App"




export const _Box = opt => {
	const defaultStyle = {
		backgroundColor: "red",
		width: 30,
		height: 30
	}
	return <Box style = {defaultStyle}/>
}

/*

export const _Button = optional => content => event => {
	//const {dispatch} = useContext(Context)
	const defaultOptional = {
		bgColor : P.lightGrey,
		dim: [undefined,undefined],
		margin : 2
	}
	const finalOpt = Object.assign(defaultOptional, optional)
	console.log("background colour:")
	console.log(finalOpt.bgColor)
	const style = {
		width: finalOpt.dim[0],
		height: finalOpt.dim[1],
		backgroundColor: finalOpt.bgColor,
		margin: finalOpt.margin
	}
	return(
		<Context.Consumer>{dispatch => 
			<Original style = {style} onClick = {() => dispatch(event)}>{content}</Original>
		}</Context.Consumer>
	)
	
}



*/



//A panel is simply a container with no child widget, undefined acts as a dummy widget
export const _Panel = optional => _Container(optional)(undefined)

export const _Graphic = optional => graphic => {
	console.log("The value of graphic is:")
	console.log(graphic)
	return _Container(optional)(graphic)
}

//A container has a child widget, a panel does not
export const _Container = optional => childWidget => {

	/*
	It does not matter if there are non styling properties in the optional structure and they are left
	in the style object given as an argument to the widget, I have tested it and React does not care
	if there are fields that are not used so they can just be left in the object. We can simply extract
	the important ones e.g. onRightClick, change dimensions to width and height etc.
	*/
	const defaultOptional = {
		bgColor : "#e0e0e0",
		dim: [undefined,undefined],
		margin: 2
	}
	const finalOpt = Object.assign(defaultOptional, optional)
	const style = {
		width: finalOpt.dim[0],
		height: finalOpt.dim[1],
		backgroundColor: finalOpt.bgColor,
		margin: finalOpt.margin,
		alignItems: "center"
	}

	return(
		<Box style = {style} display = "flex" alignItems = "center" justifyContent = "center">{childWidget}</Box>
	)

}

//EVERY GUI data type MUST take an optional argument
export const _Icon = optional => iconValue => {

	const optionalStructureDefaults = {

	}
	const finalOpt = Object.assign(opt, optionalStructureDefaults)

}