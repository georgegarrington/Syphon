import React from "react"
import {Button as Original} from "@material-ui/core"
//import {Context} from "../../ManualMineSweeper"
//import {Context} from "../../cuttingEdge/App"
//import {Context} from "../../tests/PaintV2/PaintV2"
//import {Context} from "../../PongTest2"
import {Context} from "../../App"
import * as P from "./Palette"
import { LocalConvenienceStoreOutlined } from "@material-ui/icons"

/*
export class Button extends React.Component {
	render(){
		//If the user has defined a style then this will not be undefined
		const customStyle = this.props.style
		const defaultStyle = {
			backgroundColor : P.lightGrey,
			margin : 4
		}
		return <Original style = {customStyle === undefined ? defaultStyle : customStyle}>
			{this.props.children}
		</Original>
	}
}*/

export const Button2 = optional => childWidget => event => {

	const defaultOptional = {
		
	}

}

//How to make react functional components :)
export const Button = props => {

	//const {state, dispatch} = useContext(Context)

	//console.log("Value of dispatch in Button:")
	//console.log(dispatch)

	//If the user has not given a style then use the default style and
	//if they have given a style then use that instead
	const defaultStyle = {
		backgroundColor : P.lightGrey,
		margin : 4
	}
	const sty = props.style === undefined ? defaultStyle : props.style

	return(
		<Context.Consumer>{dispatch => 
			<Original style = {{...sty, maxWidth: 300, minWidth: 300}} onClick = {() => dispatch(props.onClick)}>{props.children}</Original>
		}</Context.Consumer>
	)

}
		  

export const _Button = optional => childWidget => event => {
	const defaultOptional = {
		bgColor : P.lightGrey,
		dim: [undefined,undefined],
		margin : 2,
		onRightClick : undefined,
		style: 2
	}
	const finalOpt = Object.assign(defaultOptional, optional)
	const style = {
		maxWidth: finalOpt.dim[0],
		minWidth: finalOpt.dim[0],
		maxHeight: finalOpt.dim[1],
		minHeight: finalOpt.dim[1],
		backgroundColor: finalOpt.bgColor,
		margin: finalOpt.margin,
		borderRadius: finalOpt.style
	}
	return(
		<Context.Consumer>{dispatch => 
			//onContextMenu is literally onRightClick
			<Original style = {style} onClick = {() => dispatch(event)} 
			onContextMenu = {e =>{e.preventDefault(); dispatch(finalOpt.onRightClick)}}>{childWidget}</Original>
		}</Context.Consumer>
	)

}
