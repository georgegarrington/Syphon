import React from "react"
import {Box, Grid} from "@material-ui/core"
import * as L from "../Data/List"

//Much nicer doing it in a function way :)
export const Row = props => {
	const defaultStyle = {gap: 1}
	const sty = props.style === undefined ? defaultStyle : props.style
	return(
		<Box style = {{backgroundColor: "white"}}
		display = "flex" flexDirection = "row" alignItems = "center" justify = "center">
			{props.children}
		</Box>
	)
}

//Pure function version to abstract away having to write any JSX, at the moment
//all this does is make a plain row with the given children but doesn't give 
//any option for styling etc, so maybe change this later
export const _Row = children => {
	return (<Row>{L.$syphon2js(children)}</Row>)
}

export const Column = props => {

	const defaultStyle = {gap: 1}
	const sty = props.style === undefined ? defaultStyle : props.style
		
	return(
		//<div style = {{display : "flex", flexDirection : "column", alignItems : "start"}}></div>
		
		<Box style = {{backgroundColor : "white", gap: 3}}
		display = "flex" flexDirection = "column" justify = "center" alignItems = "center">
			{props.children}
		</Box>
	)
}

//Takes a child widget and wraps around it
export const _Box = child => {
	
	return(
		<Box >

		</Box>
	)

}

export const _Column = optional => children => {


	const val = L.$syphon2js(children)

	return <Box style = {{gap : optional.gap}} display = "flex" flexDirection = "column"
		justify = "center" alignItems = "center">
		{L.$syphon2js(children)}
		</Box>

	/*
	return(<Grid direction = "column" justify = "center" bgcolor = "blue" alignItems = "center" style = {{backgroundColor : "blue"}}>
		{val}
	</Grid>)*/

}

export const _Panel = children => {
	return (<Box></Box>)
}

export const _DummyWidget = () => <div></div>

/*
export class Row extends React.Component {
	render(){
		return <Box style = {this.props.style} display = "flex" flexDirection = "row">
			{this.props.children}
		</Box>

		
		//AN EXAMPLE OF OVERRIDING STYLE
		const customStyle = this.props.style
		return <Box style = {customStyle === undefined ? {backgroundColor : "pink"} : this.props.style} display = "flex" flexDirection = "row">
			{this.props.children}
		</Box>
	}
}*/

/*
export class Column extends React.Component {
	render(){
		return <Box style = {this.props.style} display = "flex" flexDirection = "column">
			{this.props.children}
		</Box>
	}
}*/

/*
export class Row extends Box {
	
	//return <Box display = "flex" flex-direction = "row"></Box>
}*/
