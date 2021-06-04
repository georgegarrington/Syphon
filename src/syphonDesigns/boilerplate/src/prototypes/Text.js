import * as L from "../lib/Data/List"
import React, {Component, useReducer, useEffect} from "react"
import {_Button} from "../lib/GUI/Button"
import {_Text} from "../lib/GUI/Text"
import {_Row, _Column} from "../lib/GUI/Layout"
import {Grid, Paper, createMuiTheme, ThemeProvider, Box} from "@material-ui/core"
import prand from "pure-rand"

//This is how to do custom spacing factor, so just 1 pixel
const theme = createMuiTheme({
	spacing : 1
})

const mkBox = () =>
 <Box style = {{backgroundColor: "red", height : 30, width : 30}}></Box>

export const View = () =>
	<Box 
	display = "flex"
	flexDirection = "column"
	justifyContent = "flex-start"
	spacing = {1}
	style = {{
		backgroundColor : "blue",
		//display : "flex",
		//direction : "row",
		gap: 3,
	}}>
	{(() => {
		let rows = []
		for(let i = 0; i < 10; i++){
			let elems = []
			for(let j = 0; j < 10; j++){
				elems.push(<Box style = {{backgroundColor : "red", width : 40, height : 40, margin : 1, borderRadius : 3}}></Box>)
			}
			rows.push(<Box display = "flex" flexDirection = "row" justifyContent = "flex-start">{elems}</Box>)
		}
		return rows
	})()}
	</Box>

	/*
	{(() => {

		let rows = []

		for(let j = 1; j < 2; j++){

			let list = []
			for(let i = 0; i < 10; i++){
				list.push(mkBox())
			}
			rows.push()

		}

		return rows

	})()}

<Box display = "flex" flexDirection = "row" justifyContent = "flex-start">
		<Box style = {{backgroundColor : "red", width : 30, height : 30}}></Box>
		<Box style = {{backgroundColor : "red", width : 30, height : 30}}></Box>
		<Box style = {{backgroundColor : "red", width : 30, height : 30}}></Box>
		<Box style = {{backgroundColor : "red", width : 30, height : 30}}></Box>
		<Box style = {{backgroundColor : "red", width : 30, height : 30}}></Box>
	</Box>
	<Box display = "flex" flexDirection = "row" justifyContent = "flex-start">
		<Box style = {{backgroundColor : "red", width : 30, height : 30}}></Box>
		<Box style = {{backgroundColor : "red", width : 30, height : 30}}></Box>
		<Box style = {{backgroundColor : "red", width : 30, height : 30}}></Box>
		<Box style = {{backgroundColor : "red", width : 30, height : 30}}></Box>
		<Box style = {{backgroundColor : "red", width : 30, height : 30}}></Box>
	</Box>

*/