/*export const Text = content => {
	return(<span>{content}</span>)
}*/
import React from "react"
import {Box, TextField} from "@material-ui/core"
import AceEditor from "react-ace"
import {Context} from "../../App"

const defaultStyle = {
	myName : "george"
}

export const _TextField = label => onChange => {
	return <TextField variant = "outlined"
	onChange = {event => window.$dispatchRef(onChange(event.target.value))} label = {label}/>
}

export const _Text = optional => content => {
	//Implement default optional stuff in a sec
	return <Box><span>{content}</span></Box>
	//return <span>{content}</span>
}

export const _TextEditor = value => minLines => maxLines => onType =>
	<AceEditor
	theme = "monokai"
	name = "EDITOR"
	value = {value}
	onChange = {(newString,_) => window.$dispatchRef(onType(newString))}
	editorProps={{ $blockScrolling: true }}
	minLines = {minLines}
	maxLines = {maxLines}
	/>
