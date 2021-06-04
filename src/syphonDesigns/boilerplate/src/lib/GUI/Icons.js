import React from "react"
import SaveIcon from "@material-ui/icons/Save"

import explosionSrc from "./assets/explosion.png"
import mineSrc from "./assets/mine.png"

export const saveIcon = () => <SaveIcon style = {{backgroundColor : "red"}}/>

export const explosion = () => <img style = {{width: 30, height: 30}} src = {explosionSrc}/>
export const mine = () => <img style = {{width: 30, height: 30}} src = {mineSrc}/>

export const $mkImg = opt => source => {
	const defaultOpt = {dim: [undefined,undefined]}
	const finalOpt = Object.assign(defaultOpt, opt)
	const style = {
		width: finalOpt.dim[0],
		height: finalOpt.dim[1]
	}
	return <img style = {style} src = {source}/>
}