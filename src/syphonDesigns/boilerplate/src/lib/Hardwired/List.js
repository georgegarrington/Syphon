import "./Util"
import { error } from "./Util"

export const head = list => {
	if(list instanceof EmptyList){
		error("head called on an empty list!")
	} else {
		return list.val0
	}
}

//Not sure when I will eery need this but hey ho
export const isEmpty = list => {
	return list instanceof EmptyList
}