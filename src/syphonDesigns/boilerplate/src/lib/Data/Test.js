

const mul = arg0 => {
	if(length(arg0) >= 1){
		const x = index(arg0)(0)
		return x
	}
	if(length(arg0) >= 1){
		const x = index(arg0)(0)
		const xs = drop(1)(arg0)
		return x * mul(xs)
	}
}