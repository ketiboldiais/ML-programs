

fun isEven n =	
	let fun is_even n = 
		if n = 0 then true
			else is_odd(n - 1)
		and is_odd n =
			if n = 0 then false
			else is_even(n - 1)
	in
		is_even n
	end

val x = isEven 4
val y = isEven 5
val z = isEven 6