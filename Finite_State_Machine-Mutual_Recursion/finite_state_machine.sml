(* 
 * Type: list => bool
 * This function takes a list, and returns:
 * TRUE -- if the list has the pattern [1, 0, 1, 0, 1]
 * FALSE -- otherwise
 * Operation:
 *  1. If the list is empty, return true. E.g., []
 *  2. If the current element is 1, return state_is_zero(list')  E.g., [1]
 *    state_is_zero():
 *     a. If the list is empty, return false. E.g., [1]
 *     b. If the next element is 0, return state_is_one(list')  E.g., [0, 1]
 *     c. Otherwise, false. E.g., [1, 1]
 *  3. Otherwise, false. E.g., [2, 1]
*)
fun list_is_alternating list =
	let fun state_is_one(list) = 
			case list of
				[] => true
				| 1::list' => state_is_zero(list') 
				| _ => false
		and state_is_zero(list) =
			case list of
				[] => false
				| 0::list' => state_is_one(list')
				| _ => false
	in
		state_is_one list
	end