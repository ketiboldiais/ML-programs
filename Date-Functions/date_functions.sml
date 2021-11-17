(*
	function: isOlder
		(int*int*int), (int*int*int) --> (int*int*int)  
		consumes two dates, returns true if the first date is older, otherwise false
	data
		date is a 3-tuple
			#1 is type int := year
			#2 is type int := month 
			#3 is type int := day

		firstDate is of type date
		secondDate is of type date
*)
fun isOlder (firstDate : int*int*int, secondDate : int*int*int) =
	if (#1 firstDate) < (#1 secondDate)
		then true
	else
		let 
			val sameYear = ((#1 firstDate) = (#1 secondDate)) 
			val sameMonth = ((#2 firstDate) = (#2 secondDate)) 
		in
			if ((#2 firstDate) < (#2 secondDate)) andalso sameYear
				then true
			else if ((#3 firstDate) < (#3 secondDate)) andalso sameMonth
				then true
			else
				false
		end
(* val result = isOlder((1, 1, 2), (1, 1, 3)) *)
(*
	function: number_in_month
	int list --> int

	takes a list of dates and a month, and returns how many dates in the list are in the given month

	data:
		month := type int, representing a month
		dateList := type date list, representing a list of dates
*)
fun number_in_month (dateList : (int*int*int) list, month : int) =
	if null dateList
		then 0
	else
		if (#2 (hd dateList) = month)
			then 1 + number_in_month((tl dateList), month)
		else number_in_month((tl dateList), month)
(* val result = number_in_month([(1,1,2), (1,1,2), (1,2,2), (2,2,2)], 1) *)
(*
	function: number_in_months 
	listOf dates, listOf months --> int

	takes a list of dates and a list of months, returns the number of dates in the list of dates that are in any of the months in the list of months

	data:
		monthList := type month list, representing a list of months

	val result = number_in_months([(1,2,3), (3,4,5)], [1, 2, 3])
		output: 3

	fun number_in_months(dateList : (int*int*int) list, monthList : int list) =
		if null dateList
			then 0
		else
			if ... #2 dateList ... hd monthList
				then 1 + ...
			else
				number_in_months(tl dateList, tl monthList)
*)
fun number_in_months(dateList : (int*int*int) list, monthList : int list) =
	if null monthList
		then 0
	else
		let
			val currentMonth = hd monthList
			val remainingMonths = tl monthList
			val monthMatch = number_in_month(dateList, currentMonth)
		in
			if null (tl monthList)
				then monthMatch
			else
				monthMatch + number_in_months(dateList, remainingMonths)
		end
(* val result = number_in_months([(1,1,3), (3,2,5), (1,1,4), (1, 3, 4), (1, 5, 6)], [1, 2, 5]) *)
(* 
	Function: dates_in_month
		dateList, month --> dateList
		takes a list of dates and a month, returns a list of dates whose month matches the month

	Data:
		dateList := list of dates : type (int*int*int) list
		month := month : type int

	dates_in_month ([(1,1,2), (1,2,3), (1,1,4)], 1) --> [(1,1,2), (1,1,4)]

	Template:
		fun dates_in_month(dateList : (int*int*int) list, month : int) =
			if null dateList
				then []
			else
				if (#2 (hd dateList)) = month 
					then (hd dateList) :: dates_in_month(tl dateList, month)
				else
					dates_in_month(tl dateList, month)
*)
fun dates_in_month(dateList : (int*int*int) list, month : int) =
	if null dateList
		then []
	else
		let
			val current_date = hd dateList
			val remaining_dates = tl dateList
		in
			if (#2 current_date) = month
				then current_date :: dates_in_month(remaining_dates, month)
			else
				dates_in_month(remaining_dates, month)
		end
(* val result = dates_in_month([(1,1,2), (1,2,3), (1,1,4)], 1) *)

(* 
	function: dates_in_month
		dateList, monthList --> dateList
		takes a list of dates and a list of months, returns a list of dates with months that are in the list of months.

	data:
		dateList := (int*int*int) list
		monthList := int list

	fun dates_in_months(dateList : (int*int*int) list, monthList : int list) = 
		if ...
		else ...

	val result = dates_in_months( [(1,1,2), (1,2,3), (3,4,5)], [1, 2, 5] ) --> [(1, 1, 2), (1, 2, 3)]
	val result = dates_in_months( [], [1, 2]) --> []
	val result = dates_in_months( [(1,2,3), (3,4,5), (4,5,6)], [5,6] ) --> [(4,5,6)]
	val result = dates_in_months( [(1,2,3), (3,4,5), (4,5,6)], [4,8] ) --> [(3,4,5)]

	fun dates_in_months(dateList : (int*int*int) list, monthList : int list) =
		if null dateList
			then []
		else
			if month in dateList is same as month in monthList
				then add the date for that month into the new list
			else check the other months in the monthList
*)

	fun dates_in_months(dateList : (int*int*int) list, monthList : int list) =
		if null monthList
			then []
		else
			let
				val remainingMonths = tl monthList
				val currentMonth = hd monthList
				val monthMatch = dates_in_month(dateList, hd monthList)
			in
				if null remainingMonths
					then monthMatch
				else
					dates_in_month(dateList, currentMonth) @ dates_in_months(dateList, remainingMonths)
			end

(*
	function: get_nth
	stringList, int --> string
	takes a stringList and an int n, and returns the string with position n

	data:
		stringList := a list of strings -- string list
		nth --> int --> the nth element in stringList

	fun get_nth(stringList : string list, nth : int)

	val first_test = get_nth(["john", "anne", "bill"], 1) --> "john" 
	val second_test = get_nth([]) --> ""
	val third_test = get_nth(["john", "anne", "bill"], 3) --> "bill"
*)
	fun get_nth(stringList : string list, nth : int) =
		if null stringList
			then ""
		else
			if nth = 1
				then hd stringList
			else
				get_nth(tl stringList, nth - 1)
(* Tests
	val first_test = get_nth(["john", "anne", "bill"], 1)
	val second_test = get_nth([], 1)
	val third_test = get_nth(["john", "anne", "bill"], 3)		 
*)

(*
	Function: date_to_string
		date --> stringDate
		consumes a date, and returns a string of the form "Month Day, Year"

	Data:
		date := (int*int*int)
			#1 date := year
			#2 date := month
			#3 date := day
		stringDate := string  form: "Month Day, Year"
		list of months:
		["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]

	fun date_to_string(date : (int*int*int), stringDate : string)

	date_to_string((2020, 1, 1)) --> January 1, 2020
*)

	fun date_to_string(date : (int*int*int)) =
		let 
			val monthList = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
			val year = Int.toString(#1 date)
			val month = get_nth(monthList, #2 date)
			val day = Int.toString(#3 date)
		in
			month ^ " " ^ day ^ ", " ^ year
		end

val firstTest = date_to_string((2020, 1, 1))
val secondTest = date_to_string((1994, 10, 13))
val thirdTest = date_to_string((2016, 12, 25))

(* 
	Function: number_before_reaching_sum
	intList, int --> int
	takes an intList and an int, returns an int such that the first n elements adds to less than sum, but the first n+1 elements add to sum or more 

	data:
		intList := list of int -- int list
		sum := the sum that the result of summing int list should be less than or greater than

	number_before_reaching_sum([1,2,3], 4), 1+2 = 3, 1+2+3 = 6
	number_before_reaching_sum([20, 19, 11], 15)  
*)
fun number_before_reaching_sum(nums : int list, sum : int) =
	if hd nums >= sum
		then 0
	else
		1 + number_before_reaching_sum(tl nums, sum - (hd nums))

val firstTest = number_before_reaching_sum([1, 2, 3, 4], 4)
				
			