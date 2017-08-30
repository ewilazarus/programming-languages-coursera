 (* Auxiliar functions *)

fun get_year(date: int * int * int) =
	#1 date

fun get_month(date: int * int * int) =
	#2 date

fun get_day(date: int * int * int) =
	#3 date


(* 01: Write a function is_older that takes two dates and evaluates to true or false. It evaluates to true if
the first argument is a date that comes before the second argument. (If the two dates are the same,
the result is false.) *)

fun is_older(d1: int * int * int, d2: int * int * int) =
	let
		val d1_year = get_year d1
		val d1_month = get_month d1
		val d1_day = get_day d1

		val d2_year = get_year d2
		val d2_month = get_month d2
		val d2_day = get_day d2
	in
		d1_year < d2_year orelse
		d1_year = d2_year andalso d1_month < d2_month orelse
		d1_year = d2_year andalso d1_month = d2_month andalso d1_day < d2_day
	end


(* 02: Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns
how many dates in the list are in the given month.*)

fun number_in_month(dates: (int * int * int) list, month: int) =
	if null dates
	then 0
	else
		if get_month(hd dates) = month
		then
			1 + number_in_month(tl dates, month)
		else
			number_in_month(tl dates, month)


(* 03: Write a function number_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns the number of dates in the list of dates that are in any of the months in the list of months.
Assume the list of months has no number repeated. Hint: Use your answer to the previous problem. *)

fun number_in_months(dates: (int * int * int) list, months: int list) =
	if null months
	then 0
	else number_in_month(dates, hd months) + number_in_months(dates, tl months)


(* 04: Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a
list holding the dates from the argument list of dates that are in the month. The returned list should
contain dates in the order they were originally given. *)

fun dates_in_month(dates: (int * int * int) list, month: int) =
	if null dates
	then []
	else
		if get_month(hd dates) = month
		then
			hd dates :: dates_in_month(tl dates, month)
		else
			dates_in_month(tl dates, month)


(* 05: Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns a list holding the dates from the argument list of dates that are in any of the months in
the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the
previous problem and SML's list-append operator (@). *)

fun dates_in_months(dates: (int * int * int) list, months: int list) =
	if null months
	then []
	else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)


(* 06: Write a function get_nth that takes a list of strings and an int n and returns the nth element of the
list where the head of the list is 1st. Do not worry about the case where the list has too few elements:
your function may apply hd or tl to the empty list in this case, which is okay. *)

fun get_nth(strings: string list, n: int) =
	if n = 1
	then hd strings
	else get_nth(tl strings, n-1)


(* 07: Write a function date_to_string that takes a date and returns a string of the form January 20, 2013
(for example). Use the operator ^ for concatenating strings and the library function Int.toString
for converting an int to a string. For producing the month part, do not use a bunch of conditionals.
Instead, use a list holding 12 strings and your answer to the previous problem. For consistency, put a
comma following the day and use capitalized English month names: January, February, March, April,
May, June, July, August, September, October, November, December. *)

fun date_to_string(date: int * int * int) =
	let
		val months = [
			"January", 		(* 01 *)
			"February",		(* 02 *)
			"March",		(* 03 *)
			"April",		(* 04 *)
			"May",			(* 05 *)
			"June",			(* 06 *)
			"July",			(* 07 *)
			"August",		(* 08 *)
			"September",	(* 09 *)
			"October",		(* 10 *)
			"November",		(* 11 *)
			"December"		(* 12 *)
		]
	in
		get_nth(months, get_month(date)) ^ " " ^ Int.toString(get_day(date)) ^ ", " ^ Int.toString(get_year(date))
	end


(* 08: Write a function number_before_reaching_sum that takes an int called sum, which you can assume
is positive, and an int list, which you can assume contains all positive numbers, and returns an int.
You should return an int n such that the first n elements of the list add to less than sum, but the first
n+1 elements of the list add to sum or more. Assume the entire list sums to more than the passed in
value; it is okay for an exception to occur if this is not the case. *)

fun number_before_reaching_sum(sum: int, numbers: int list) =
	let
		val sum_aux = sum - hd numbers
	in
		if sum_aux > 0
		then 1 + number_before_reaching_sum(sum_aux, tl numbers)
		else 0
	end


(* 09: Write a function what_month that takes a day of year (i.e., an int between 1 and 365) and returns
what month that day is in (1 for January, 2 for February, etc.). Use a list holding 12 integers and your
answer to the previous problem.) *)

fun what_month(day: int) =
	let
		val months = [
			31,		(* JANUARY *)
			28,		(* FEBRUARY *)
			31,		(* MARCH *)
			30,		(* APRIL *)
			31,		(* MAY *)
			30,		(* JUNE *)
			31,		(* JULY *)
			31,		(* AUGUST *)
			30,		(* SEPTEMBER *)
			31,		(* OCTOBER *)
			30,		(* NOVEMBER *)
			31		(* DECEMBER *)
		]
	in
		number_before_reaching_sum(day, months) + 1
	end


(* 10: Write a function month_range that takes two days of the year day1 and day2 and returns an int list
[m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month
of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1>day2. *)

fun month_range(day1: int, day2: int) =
	if day1 > day2
	then []
	else what_month(day1) :: month_range(day1 + 1, day2)


(* 11: Write a function oldest that takes a list of dates and evaluates to an (int*int*int) option. It
evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list. *)

fun oldest(dates: (int * int * int) list) =
	if null dates
	then NONE
	else
		let
			val d1 = hd dates
			val d2 = oldest(tl dates)
		in
			if isSome d2
			then
				if is_older(d1, valOf d2)
				then SOME d1
				else d2
			else SOME d1
		end


(* 12: Challenge Problem: Write functions number_in_months_challenge and dates_in_months_challenge
that are like your solutions to problems 3 and 5 except having a month in the second argument multiple
times has no more effect than having it once. (Hint: Remove duplicates, then use previous work.) *)

fun dedup(occurrences: int list) =
	if null occurrences
	then []
	else
		let
			fun dedup_aux(os: int list) =
				if null os
				then []
				else
					if hd occurrences = hd os
					then dedup_aux(tl os)
					else hd os :: dedup_aux(tl os)
		in
			hd occurrences :: dedup(dedup_aux(tl occurrences))
		end


(* 13: Challenge Problem: Write a function reasonable_date that takes a date and determines if it
describes a real date in the common era. A "real date" has a positive year (year 0 did not exist), a
month between 1 and 12, and a day appropriate for the month. Solutions should properly handle leap
years. Leap years are years that are either divisible by 400 or divisible by 4 but not divisible by 100.
(Do not worry about days possibly lost in the conversion to the Gregorian calendar in the Late 1500s.) *)

fun reasonable_date(date: int * int * int) =
	let
		val is_valid_year = get_year date > 0
		val is_valid_month = get_month date >= 1 andalso get_month date <= 12

		val is_leap_year = get_year date mod 400 = 0 orelse
		 get_year date mod 4 = 0 andalso get_year date mod 100 <> 0

		val days_in_months = if is_leap_year
		then [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
		else [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

		fun get_nth(ints: int list, n: int) =
			if n = 1
			then hd ints
			else get_nth(tl ints, n-1)

		val is_valid_day = is_valid_month andalso
		 get_day date >= 1 andalso get_day date <= get_nth(days_in_months, get_month date)
	in
		is_valid_day andalso is_valid_month andalso is_valid_year
	end
