val test01_01 = is_older((1,2,3),(2,3,4)) = true	
val test01_02 = is_older((1999,2,13),(2012,3,24)) = true
val test01_03 = is_older((2,3,4),(1,2,3)) <> true
val test01_04 = is_older((1,2,3),(1,2,3)) <> true

val test02_01 = number_in_month([(2012,2,28),(2013,12,1)],2) = 1
val test02_02 = number_in_month([(2012,2,28),(2013,12,1)],2) <> 2
val test02_03 = number_in_month([(2012,2,28),(2012,2,28),(2012,2,28),(2013,12,1)],2) = 3
val test02_04 = number_in_month([(2012,2,28),(2012,2,28),(2012,2,28),(2013,12,1)],2) <> 4
val test02_05 = number_in_month([],2) <> 4
val test02_06 = number_in_month([(2012,3,28),(2013,12,1)],2) = 0
val test02_07 = number_in_month([],2) = 0

val test03_01 = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test03_02 = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) <> 4
val test03_03 = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[4]) = 1
val test03_04 = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) <> 0
val test03_05 = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) <> 10
val test03_06 = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = 0
val test03_07 = number_in_months([],[2,3,4]) <> 3
val test03_08 = number_in_months([],[2,3,4]) = 0
val test03_09 = number_in_months([],[]) = 0

val test04_01 = dates_in_month([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test04_02 = dates_in_month([(2012,2,28),(2012,2,20),(2012,2,18),(2013,12,1)],2) = [(2012,2,28),(2012,2,20),(2012,2,18)]
val test04_03 = dates_in_month([(2012,2,28),(2012,2,20),(2012,2,18),(2013,12,1)],2) <> [(2012,2,18),(2012,2,20),(2012,2,28)]
val test04_04 = dates_in_month([(2012,2,28),(2012,2,20),(2012,2,18),(2013,12,1)],2) <> []
val test04_05 = dates_in_month([],2) = []
val test04_06 = dates_in_month([(2012,2,28),(2012,2,20),(2012,2,18),(2013,12,1)],11) = []

val test05_01 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
val test05_02 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2]) = [(2012,2,28)]
val test05_03 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[4]) = [(2011,4,28)]
val test05_04 = dates_in_months([],[2,4]) = []
val test05_05 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = []

val test06_01 = get_nth(["hi", "there", "how", "are", "you"], 2) = "there"
(* val test06_02 = get_nth([], 2) = "there"  EXCEPTION! *)
(* val test06_03 = get_nth(["hi", "there", "how", "are", "you"], 20) = "there"  EXCEPTION! *)
(* val test06_04 = get_nth(["hi", "there", "how", "are", "you"], -2) = "there"  EXCEPTION! *)
(* val test06_05 = get_nth(["hi", "there", "how", "are", "you"], 0) = "there" EXCEPTION! *)

val test07_01 = date_to_string((2013, 6, 1)) = "June 1, 2013"
val test07_02 = date_to_string((1988, 3, 21)) = "March 21, 1988"

val test08_01 = number_before_reaching_sum(10, [1,2,3,4,5]) = 3
val test08_02 = number_before_reaching_sum(10, [1,1,1,1,1,1,1,1,1,1,1]) = 9
val test08_03 = number_before_reaching_sum(10, [10,12]) = 0

val test09_01 = what_month(70) = 3
val test09_02 = what_month(80) = 3

val test10_01 = month_range(31, 34) = [1,2,2,2]

val test11_01 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)