use "hw1.sml";

(* is_older tests *)

val test1 = not(is_older((1999,7,1999), (1995,8,8)))

val test2 = is_older((2004,6,3), (2005,9,9))

val test3 = is_older((2020, 5, 1), (2020, 5, 2))

val test4 = is_older((2020, 5, 1), (2020, 5, 2))

val test5 = not(is_older((2020, 5, 1), (2020, 5, 1)))

(* number_in_month tests *)

val test6 = number_in_month([], 2) = 0

val test7 = number_in_month([(2, 1, 2)], 2) = 0

val test8 = number_in_month([(11, 2, 4), (1, 7, 6), (1, 2, 9)], 2) = 2

(* number_in_months tests *)

val test9 = number_in_months([(11, 2, 4), (1, 7, 6), (1, 2, 9)], [2, 7]) = 3

val test10 = number_in_months([], [1, 2, 3, 4, 6, 7, 8, 9]) = 0

val test11 = number_in_months([(3, 5, 1), (6, 4, 1)], [1, 9]) = 0

(* dates_in_month tests *)

val test12 = dates_in_month([], 2) = []

val test13 = dates_in_month([(2020, 2, 1), (2020, 5, 1)], 2) = [(2020, 2, 1)]

val test14 = dates_in_month([(6, 2, 6), (2020, 6, 1), (2010, 6, 4)], 6) = [(2020, 6, 1), (2010, 6, 4)]

(* dates_in_months tests *)

val test15 = dates_in_months([], [2, 5, 6]) = []

val test16 = dates_in_months([(6, 2, 6), (2020, 6, 1)], [2, 6]) = [(6, 2, 6), (2020, 6, 1)]

(* get_nth tests *)

val test17 = get_nth(["one", "two", "three"], 2) = "two"

(* date_to_string tests *)

val test18 = date_to_string (2020, 5, 1) = "May 1, 2020"

(* number_before_reaching_sum *)

val test19 = number_before_reaching_sum ( 10, [5, 4, 1] ) = 2

val test20 = number_before_reaching_sum ( 10, [11] ) = 0

(* what_month *)

val test21 = what_month(365) = 12

val test22 = what_month(31) = 1

(* month_range *)

val test23 = month_range (30, 32) = [1, 1, 2]

val test24 = month_range (32, 30) = []

(* oldest *)

val test25 = oldest([]) = NONE

val test26 = oldest ([ (2020, 5, 9), (2020, 5, 10), (1995, 12, 21) ]) = SOME (1995, 12, 21)

val test27 = oldest( [ (1850, 12, 10) ]) = SOME (1850, 12, 10)
						
(* uniq *)

val test28 = uniq ( List.tabulate (100, fn x => 2) ) = [2]

(* number_in_months_challenge *)

val test29 = number_in_months_challenge ( [(1, 3, 5), (6, 3, 12), (5, 1, 555)], [3, 3, 2]) = 2

(* dates_in_months_challenge *)

val test30 = dates_in_months_challenge ( [(1, 3, 5), (6, 3, 12), (5, 1, 555)], [3, 3, 2]) = [(1,3,5),(6,3,12)]

(* reasonable_date *)

val test31 = reasonable_date (1600, 2, 29)

val test32 = not ( reasonable_date (2200, 2, 29) )

val test33 = reasonable_date (1632, 2, 29)

val test34 = not ( reasonable_date(0, 5, 5) )

val test35 = not ( reasonable_date(2000, 0, 5) )

val test36 = not ( reasonable_date(2000, 13, 5) )

val test37 = not ( reasonable_date(2000, 4, 31) )
