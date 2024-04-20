fun is_older (date1: (int*int*int),date2: (int*int*int)) = 
    (#1 date1 < #1 date2) orelse
    (#1 date1 = #1 date2 andalso (#2 date1 < #2 date2)) orelse
    (#1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 < #3 date2)

fun get_month (date: (int*int*int)) = #2 date

fun number_in_month (dates : (int*int*int) list, target: int) = 
    let fun count_month (count : int, dates: (int*int*int) list, target : int) =
        if null dates then count
            else 
                if target = get_month(hd dates) then count_month(count + 1, tl dates, target) 
                else count_month(count, tl dates, target)
    in
        count_month(0, dates, target)
    end

fun number_in_months (dates : (int*int*int) list, targets: int list) = 
    let fun count_months (count : int, dates: (int*int*int) list, targets : int list) =
        if null targets then count
        else 
            count_months (count + number_in_month(dates, hd targets), dates, tl targets)
    in
        count_months(0, dates, targets)
    end

fun dates_in_month (dates : (int*int*int) list, target: int) = 
    if null dates then []
    else 
        if #2 (hd dates) = target then (hd dates)::dates_in_month(tl dates, target) 
        else dates_in_month(tl dates, target)

fun dates_in_months (dates : (int*int*int) list, targets: int list) = 
    if null targets then []
    else 
        dates_in_month(dates, hd targets) @ dates_in_months(dates, tl targets)

fun get_nth (lis: string list, num: int) = 
    let fun count_nth(lis: string list, count: int, target: int) = 
        if count = target then hd lis
        else count_nth(tl lis, count + 1, target)
    in
        count_nth(lis, 1, num)
    end


(*
fun date_to_string (2013, 6, 1) = "June 1, 2013"

fun number_before_reaching_sum (10, [1,2,3,4,5]) = 3

fun what_month 70 = 3

fun month_range (31, 34) = [1,2,2,2]

fun oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
 *)