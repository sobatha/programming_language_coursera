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

val month_name = ["January", "February", "March", "Spring", "May", "June", "July", "August", 
                "September", "October", "November", "December"]

fun date_to_string (year : int, month : int, day: int) = 
    get_nth(month_name, month) ^ " " ^ Int.toString day ^ "," ^ " " ^ Int.toString year

fun number_before_reaching_sum (target : int, lis: int list) = 
    let
        fun count_before_reaching_sum (lis: int list, accum: int, target: int, count: int) =
            if accum >= target then count
            else if null lis then count
            else count_before_reaching_sum(tl lis, accum + hd lis, target, count + 1)
    in
        if null lis then 0
        else count_before_reaching_sum(tl lis, hd lis, target, 0)
    end

val month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

fun what_month (day: int) = 
    number_before_reaching_sum(day, month_days) + 1
    
fun month_range (from: int, to: int) =
    if from > to then []
    else what_month(from)::month_range(from+1, to)

fun oldest(dates: (int*int*int) list) = 
    if null dates then NONE
    else 
        let val tl_ans = oldest(tl dates)
        in 
            if isSome tl_ans andalso is_older(valOf tl_ans, hd dates)
            then tl_ans
            else SOME (hd dates)
        end

fun find(dates: (int*int*int) list, target: (int*int*int)) = 
    if null dates then false
    else if #1 target = #1 (hd dates) 
        andalso #2 target = #2 (hd dates) 
        andalso #3 target = #3 (hd dates) 
        then true
    else find(tl dates, target)

fun remove_duplicates(dates: (int*int*int) list) = 
    if null dates then []
    else 
        let val tl_ans = remove_duplicates(tl dates)
        in
            if find(tl_ans, hd dates) then tl_ans
            else (hd dates)::tl_ans
        end

fun number_in_months_challenge(dates : (int*int*int) list, target: int) =
    number_in_month(remove_duplicates(dates), target)