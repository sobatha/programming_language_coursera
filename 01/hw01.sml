fun is_older (date1: (int*int*int),date2: (int*int*int)) = 
    (#1 date1 < #1 date2) orelse
    (#1 date1 = #1 date2 andalso (#2 date1 < #2 date2)) orelse
    (#1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 < #3 date2)

fun get_month (date: (int*int*int)) = #2 date

fun number_in_month (dates : (int*int*int) list, target: int) = 
    if null dates then 0
    else if #2 (hd dates) = target then 1 + number_in_month(tl dates, target)
    else number_in_month(tl dates, target)

fun number_in_months (dates : (int*int*int) list, targets: int list) = 
    if null targets then 0
    else if number_in_month(dates, hd targets) + number_in_months(dates, tl targets)

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
    if num = 1 then hd lis
    else get_nth(tl lis, num - 1)

val month_name = ["January", "February", "March", "Spring", "May", "June", "July", "August", 
                "September", "October", "November", "December"]

fun date_to_string (year : int, month : int, day: int) = 
    get_nth(month_name, month) ^ " " ^ Int.toString day ^ "," ^ " " ^ Int.toString year

fun number_before_reaching_sum (target : int, lis: int list) = 
    if hd lis >= target then 0
    else 1 + number_before_reaching_sum(target - (hd lis), tl lis)

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