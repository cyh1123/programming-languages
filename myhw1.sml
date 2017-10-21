
(*1*)
fun is_older (date1 : int*int*int, date2 : int*int*int) =
  if #1 date1 < #1 date2
  then true
  else if #1 date1 = #1 date2 andalso #2 date1 < #2 date2
  then true
  else if #2 date1 = #2 date2 andalso #3 date1 < #3 date2
  then true
  else false

(*2*)
fun number_in_month (dates : (int*int*int) list, month : int) =
(*  let val i = 0
  in let fun count (dates:(int*int*int) list, month:int, i) =
           if null dates
           then i
           else if #2 (hd dates) = month
           then count (tl dates, month, i+1)
           else count (tl dates, month, i)
     in count (dates, month, i)
     end
  end *)
  if null dates
  then 0
  else if #2 (hd dates) = month
  then 1 + number_in_month (tl dates, month)
  else number_in_month (tl dates, month)
                       


(*3*)  
fun number_in_months (dates:(int*int*int) list, months:int list) =
  if null months
  then 0
  else number_in_month (dates, hd months) + number_in_months (dates, tl months)

                                                             
(*4*)
fun dates_in_month (dates:(int*int*int) list, month: int)=
  if null dates
  then []
  else if #2 (hd dates) = month
  then hd dates :: dates_in_month (tl dates, month)
  else dates_in_month (tl dates, month)


(*5*)
fun append (x:(int*int*int) list, y:(int*int*int) list)=
  if null x
  then y
  else hd x :: append(tl x, y)
                     
fun dates_in_months (dates:(int*int*int) list, months:int list)=
  if null months
  then []
  else append(dates_in_month (dates, hd months), dates_in_months (dates, tl months))
                                                                         

(*6*)
fun get_nth (s:string list, n:int)=
  if n = 1
  then hd s
  else get_nth (tl s, n-1)

(*7*)
fun date_to_string (date:int*int*int)=
  let val months = ["January", "February", "March", "April","May", "June",
                    "July", "August", "September", "October", "November", "December"]
      val n = #2 date
      val fir = get_nth(months, n)
      val sec = Int.toString(#3 date)
      val thi = Int.toString(#1 date)
  in fir^" "^sec^", "^thi
  end


(*8*)
fun number_before_reaching_sum (sum:int, num_list:int list)=
  if sum <= hd num_list
  then 0
  else 1 + number_before_reaching_sum(sum - (hd num_list), tl num_list)
(*if add first N numbers of the num_list, it may have to creat a variable N to count the numbers*)


(*9*)
fun what_month (day:int)=
  let val days_of_common_year = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in number_before_reaching_sum(day, days_of_common_year) + 1
  end


(*10*)
fun month_range (day1:int, day2:int)=
  if day1 > day2
  then []
  else what_month (day1) :: month_range (day1 + 1, day2)

(*11*)
fun min_date (dates:(int*int*int) list)=
  if null (tl dates)
  then hd dates
  else if is_older(hd (tl dates), hd dates)
  then min_date(tl dates)
  else min_date(hd dates :: tl (tl dates))

fun oldest (dates:(int*int*int) list)=
  if null dates
  then NONE
  else SOME (min_date(dates))


(*12*)
fun not_in (num:int, nums:int list)=
  if null nums
  then true
  else if num = hd nums
  then false
  else not_in (num, tl nums)
              
fun set_of (months:int list)=
  if null months
  then []
  else if not_in(hd months, tl months)
  then hd months :: set_of(tl months)
  else set_of (tl months)
              
fun number_in_months_challenge (dates:(int*int*int) list, months:int list) =
  number_in_months(dates, set_of(months))

fun dates_in_months_challenge (dates:(int*int*int) list, months:int list) =
  dates_in_months(dates, set_of(months))




            
(*13*)
fun leap_year (year:int)=
  if year < 1
  then false
  else if year mod 400 = 0
  then true
  else if year mod 4 = 0 andalso year mod 100 <> 0
  then true
  else false
           
fun days_of_nth_month (days:int list, n:int)=
  if n = 1
  then hd days
  else days_of_nth_month (tl days, n-1)
               
fun reasonable_date (date:int*int*int)=
  let val year = #1 date
      val month = #2 date
      val day = #3 date
      val days_of_leap_years = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
      val days_of_common_years = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in if year < 1 orelse month < 1 orelse month > 12 orelse day < 1
     then false
     else if leap_year(year)
     then if day <= days_of_nth_month(days_of_leap_years, month)
          then true
          else false
     else if day <= days_of_nth_month(days_of_common_years, month)
     then true
     else false
  end
      

