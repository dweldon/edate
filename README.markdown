Overview
--------
edate is an erlang date manipulation library

Installation
------------
    git clone git://github.com/dweldon/edate.git
    cd edate && make

Functions
---------
* today/0
* tomorrow/0
* yesterday/0
* shift/2
* shift/3
* subtract/2
* beginning_of_month/1
* end_of_month/1
* date_to_string/1
* string_to_date/1
* day_of_week/1

Function Descriptions
---------------------
### today() -> Date
returns today's date.

### tomorrow() -> Date
returns tomorrow's date.

### yesterday() -> Date
returns yesterday's date.

### shift(N, Period) -> Date
returns a new date after shifting today's date by N periods. N is an integer,
Period is `day | days | week | weeks | month | months | year | years`

### shift(StartDate, N, Period) -> Date
returns a new date after shifting StartDate by N periods. N is an integer,
Period is `day | days | week | weeks | month | months | year | years`
    > edate:shift({2000,1,1}, -2, days).
    {1999,12,30}
    > edate:shift({2000,1,1}, 1, week).
    {2000,1,8}
    > edate:shift({2000,1,1}, 4, months).
    {2000,5,1}
    > edate:shift({2000,1,1}, 2, years).
    {2002,1,1}

### subtract(Date1, Date2) -> Days
returns Date1 - Date2 in an integer number of days. if Days > 0, Date1 > Date2.
    > edate:subtract({2010,7,4}, {2010,7,1}).
    3
    > edate:subtract({2010,7,1}, {2010,7,4}).
    -3

### beginning_of_month(StartDate) -> Date
returns a new date representing the beginning of the month containing StartDate.
    > edate:beginning_of_month({2010,2,15}).
    {2010,2,1}
    > edate:beginning_of_month(edate:shift({2010,7,2}, -1, month)).
    {2010,6,1}

### end_of_month(StartDate) -> Date
returns a new date representing the end of the month containing StartDate.
    > edate:end_of_month({2010,2,15}).
    {2010,2,28}
    > edate:end_of_month(edate:shift({2010,7,2}, -1, month)).
    {2010,6,30}

### date_to_string(Date) -> String
returns an [ISO 8601](http://en.wikipedia.org/wiki/Iso8601) representation of Date.
    > edate:date_to_string({1976,3,18}).
    "1976-03-18"

### string_to_date(String) -> Date
returns a date representation of String. the following formats are valid
(with or without zero-padding): YYYY-MM-DD, YYYY/MM/DD, MM-DD-YYYY, MM/DD/YYYY.
    > edate:string_to_date("1976-03-18").
    {1976,3,18}
    > edate:string_to_date("3/18/1976").
    {1976,3,18}

### day_of_week(Date) -> String
returns the day of the week as a string ("monday".."sunday").
    > edate:day_of_week({2010,7,4}).
    "sunday"
