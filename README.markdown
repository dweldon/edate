Overview
--------
edate is an erlang date manipulation library

Installation
------------
    git clone git://github.com/dweldon/edate.git
    cd edate && make

Functions
---------
### today() -> Date
returns today's date.

### tomorrow() -> Date
returns tomorrow's date.

### yesterday() -> Date
returns yesterday's date.

### shift(N, Period) -> Date
returns a new date after shifting today's date by N periods. N is an integer,
Period is day | days | week | weeks | month | months | year | years

### shift(StartDate, N, Period) -> Date
returns a new date after shifting StartDate by N periods. N is an integer,
Period is day | days | week | weeks | month | months | year | years
    > edate:shift({2000,1,1}, -2, days)
    {1999,12,30}
    > edate:shift({2000,1,1}, 1, week)
    {2000,1,8}
    > edate:shift({2000,1,1}, 4, months)
    {2000,5,1}
    > edate:shift({2000,1,1}, 2, years)
    {2002,1,1}
