Overview
--------
edate is an erlang date manipulation library

Installation
------------
    git clone git://github.com/dweldon/edate.git
    cd edate && make

Functions
---------
### today
returns the current date as {Year, Month, Day}.
    > edate:today().
    {2010,7,1}

### tomorrow
returns tomorrow's date as {Year, Month, Day}.
    > edate:tomorrow().
    {2010,7,2}

### yesterday()
returns yesterday's date as {Year, Month, Day}.
    > edate:yesterday().
    {2010,6,30}
