Overview
--------
edate is a date manipulation library for erlang

Installation
------------
    git clone git://github.com/dweldon/edate.git
    cd edate && make

Interface
---------
The following examples give an overview of the edate interface. Please see the
complete documentation by running `make doc`.

### String Conversion
    > edate:date_to_string({1976,3,18}).
    "1976-03-18"

    > edate:string_to_date("3/18/1976").
    {1976,3,18}

### Date Math
    > edate:beginning_of_month({2010,2,15}).
    {2010,2,1}

    > edate:end_of_month({2010,2,15}).
    {2010,2,28}

    > edate:shift({2010,2,27}, 1, week).
    {2010,3,6}

    > edate:subtract(edate:tomorrow(), edate:yesterday()).
    2

### Comparison
    > edate:is_after({1950,7,2}, {1950,7,1}).
    true

    > edate:is_before({1950,7,1}, {1950,7,2}).
    true

    > edate:is_in_future({3000,7,1}).
    true

    > edate:is_in_past({1950,7,1}).
    true

### Miscellaneous
    > edate:day_of_week({2010,7,4}).
    "sunday"
