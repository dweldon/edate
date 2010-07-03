%% Copyright (c) 2010 David Weldon
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.

-module(edate).
-export([today/0,
         tomorrow/0,
         yesterday/0,
         shift/2,
         shift/3,
         subtract/2,
         is_in_future/1,
         is_in_past/1,
         beginning_of_month/1,
         end_of_month/1,
         date_to_string/1,
         string_to_date/1,
         day_of_week/1]).
-export([easter/1]).
-include_lib("eunit/include/eunit.hrl").

% @spec today() -> date()
today() -> date().

% @spec tomorrow() -> date()
tomorrow() -> shift(1, day).

% @spec yesterday() -> date()
yesterday() -> shift(-1, day).

% @spec shift(N::integer(), Period::period()) -> date()
%       period() = day | days | week | weeks | month | months | year | years
shift(N, Period) -> shift(date(), N, Period).

% @spec shift(Date::date(), N::integer(), Period::period()) -> date()
%       period() = day | days | week | weeks | month | months | year | years
shift(Date, N, Period) when Period =:= day; Period =:= days ->
    calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) + N);
shift(Date, N, Period) when Period =:= week; Period =:= weeks ->
    shift(Date, 7*N, days);
shift(Date, N, Period) when Period =:= year; Period =:= years ->
    shift(Date, 12*N, months);
shift({Y, M, D}, N, Period) when Period =:= month; Period =:= months ->
    % in order for the modular arithmetic to work, months in this function range
    % from 0 to 11 (January to December)
    TotalMonths = 12*Y + M-1 + N,
    Month = TotalMonths rem 12,
    Year = (TotalMonths - Month) div 12,
    % add one back to the month to fix our mod 12 shenanigans
    find_valid_date({Year, Month+1, D}).

% @spec find_valid_date(Date::date()) -> date()
% @doc returns Date if valid. otherwise, backward searches for a valid date.
find_valid_date(Date) ->
    case calendar:valid_date(Date) of
        true -> Date;
        false ->
            {Y, M, D} = Date,
            find_valid_date({Y, M, D-1})
    end.

% @spec subtract(Date1::date(), Date2::date()) -> integer()
subtract(Date1, Date2) ->
    calendar:date_to_gregorian_days(Date1) -
    calendar:date_to_gregorian_days(Date2).

% @spec is_in_future(Date) -> bool()
is_in_future(Date) -> subtract(Date, today()) > 0.

% @spec is_in_past(Date) -> bool()
is_in_past(Date) -> subtract(Date, today()) < 0.

% @spec beginning_of_month(Date::date()) -> date()
beginning_of_month({Y, M, _D}) -> {Y, M, 1}.

% @spec end_of_month(Date::date()) -> date()
end_of_month({Y, M, _D}) -> {Y, M, calendar:last_day_of_the_month(Y, M)}.

% @spec date_to_string(Date::date()) -> string()
date_to_string({Y, M, D}) ->
    true = calendar:valid_date({Y, M, D}),
    lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D])).

% @spec string_to_date(String::string()) -> date()
string_to_date(String) ->
    [Year, Month, Day] =
        case string:tokens(String, "-/") of
            [Y, M, D] when length(Y) =:= 4 -> [Y, M, D];
            [M, D, Y] when length(Y) =:= 4 -> [Y, M, D]
        end,
    Date = list_to_tuple([list_to_integer(X) || X <- [Year, Month, Day]]),
    true = calendar:valid_date(Date),
    Date.

% @spec day_of_week(Date) -> string()
day_of_week(Date) ->
    case calendar:day_of_the_week(Date) of
        1 -> "monday";
        2 -> "tuesday";
        3 -> "wednesday";
        4 -> "thursday";
        5 -> "friday";
        6 -> "saturday";
        7 -> "sunday"
    end.

% derived from http://www.gmarts.org/index.php?go=415#geteasterdatec
% converted by Evan Haas <evanhaas@gmail.com>
% @spec easter(Year::integer()) -> date()
% @doc returns Date of Easter (Roman Catholic) in the specified year
easter(Year) ->
    NCent = Year div 100,
    NRemain19 = Year rem 19,
    N1Tmp1 = fix(NCent, ((NCent - 15) div 2) + 202 - (11 * NRemain19)),
    N1Tmp2 = N1Tmp1 rem 30,
    N1 =
        case N1Tmp2 == 29 orelse (N1Tmp2 == 28 andalso NRemain19 > 10) of
            true -> N1Tmp2 - 1;
            false -> N1Tmp2
        end,
    DtPFM =
        case N1 > 10 of
            true -> {Year, 4, N1 - 10};
            false -> {Year, 3, N1 + 21}
        end,
    NWeekDay = calendar:day_of_the_week(DtPFM) rem 7,
    shift(DtPFM, 7 - NWeekDay, days).

fix(NCent, N1) when NCent >= 38 -> N1 - 2;
fix(NCent, N1) when NCent == 21 orelse NCent == 24 orelse NCent == 25 -> N1 - 1;
fix(NCent, N1) when NCent == 33 orelse NCent == 36 orelse NCent == 37 -> N1 - 2;
fix(NCent, N1) when NCent > 26 -> N1 - 1;
fix(_NCent, N1) -> N1.

shift_test_() ->
    [?_assertEqual(date(), shift(0, days)),
     ?_assertEqual(date(), shift(0, days)),
     ?_assertEqual(date(), shift(0, months)),
     ?_assertEqual(date(), shift(0, years)),
     % relative dates
     ?_assertEqual(tomorrow(), shift(1, day)),
     ?_assertEqual(yesterday(), shift(-1, day)),
     % simple day addition
     ?_assertEqual({2000,1,2}, shift({2000,1,1}, 1, day)),
     ?_assertEqual({2000,1,3}, shift({2000,1,1}, 2, days)),
     % simple week addition
     ?_assertEqual({2000,1,8}, shift({2000,1,1}, 1, week)),
     ?_assertEqual({2000,1,15}, shift({2000,1,1}, 2, weeks)),
     % simple month addition
     ?_assertEqual({2000,2,1}, shift({2000,1,1}, 1, month)),
     ?_assertEqual({2000,3,1}, shift({2000,1,1}, 2, months)),
     % simple year addition
     ?_assertEqual({2003,1,1}, shift({2000,1,1}, 3, years)),
     % simple year subtraction
     ?_assertEqual({1997,1,1}, shift({2000,1,1}, -3, years)),
     % day subtraction at year boundary
     ?_assertEqual({1999,12,31}, shift({2000,1,1}, -1, day)),
     ?_assertEqual({1999,12,30}, shift({2000,1,1}, -2, days)),
     % week subtraction at year boundary
     ?_assertEqual({1999,12,25}, shift({2000,1,1}, -1, week)),
     ?_assertEqual({1999,12,18}, shift({2000,1,1}, -2, weeks)),
     % month subtraction at year boundary
     ?_assertEqual({1999,12,1}, shift({2000,1,1}, -1, month)),
     ?_assertEqual({1999,11,1}, shift({2000,1,1}, -2, months)),
     % 1 year = 12 months = 365 days (in a non-leap year)
     ?_assertEqual(shift({2001,5,10}, 1, year), shift({2001,5,10}, 12, months)),
     ?_assertEqual(shift({2001,5,10}, 1, year), shift({2001,5,10}, 365, days)),
     % date rounding from month addition and subtraction
     ?_assertEqual({2001,2,28}, shift({2001,1,31}, 1, month)),
     ?_assertEqual({2001,2,28}, shift({2001,3,31}, -1, month)),
     % leap year
     ?_assertEqual({2012,2,29}, shift({2012,1,31}, 1, month)),
     ?_assertEqual({2012,2,29}, shift({2012,4,30}, -2, months)),
     ?_assertEqual({2013,2,28}, shift({2012,2,29}, 1, year))].

subtract_test_() ->
    [?_assertEqual(3, subtract({2010,7,4}, {2010,7,1})),
     ?_assertEqual(-3, subtract({2010,7,1}, {2010,7,4})),
     ?_assertEqual(1, subtract(today(), yesterday())),
     ?_assertEqual(0, subtract(today(), today()))].

is_in_future_test_() ->
    [?_assertEqual(true, is_in_future(tomorrow())),
     ?_assertEqual(false, is_in_future(yesterday())),
     ?_assertEqual(false, is_in_future(today()))].

is_in_past_test_() ->
    [?_assertEqual(false, is_in_past(tomorrow())),
     ?_assertEqual(true, is_in_past(yesterday())),
     ?_assertEqual(false, is_in_past(today()))].

beginning_of_month_test_() ->
    [?_assertEqual({2012,2,1}, beginning_of_month({2012,2,15})),
     ?_assertEqual({2012,2,1}, beginning_of_month({2012,2,1}))].

end_of_month_test_() ->
    [?_assertEqual({2012,2,29}, end_of_month({2012,2,29})),
     ?_assertEqual({2012,2,29}, end_of_month({2012,2,1})),
     ?_assertEqual({2010,2,28}, end_of_month({2010,2,28})),
     ?_assertEqual({2010,2,28}, end_of_month({2010,2,1}))].

date_to_string_test_() ->
    [?_assertEqual("1976-12-20", date_to_string({1976,12,20})),
     ?_assertEqual("1976-03-18", date_to_string({1976,3,18})),
     ?_assertEqual("2010-01-02", date_to_string({2010,1,2}))].

string_to_date_test_() ->
    [?_assertEqual({2010,1,2}, string_to_date("2010-01-02")),
     ?_assertEqual({2010,1,2}, string_to_date("2010-1-2")),
     ?_assertEqual({2010,1,2}, string_to_date("2010/01/02")),
     ?_assertEqual({2010,1,2}, string_to_date("2010/1/2")),
     ?_assertEqual({2010,1,2}, string_to_date("01-02-2010")),
     ?_assertEqual({2010,1,2}, string_to_date("1-2-2010")),
     ?_assertEqual({2010,1,2}, string_to_date("01/02/2010")),
     ?_assertEqual({2010,1,2}, string_to_date("1/2/2010"))].

day_of_week_test_() ->
    [?_assertEqual("monday", day_of_week({2010,6,28})),
     ?_assertEqual("tuesday", day_of_week({2010,6,29})),
     ?_assertEqual("wednesday", day_of_week({2010,6,30})),
     ?_assertEqual("thursday", day_of_week({2010,7,1})),
     ?_assertEqual("friday", day_of_week({2010,7,2})),
     ?_assertEqual("saturday", day_of_week({2010,7,3})),
     ?_assertEqual("sunday", day_of_week({2010,7,4}))].

easter_test_() ->
    [?_assertEqual({2008,3,23}, easter(2008)),
     ?_assertEqual({2009,4,12}, easter(2009)),
     ?_assertEqual({2010,4,4}, easter(2010)),
     ?_assertEqual({2011,4,24}, easter(2011)),
     ?_assertEqual({2012,4,8}, easter(2012)),
     ?_assertEqual({2013,3,31}, easter(2013)),
     ?_assertEqual({2014,4,20}, easter(2014))].
