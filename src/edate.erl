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
         easter/1]).
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

fix(NCent, N1) when NCent >= 38 -> N1 - 2;
fix(NCent, N1) when NCent == 21 orelse NCent == 24 orelse NCent == 25 -> N1 - 1;
fix(NCent, N1) when NCent == 33 orelse NCent == 36 orelse NCent == 37 -> N1 - 2;
fix(NCent, N1) when NCent > 26 -> N1 - 1;
fix(_NCent, N1) -> N1. 

% Derived from http://www.gmarts.org/index.php?go=415#geteasterdatec
% Converted by Evan Haas <evanhaas@gmail.com>
% @spec easter(Year::integer()) -> date()
% @doc returns Date of Easter (Roman Catholic) in the specified year
easter(Year) ->
  NCent = Year div 100,
  NRemain19 = Year rem 19,
  N1Tmp1 = fix(NCent, ((NCent - 15) div 2) + 202 - (11 * NRemain19)),
  N1Tmp2 = N1Tmp1 rem 30,

  N1 = case N1Tmp2 == 29 orelse (N1Tmp2 == 28 andalso NRemain19 > 10) of
        true -> N1Tmp2 - 1;
        false -> N1Tmp2
       end,

  DtPFM = case N1 > 10 of
            true -> {Year, 4, N1 - 10};
            false -> {Year, 3, N1 + 21}
          end,

  NWeekDay = calendar:day_of_the_week(DtPFM) rem 7,
  calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(DtPFM) + (7 - NWeekDay)).
      
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
