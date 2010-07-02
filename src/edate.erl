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
-export([today/0, tomorrow/0, yesterday/0]).
-include_lib("eunit/include/eunit.hrl").


% @spec today() -> {Year, Month, Day}
today() -> date().

% @spec tomorrow() -> {Year, Month, Day}
tomorrow() -> shift(date(), 1, day).

% @spec yesterday() -> {Year, Month, Day}
yesterday() -> shift(date(), -1, day).

% @spec shift(Date::date(), N::integer(), Type::type()) -> {Year, Month, Day}
%       type() = day | days | week | weeks | month | months | year | years
%       date() = {Year, Month, Day}
shift(Date, N, days) ->
    shift(Date, N, day);
shift(Date, N, day) ->
    calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) + N).
