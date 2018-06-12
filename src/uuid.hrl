%% -----------------------------------------------------------------------------
%% Copyright © 2010-2018 Per Andersson
%%
%% Erlang UUID is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation, either version 3 of the
%% License, or (at your option) any later version.
%%
%% Erlang UUID is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with Erlang UUID.  If not, see
%% <http://www.gnu.org/licenses/>.
%% -----------------------------------------------------------------------------
%% @author Per Andersson <avtobiff@gmail.com>
%% @copyright 2010-2018 Per Andersson
%%            2012 Bip Thelin
%%            2013 Ryan Flynn
%%            2013 Iivari Äikäs
%%            2013 William Cummings
%%            2014 Gianni Gambetti
%%            2015 Peter Hizalev
%%            2017 Jean Rouge
%% @doc
%% Erlang UUID
%%
%% Include file for types, definitions, and macros.
%% @end
%% -----------------------------------------------------------------------------


%% Variant, corresponds to variant 1 0 of RFC 4122.
-define(VARIANT10, 2#10).


%% For 100 nanosecond interval transformation in UUIDv1.

%% Offset between 15 October 1582 and 1 January 1970
-define(nanosecond_intervals_offset, 122192928000000000).

%% microseconds to nanoseconds
-define(nanosecond_intervals_factor, 10).


%% Version
-define(UUIDv1, 1).
-define(UUIDv3, 3).
-define(UUIDv4, 4).
-define(UUIDv5, 5).
