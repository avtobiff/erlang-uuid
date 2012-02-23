%% -----------------------------------------------------------------------------
%% Copyright © 2010-2012 Per Andersson
%%
%% Erlang UUID is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% Erlang UUID is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with Erlang UUID.  If not, see <http://www.gnu.org/licenses/>.
%% -----------------------------------------------------------------------------
%% @author Per Andersson <avtobiff@gmail.com>
%% @copyright 2010-2012 Per Andersson
%% @doc
%% Erlang UUID
%%
%% Include file for types, definitions, and macros.
%% @end
%% -----------------------------------------------------------------------------

%% The UUID types
-type uuid()::binary().
-type uuid_string()::string().
-type urn()::string().

%% Variant, corresponds to variant 1 0 of RFC 4122.
-define(VARIANT10, 2#10).

%% Version
-define(UUIDv1, 1).
-define(UUIDv3, 3).
-define(UUIDv4, 4).
-define(UUIDv5, 5).
