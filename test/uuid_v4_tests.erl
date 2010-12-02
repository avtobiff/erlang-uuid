%% -----------------------------------------------------------------------------
%% Copyright @ 2010 Per Andersson
%%
%% erlang-uuid is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% erlang-uuid is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with erlang-uuid.  If not, see <http://www.gnu.org/licenses/>.
%% -----------------------------------------------------------------------------
%% @author Per Andersson <avtobiff@gmail.com>
%% @copyright 2010 Per Andersson
%% @doc
%% Erlang UUID generator
%%
%% HERE BE UUIDv4 TESTS
%% @end
%% -----------------------------------------------------------------------------

-module(uuid_v4_tests).
-author('Per Andersson').

-include_lib("eunit/include/eunit.hrl").


uuid_v4_binary_test() ->
   ?assertMatch(<<_U0:48, 4:4, _U1:12, 10:4, _U2:60>>, uuid:uuid4()).

representation_test() ->
    Uuid = uuid:uuid4(),
    ?assertMatch(Uuid, uuid:to_binary(uuid:to_string(Uuid))).

exceptions_test() ->
    ?assertMatch(ok, try_badarg(to_binary, 0)),
    ?assertMatch(ok, try_badarg(to_string, 0)).


try_badarg(F, A) ->
    try
        uuid:F(A)
    catch error:badarg ->
        ok
    end.
