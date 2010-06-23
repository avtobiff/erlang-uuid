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
%% Currently implements UUID v4, UUID generated with (pseudo) random number
%% generator.
%%
%% Example usage
%% <pre>
%%     1> uuid:to_string(uuid:uuid4()).
%%     "79f492f8-1337-4200-abcd-92bada1cacao"
%% </pre>
%% @end
%% -----------------------------------------------------------------------------

-module(uuid).
-author('Per Andersson').

-export([uuid4/0, to_string/1]).



%% @doc  Create a UUID v4 (random) as a binary
%% @spec () -> binary()
uuid4() ->
    {A1, A2, A3} = now(),
    random:seed(A1, A2, A3),

    U0 = random:uniform(round(math:pow(2,48) - 1)),
    U1 = random:uniform(round(math:pow(2,12) - 1)),
    U2 = random:uniform(round(math:pow(2,60) - 1)),

    uuid4(U0, U1, U2).


%% @private
%% @doc  Create a 128 bit binary (UUID v4) from input
%% @spec (U0, U1, U2) -> binary()
%% where U0 = U1 = U2 = integer()
uuid4(U0, U1, U2) -> <<U0:48, 4:4, U1:12, 10:4, U2:60>>.


%% @doc  Format uuid string from binary
%% @spec (Uuid::binary()) -> string()
to_string(<<U0:32, U1:16, U2:16, U3:16, U4:48>>) ->
    lists:flatten(io_lib:format(
        "~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
        [U0, U1, U2, U3, U4])).
