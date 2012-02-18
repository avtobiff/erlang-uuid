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
%% along with erlang-uuid.  If not, see <http://www.gnu.org/licenses/>.
%% -----------------------------------------------------------------------------
%% @author Per Andersson <avtobiff@gmail.com>
%% @copyright 2010-2012 Per Andersson
%% @doc
%% Erlang UUID
%%
%% Currently implements UUID v4 and v5 as of RFC 4122.
%%
%% Example usage
%% <pre>
%%     1> uuid:to_string(uuid:uuid4()).
%%     "79f492f8-1337-4200-abcd-92bada1cacao"
%%     2> uuid:to_string(uuid:uuid5(dns, "fqdn.example.com")).
%%     "8fd7fa87-4c20-5809-a1b0-e07f5c224f02"
%% </pre>
%% @end
%% -----------------------------------------------------------------------------

-module(uuid).
-author('Per Andersson').

-export([uuid4/0, uuid5/2, to_string/1, to_string/2, to_binary/1]).



%% @doc  Create a UUID v4 (random) as a binary
-spec uuid4() -> binary().
uuid4() ->
    {A1, A2, A3} = now(),
    random:seed(A1, A2, A3),

    U0 = random:uniform((2 bsl 48) - 1),
    U1 = random:uniform((2 bsl 12) - 1),
    U2 = random:uniform((2 bsl 60) - 1),

    uuid4(U0, U1, U2).


%% @doc  Create a UUID v5 (name based) as a binary.
%%       Magic numbers are from Appendix C of the RFC 4122.
-spec uuid5(NamespaceOrUuid::atom() | string() | binary(),
            Name::string()) -> binary().
uuid5(dns, Name) ->
    uuid5(list_to_binary([<<16#6ba7b8109dad11d180b400c04fd430c8:128>>, Name]));
uuid5(url, Name) ->
    uuid5(list_to_binary([<<16#6ba7b8119dad11d180b400c04fd430c8:128>>, Name]));
uuid5(oid, Name) ->
    uuid5(list_to_binary([<<16#6ba7b8129dad11d180b400c04fd430c8:128>>, Name]));
uuid5(x500, Name) ->
    uuid5(list_to_binary([<<16#6ba7b8149dad11d180b400c04fd430c8:128>>, Name]));
uuid5(nil, Name) ->
    uuid5(list_to_binary([<<0:128>>, Name]));
uuid5(UuidStr, Name) when is_list(UuidStr) ->
    uuid5(list_to_binary([to_binary(UuidStr), Name]));
uuid5(UuidBin, Name) when is_binary(UuidBin) ->
    uuid5(list_to_binary([UuidBin, Name]));
uuid5(_, _) ->
    erlang:error(badarg).


%% @private
%% @doc  Create a UUID v5 (name based) from binary
-spec uuid5(Data::binary()) -> binary().
uuid5(Data) ->
    <<Sha1:128, _:32>> = crypto:sha(Data),

    <<TimeLow:32, TimeMid:16, _AndVersion:4, TimeHi:12,
      _AndReserved:2, ClockSeqHi:6, ClockSeqLow:8, Node:48>> = <<Sha1:128>>,

    Version = 5,
    Variant = 2#10,

    <<TimeLow:32, TimeMid:16, Version:4, TimeHi:12,
      Variant:2, ClockSeqHi:6, ClockSeqLow:8, Node:48>>.


%% @doc  Create a 128 bit binary (UUID v4) from input
-spec uuid4(U0::integer(), U1::integer(), U2::integer()) -> binary().
uuid4(U0, U1, U2) -> <<U0:48, 4:4, U1:12, 10:4, U2:60>>.


%% @doc  Format UUID string from binary
-spec to_string(Uuid::binary()) -> string().
to_string(Uuid) when is_binary(Uuid) ->
    to_string(pretty, Uuid);
to_string(_) ->
    erlang:error(badarg).

-spec to_string(simple | pretty, Uuid::binary()) -> string().
to_string(pretty, <<U0:32, U1:16, U2:16, U3:16, U4:48>>) ->
    lists:flatten(io_lib:format(
        "~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
        [U0, U1, U2, U3, U4]));
to_string(simple, <<S:128>>) ->
    lists:flatten(io_lib:format("~32.16.0b", [S]));
to_string(_, _) ->
    erlang:error(badarg).


%% @doc  Format uuid binary from string
-spec to_binary(UuidStr::string()) -> binary().
to_binary(UuidStr) when is_list(UuidStr) ->
    Parts = string:tokens(UuidStr, "$-"),
    [I0, I1, I2, I3, I4] = [hex_to_int(Part) || Part <- Parts],
    <<I0:32, I1:16, I2:16, I3:16, I4:48>>;
to_binary(_) ->
    erlang:error(badarg).


%% @private
%% @doc  Convert from hexadecimal digit represented as string to decimal.
-spec hex_to_int(Hex::string()) -> integer().
hex_to_int(Hex) ->
    {ok, [D], []} = io_lib:fread("~16u", Hex),
    D.
