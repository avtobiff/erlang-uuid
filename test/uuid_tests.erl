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
%% HERE BE UUID TESTS
%% @end
%% -----------------------------------------------------------------------------

-module(uuid_tests).
-author('Per Andersson').

-include_lib("eunit/include/eunit.hrl").

-include("../src/uuid.hrl").


uuid_binary_test() ->
    Uuid = uuid:uuid4(),

    %% UUID v1
    NodeId = uuid:get_node(),
    ?assertMatch(<<_U0:48, ?UUIDv1:4, _U1:12, ?VARIANT:2, _U2:14,
                   NodeId/binary>>,
                 uuid:uuid1()),

    NilNode = <<0:48>>,
    ClockSeq = <<ClockSeqHi:6, ClockSeqLow:8>> = <<0:14>>,
    ?assertMatch(<<_U0:48, ?UUIDv1:4, _U1:12, ?VARIANT:2,
                   ClockSeqLow:8, ClockSeqHi:6, NilNode/binary>>,
                 uuid:uuid1(NilNode, ClockSeq)),

    %% UUID v3
    ?assertMatch(<<_U0:48, ?UUIDv3:4, _U1:12, ?VARIANT:2, _U2:62>>,
                 uuid:uuid3(dns, "fqdn.example.com")),
    ?assertEqual(uuid:uuid3(dns, "fqdn.example.com"),
                 uuid:uuid3(dns, "fqdn.example.com")),

    ?assertMatch(<<_U0:48, ?UUIDv3:4, _U1:12, ?VARIANT:2, _U2:62>>,
                 uuid:uuid3(oid, "2.5.6")),
    ?assertEqual(uuid:uuid3(oid, "2.5.6"), uuid:uuid3(oid, "2.5.6")),

    ?assertMatch(<<_U0:48, ?UUIDv3:4, _U1:12, ?VARIANT:2, _U2:62>>,
                 uuid:uuid3(url, "http://fqdn.example.com/path")),
    ?assertEqual(uuid:uuid3(url, "http://fqdn.example.com/path"),
                 uuid:uuid3(url, "http://fqdn.example.com/path")),

    ?assertMatch(<<_U0:48, ?UUIDv3:4, _U1:12, ?VARIANT:2, _U2:62>>,
                 uuid:uuid3(x500, "cn=John Doe, o=Acme, Inc., c=US")),
    ?assertEqual(uuid:uuid3(x500, "cn=John Doe, o=Acme, Inc., c=US"),
                 uuid:uuid3(x500, "cn=John Doe, o=Acme, Inc., c=US")),

    ?assertMatch(<<_U0:48, ?UUIDv3:4, _U1:12, ?VARIANT:2, _U2:62>>,
                 uuid:uuid3(nil, "my own unique name")),
    ?assertEqual(uuid:uuid3(nil, "my own unique name"),
                 uuid:uuid3(nil, "my own unique name")),

    ?assertMatch(<<_U0:48, ?UUIDv3:4, _U1:12, ?VARIANT:2, _U2:62>>,
                 uuid:uuid3(Uuid, "fqdn.example.com")),
    ?assertEqual(uuid:uuid3(Uuid, "fqdn.example.com"),
                 uuid:uuid3(Uuid, "fqdn.example.com")),

    ?assertMatch(<<_U0:48, ?UUIDv3:4, _U1:12, ?VARIANT:2, _U2:62>>,
                 uuid:uuid3(uuid:to_string(Uuid), "fqdn.example.com")),
    ?assertEqual(uuid:uuid3(uuid:to_string(Uuid), "fqdn.example.com"),
                 uuid:uuid3(uuid:to_string(Uuid), "fqdn.example.com")),

    %% UUID v4
    ?assertMatch(<<_U0:48, ?UUIDv4:4, _U1:12, ?VARIANT:2, _U2:62>>,
                 uuid:uuid4()),

    %% UUID v5
    ?assertMatch(<<_U0:48, ?UUIDv5:4, _U1:12, ?VARIANT:2, _U2:62>>,
                 uuid:uuid5(dns, "fqdn.example.com")),
    ?assertEqual(uuid:uuid5(dns, "fqdn.example.com"),
                 uuid:uuid5(dns, "fqdn.example.com")),

    ?assertMatch(<<_U0:48, ?UUIDv5:4, _U1:12, ?VARIANT:2, _U2:62>>,
                 uuid:uuid5(oid, "2.5.6")),
    ?assertEqual(uuid:uuid5(oid, "2.5.6"), uuid:uuid5(oid, "2.5.6")),

    ?assertMatch(<<_U0:48, ?UUIDv5:4, _U1:12, ?VARIANT:2, _U2:62>>,
                 uuid:uuid5(url, "http://fqdn.example.com/path")),
    ?assertEqual(uuid:uuid5(url, "http://fqdn.example.com/path"),
                 uuid:uuid5(url, "http://fqdn.example.com/path")),

    ?assertMatch(<<_U0:48, ?UUIDv5:4, _U1:12, ?VARIANT:2, _U2:62>>,
                 uuid:uuid5(x500, "cn=John Doe, o=Acme, Inc., c=US")),
    ?assertEqual(uuid:uuid5(x500, "cn=John Doe, o=Acme, Inc., c=US"),
                 uuid:uuid5(x500, "cn=John Doe, o=Acme, Inc., c=US")),

    ?assertMatch(<<_U0:48, ?UUIDv5:4, _U1:12, ?VARIANT:2, _U2:62>>,
                 uuid:uuid5(nil, "my own unique name")),
    ?assertEqual(uuid:uuid5(nil, "my own unique name"),
                 uuid:uuid5(nil, "my own unique name")),

    ?assertMatch(<<_U0:48, ?UUIDv5:4, _U1:12, ?VARIANT:2, _U2:62>>,
                 uuid:uuid5(Uuid, "fqdn.example.com")),
    ?assertEqual(uuid:uuid5(Uuid, "fqdn.example.com"),
                 uuid:uuid5(Uuid, "fqdn.example.com")),

    ?assertMatch(<<_U0:48, ?UUIDv5:4, _U1:12, ?VARIANT:2, _U2:62>>,
                 uuid:uuid5(uuid:to_string(Uuid), "fqdn.example.com")),
    ?assertEqual(uuid:uuid5(uuid:to_string(Uuid), "fqdn.example.com"),
                 uuid:uuid5(uuid:to_string(Uuid), "fqdn.example.com")).

representation_test() ->
    Uuid1 = uuid:uuid1(),
    Uuid3 = uuid:uuid5(dns, "fqdn.example.com"),
    Uuid4 = uuid:uuid4(),
    Uuid5 = uuid:uuid5(dns, "fqdn.example.com"),
    ?assertMatch(Uuid1, uuid:to_binary(uuid:to_string(Uuid1))),
    ?assertMatch(Uuid3, uuid:to_binary(uuid:to_string(Uuid3))),
    ?assertMatch(Uuid4, uuid:to_binary(uuid:to_string(Uuid4))),
    ?assertMatch(Uuid5, uuid:to_binary(uuid:to_string(Uuid5))).

tostring_test() ->
    SimpleUuid = "8fd7fa874c205809a1b0e07f5c224f02",
    PrettyUuid = "8fd7fa87-4c20-5809-a1b0-e07f5c224f02",
    ?assertMatch(PrettyUuid,
                 uuid:to_string(uuid:uuid5(dns, "fqdn.example.com"))),
    ?assertMatch(PrettyUuid,
                 uuid:to_string(pretty, uuid:uuid5(dns, "fqdn.example.com"))),
    ?assertMatch(SimpleUuid,
                 uuid:to_string(simple, uuid:uuid5(dns, "fqdn.example.com"))).

exceptions_test() ->
    ?assertMatch(ok, try_badarg(to_binary, [0])),
    ?assertMatch(ok, try_badarg(to_string, [0])),
    ?assertMatch(ok, try_badarg(to_string, [0, 0])),
    ?assertMatch(ok, try_badarg(uuid3,     [0, 0])),
    ?assertMatch(ok, try_badarg(uuid5,     [0, 0])).

urn_test() ->
    UuidBin = uuid:uuid4(),
    UrnBin  = uuid:to_uuid_urn(UuidBin),
    UuidStr = uuid:to_string(UuidBin),
    UrnStr  = uuid:to_uuid_urn(UuidStr),

    [$u,$r,$n,$:,$u,$u,$i,$d,$:|UrnUuidBinStr] = UrnBin,
    ?assertMatch(UrnUuidBinStr, uuid:to_string(UuidBin)),

    [$u,$r,$n,$:,$u,$u,$i,$d,$:|UrnUuidStr] = UrnStr,
    ?assertMatch(UrnUuidStr, UuidStr),

    ?assertMatch(UrnUuidBinStr, UrnUuidStr),

    ?assertMatch(UrnBin, uuid:to_uuid_urn(UrnBin)).


%% helper functions
try_badarg(F, A) ->
    try
        apply(uuid, F, A)
    catch error:badarg ->
        ok
    end.
