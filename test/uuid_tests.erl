%% -----------------------------------------------------------------------------
%% Copyright © 2010 Per Andersson
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
%% @copyright 2010 Per Andersson
%% @doc
%% Erlang UUID
%%
%% HERE BE UUID TESTS
%% @end
%% -----------------------------------------------------------------------------

-module(uuid_tests).
-author('Per Andersson').

-include_lib("eunit/include/eunit.hrl").


uuid_binary_test() ->
    Uuid = uuid:uuid4(),
   ?assertMatch(<<_U0:48, 4:4, _U1:12, 10:4, _U2:60>>, uuid:uuid4()),
   ?assertMatch(<<_U0:48, 5:4, _U1:12, _:2, _U2:62>>, uuid:uuid5(dns, "fqdn.example.com")),
   ?assertEqual(uuid:uuid5(dns, "fqdn.example.com"), uuid:uuid5(dns, "fqdn.example.com")),
   ?assertMatch(<<_U0:48, 5:4, _U1:12, _:2, _U2:62>>, uuid:uuid5(oid, "2.5.6")),
   ?assertEqual(uuid:uuid5(oid, "2.5.6"), uuid:uuid5(oid, "2.5.6")),
   ?assertMatch(<<_U0:48, 5:4, _U1:12, _:2, _U2:62>>,
                    uuid:uuid5(url, "http://fqdn.example.com/path")),
   ?assertEqual(uuid:uuid5(url, "http://fqdn.example.com/path"),
                    uuid:uuid5(url, "http://fqdn.example.com/path")),
   ?assertMatch(<<_U0:48, 5:4, _U1:12, _:2, _U2:62>>,
                    uuid:uuid5(x500, "cn=John Doe, o=Acme, Inc., c=US")),
   ?assertEqual(uuid:uuid5(x500, "cn=John Doe, o=Acme, Inc., c=US"),
                    uuid:uuid5(x500, "cn=John Doe, o=Acme, Inc., c=US")),
   ?assertMatch(<<_U0:48, 5:4, _U1:12, _:2, _U2:62>>, uuid:uuid5(nil, "my own unique name")),
   ?assertEqual(uuid:uuid5(nil, "my own unique name"),
                    uuid:uuid5(nil, "my own unique name")),
   ?assertMatch(<<_U0:48, 5:4, _U1:12, _:2, _U2:62>>, uuid:uuid5(Uuid, "fqdn.example.com")),
   ?assertEqual(uuid:uuid5(Uuid, "fqdn.example.com"),
                    uuid:uuid5(Uuid, "fqdn.example.com")),
   ?assertMatch(<<_U0:48, 5:4, _U1:12, _:2, _U2:62>>,
                    uuid:uuid5(uuid:to_string(Uuid), "fqdn.example.com")),
   ?assertEqual(uuid:uuid5(uuid:to_string(Uuid), "fqdn.example.com"),
                    uuid:uuid5(uuid:to_string(Uuid), "fqdn.example.com")).

representation_test() ->
    Uuid4 = uuid:uuid4(),
    Uuid5 = uuid:uuid5(dns, "fqdn.example.com"),
    ?assertMatch(Uuid4, uuid:to_binary(uuid:to_string(Uuid4))),
    ?assertMatch(Uuid5, uuid:to_binary(uuid:to_string(Uuid5))).

tostring_test() ->
    SimpleUuid = "8fd7fa874c205809a1b0e07f5c224f02",
    PrettyUuid = "8fd7fa87-4c20-5809-a1b0-e07f5c224f02",
    ?assertMatch(PrettyUuid, uuid:to_string(uuid:uuid5(dns, "fqdn.example.com"))),
    ?assertMatch(PrettyUuid, uuid:to_string(pretty, uuid:uuid5(dns, "fqdn.example.com"))),
    ?assertMatch(SimpleUuid, uuid:to_string(simple, uuid:uuid5(dns, "fqdn.example.com"))).

exceptions_test() ->
    ?assertMatch(ok, try_badarg(to_binary, 0)),
    ?assertMatch(ok, try_badarg(to_string, 0)).


try_badarg(F, A) ->
    try
        uuid:F(A)
    catch error:badarg ->
        ok
    end.
