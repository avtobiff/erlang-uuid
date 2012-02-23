%% -----------------------------------------------------------------------------
%% Copyright © 2010-2012 Per Andersson
%% Copyright © 2012 Bip Thelin
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
%%            2012 Bip Thelin
%% @doc
%% Erlang UUID
%%
%% Currently implements UUID v1, v3, v4, and v5 as of RFC 4122.
%%
%% Example usage
%% <pre>
%%      1> uuid:to_string(uuid:uuid1()).
%%      "f412e400-c445-1131-bdc6-03f9e757eb34"
%%      2> uuid:to_string(uuid:uuid3(dns, "fqdn.example.com")).
%%      "06eaa791-8c2e-3b0d-8a07-c80979fd1b98"
%%      3> uuid:to_string(uuid:uuid3(uuid:uuid4(), "my name")).
%%      "fcf82b93-aa5e-3d79-b95e-726420f89e1b"
%%      4> uuid:to_string(uuid:uuid4()).
%%      "79f492f8-1337-4200-abcd-92bada1cacao"
%%      5> uuid:to_string(uuid:uuid5(dns, "fqdn.example.com")).
%%      "8fd7fa87-4c20-5809-a1b0-e07f5c224f02"
%%      6> uuid:to_string(uuid:uuid5(uuid:uuid4(), "my name")).
%%      "6ff58b11-e0b2-536c-b6be-bdccd38836a2"
%% </pre>
%% @end
%% @reference See <a href="http://www.ietf.org/rfc/rfc4122.txt">RFC 4122</a>
%%            for more information.
%% -----------------------------------------------------------------------------

-module(uuid).
-author('Per Andersson').

-include("uuid.hrl").

-export([get_node/0,
         to_binary/1,
         to_string/1, to_string/2,
         to_uuid_urn/1,
         uuid1/0, uuid1/2,
         uuid3/2,
         uuid4/0,
         uuid5/2,
         variant/1,
         version/1,
         is_v1/1, is_v3/1, is_v4/1, is_v5/1,
         is_valid/1]).


%% =============================================================================
%% UUID v1
%% =============================================================================

%% @doc Create a UUID v1 (timebased).
-spec uuid1() -> uuid().
uuid1() ->
    uuid1(null, null).

-spec uuid1(NodeArg::binary() | null, ClockSeqArg::binary() | null) -> uuid().
uuid1(NodeArg, ClockSeqArg) ->
    %% Determine values for UTC timestamp and clock sequence.
    %% FIXME Use random clock sequence for now ("state is unavailable").
    UtcTimestamp =
        calendar:datetime_to_gregorian_seconds(calendar:universal_time()),

    %% Multiply timestamp with amount of hundred nanosecond intervals per
    %% second since clock have lower resolution than this.
    HundredNanosPerSecond = round(1.0e9), % to integer()
    Timestamp = UtcTimestamp * HundredNanosPerSecond,

    <<TimeHi:12, TimeMid:16, TimeLow:32>> = <<Timestamp:60>>,

    ClockSequence =
        %% Use ClockSeq if supplied otherwise Generate random clock sequence.
        case ClockSeqArg of
            null        ->
                random:seed(now()),
                Rnd = random:uniform(2 bsl 14 - 1),
                <<Rnd:14>>;
            ClockSeqArg ->
                <<_:14>> = ClockSeqArg % make 14 bits wide
        end,
    <<ClockSeqHi:6, ClockSeqLow:8>> = ClockSequence,

    %% Get MAC address
    Node =
        case NodeArg of
            null -> get_node();
            _    -> NodeArg
        end,

    %% Compose UUIDv1
    <<TimeLow:32, TimeMid:16, ?UUIDv1:4, TimeHi:12,
      ?VARIANT10:2, ClockSeqLow:8, ClockSeqHi:6, Node/binary>>.


%% =============================================================================
%% UUID v3
%% =============================================================================
%% @doc  Create a UUID v3 (name based, MD5 is hashing function) as a binary.
%%       Magic numbers are from Appendix C of the RFC 4122.
-spec uuid3(NamespaceOrUuid::atom() | uuid_string() | uuid(),
            Name::string()) -> uuid().
uuid3(dns, Name) ->
    create_namebased_uuid(md5,
        list_to_binary([<<16#6ba7b8109dad11d180b400c04fd430c8:128>>, Name]));
uuid3(url, Name) ->
    create_namebased_uuid(md5,
        list_to_binary([<<16#6ba7b8119dad11d180b400c04fd430c8:128>>, Name]));
uuid3(oid, Name) ->
    create_namebased_uuid(md5,
        list_to_binary([<<16#6ba7b8129dad11d180b400c04fd430c8:128>>, Name]));
uuid3(x500, Name) ->
    create_namebased_uuid(md5,
        list_to_binary([<<16#6ba7b8149dad11d180b400c04fd430c8:128>>, Name]));
uuid3(nil, Name) ->
    create_namebased_uuid(md5, list_to_binary([<<0:128>>, Name]));
uuid3(UuidStr, Name) when is_list(UuidStr) ->
    create_namebased_uuid(md5, list_to_binary([to_binary(UuidStr), Name]));
uuid3(UuidBin, Name) when is_binary(UuidBin) ->
    create_namebased_uuid(md5, list_to_binary([UuidBin, Name]));
uuid3(_, _) ->
    erlang:error(badarg).


%% =============================================================================
%% UUID v4
%% =============================================================================

%% @doc  Create a UUID v4 (random) as a binary
-spec uuid4() -> uuid().
uuid4() ->
    random:seed(now()),

    U0 = random:uniform((2 bsl 48) - 1),
    U1 = random:uniform((2 bsl 12) - 1),
    U2 = random:uniform((2 bsl 62) - 1),

    uuid4(U0, U1, U2).


%% @private
%% @doc  Create a 128 bit binary (UUID v4) from input
-spec uuid4(U0::integer(), U1::integer(), U2::integer()) -> uuid().
uuid4(U0, U1, U2) ->
    % Set the four most significant bits (bits 12 through 15) of the
    % time_hi_and_version field to 0100, corresponding to version 4.

    % Set the two most significant bits (bits 6 and 7) of the
    % clock_seq_hi_and_reserved to zero and one, respectively.
    % Corresponding to variant 1 0.

    % Set all other bits pseudo-randomly chosen values
    % (as generated by caller).

    <<U0:48, ?UUIDv4:4, U1:12, ?VARIANT10:2, U2:62>>.


%% =============================================================================
%% UUID v5
%% =============================================================================

%% @doc  Create a UUID v5 (name based, SHA1 is hashing function) as a binary.
%%       Magic numbers are from Appendix C of the RFC 4122.
-spec uuid5(NamespaceOrUuid::atom() | uuid_string() | uuid(),
            Name::string()) -> uuid().
uuid5(dns, Name) ->
    create_namebased_uuid(sha1,
        list_to_binary([<<16#6ba7b8109dad11d180b400c04fd430c8:128>>, Name]));
uuid5(url, Name) ->
    create_namebased_uuid(sha1,
        list_to_binary([<<16#6ba7b8119dad11d180b400c04fd430c8:128>>, Name]));
uuid5(oid, Name) ->
    create_namebased_uuid(sha1,
        list_to_binary([<<16#6ba7b8129dad11d180b400c04fd430c8:128>>, Name]));
uuid5(x500, Name) ->
    create_namebased_uuid(sha1,
        list_to_binary([<<16#6ba7b8149dad11d180b400c04fd430c8:128>>, Name]));
uuid5(nil, Name) ->
    create_namebased_uuid(sha1, list_to_binary([<<0:128>>, Name]));
uuid5(UuidStr, Name) when is_list(UuidStr) ->
    create_namebased_uuid(sha1, list_to_binary([to_binary(UuidStr), Name]));
uuid5(UuidBin, Name) when is_binary(UuidBin) ->
    create_namebased_uuid(sha1, list_to_binary([UuidBin, Name]));
uuid5(_, _) ->
    erlang:error(badarg).


%% @private
%% @doc  Create a UUID v3 or v5 (name based) from binary, using MD5 or SHA1
%%       respectively.
-spec create_namebased_uuid(HashFunction::md5 | sha1,
                            Data::binary()) -> uuid().
create_namebased_uuid(md5, Data) ->
    Md5 = crypto:md5(Data),
    compose_namebased_uuid(?UUIDv3, Md5);
create_namebased_uuid(sha1, Data) ->
    <<Sha1:128, _:32>> = crypto:sha(Data),
    compose_namebased_uuid(?UUIDv5, <<Sha1:128>>).

%% @private
%% @doc  Compose a namebased UUID (v3 or v5) with input hashed data.
-spec compose_namebased_uuid(Version::3 | 5, Hash::binary()) -> uuid().
compose_namebased_uuid(Version, Hash) ->
    <<TimeLow:32, TimeMid:16, _AndVersion:4, TimeHi:12,
      _AndReserved:2, ClockSeqHi:6, ClockSeqLow:8, Node:48>> = Hash,

    <<TimeLow:32, TimeMid:16, Version:4, TimeHi:12,
      ?VARIANT10:2, ClockSeqHi:6, ClockSeqLow:8, Node:48>>.


%% =============================================================================
%% Formatting functions
%% =============================================================================

%% @doc  Format UUID string from binary
-spec to_string(Uuid::uuid()) -> uuid_string().
to_string(<<_:128>> = Uuid) when is_binary(Uuid) ->
    to_string(pretty, Uuid);
to_string(_) ->
    erlang:error(badarg).

-spec to_string(simple | pretty, Uuid::uuid()) -> uuid_string().
to_string(pretty, <<U0:32, U1:16, U2:16, U3:16, U4:48>>) ->
    lists:flatten(io_lib:format(
        "~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
        [U0, U1, U2, U3, U4]));
to_string(simple, <<S:128>>) ->
    lists:flatten(io_lib:format("~32.16.0b", [S]));
to_string(_, _) ->
    erlang:error(badarg).


%% @doc  Create UUID URN from UUID binary or string.
-spec to_uuid_urn(UuidOrUrn::uuid() | uuid_string()) -> urn().
to_uuid_urn([$u, $r, $n, $:, $u, $u, $i, $d, $: |_] = Urn) ->
    Urn;
to_uuid_urn(<<_:128>> = Uuid) ->
    "urn:uuid:" ++ uuid:to_string(Uuid);
to_uuid_urn(Uuid) when is_list(Uuid) ->
    "urn:uuid:" ++ Uuid.


%% @doc Format UUID binary from string.
-spec to_binary(UuidStr::uuid_string()) -> uuid().
to_binary(UuidStr) when is_list(UuidStr) ->
    case length(UuidStr) of
        36 -> to_binary(pretty, UuidStr);
        32 -> to_binary(simple, UuidStr);
        _  -> erlang:error(badarg)
    end;
to_binary(_) ->
    erlang:error(badarg).

%% @private
-spec to_binary(simple | pretty, UuidStr::uuid_string()) -> uuid().
to_binary(simple, UuidStr) ->
    Num = hex_to_int(UuidStr),
    <<Num:128>>;
to_binary(pretty, UuidStr) ->
    Parts = string:tokens(UuidStr, "$-"),
    [I0, I1, I2, I3, I4] = [hex_to_int(Part) || Part <- Parts],
    <<I0:32, I1:16, I2:16, I3:16, I4:48>>.



%% =============================================================================
%% Helper functions
%% =============================================================================

%% @private
%% @doc  Convert from hexadecimal digit represented as string to decimal.
-spec hex_to_int(Hex::string()) -> integer().
hex_to_int(Hex) ->
    {ok, [D], []} = io_lib:fread("~16u", Hex),
    D.


%% @private
%% @doc Predicate function for filtering interfaces to use
-spec filter_if({IfName::string(), IfConfig::list(tuple())}) -> true | false.
filter_if({IfName, IfConfig}) ->
    [HasHwAddr] =
        [true || {IfConfigItem, _} <- IfConfig, IfConfigItem =:= hwaddr],

    case {IfName, HasHwAddr} of
        % do not use loopback interface
        {"lo", _}     -> false;
        % use interface with a hwaddr
        {_   , true}  -> true;
        % do not use interfaces without a hwaddr
        _             -> false
    end.


%% @doc Get node id (IEEE 802 (MAC) address). Create random node id if hardware
%%      addres can be found.
-spec get_node() -> binary().
get_node() ->
    %% Get interfaces
    {ok, Ifs0} = inet:getifaddrs(),

    %% Take hwaddr from first interface
    Ifs1 = lists:filter(fun filter_if/1, Ifs0),

    case length(Ifs1) of
        %% No interface, create random 48-bit number with bit 8 set to one.
        0 -> random:seed(now()),
             Rnd = random:uniform(2 bsl 48 - 1),
             <<RndHi:7, _:1, RndLow:40>> = <<Rnd:48>>,
             %% Set 8 to 1
             <<RndHi:7, 1:1, RndLow:40>>;

        %% Use hardware address from first interface found.
        _ -> [{_IfName, IfConfig}|_] = Ifs1,
             [HwAddr] = [HwAddr || {IfConfigItemName, HwAddr}  <- IfConfig,
                                   IfConfigItemName =:= hwaddr],
             list_to_binary(HwAddr)
    end.


%% @doc Return version for supplied UUID.
-spec version(Uuid::uuid() | uuid_string()) -> integer().
version(<<_:128>> = Uuid) ->
    <<_:48, Version:4, _:76>> = Uuid,
    Version;
version(UuidStr) when is_list(UuidStr) ->
    version(uuid:to_binary(UuidStr));
version(_) ->
    erlang:error(badarg).


%% @doc Return variant for supplied UUID.
-spec variant(Uuid::uuid() | uuid_string()) -> reserved_microsoft
                                             | reserved_ncs
                                             | resered_future
                                             | rfc4122.
variant(<<_:128>> = Uuid) ->
    <<_:64, V2:1, V1:1, V0:1, _:61>> = Uuid,
    case {V2, V1, V0} of
        {0, _, _} -> reserved_ncs;
        {1, 0, _} -> rfc4122;
        {1, 1, 0} -> reserved_microsoft;
        {1, 1, 1} -> reserved_future
    end;
variant(UuidStr) when is_list(UuidStr) ->
    variant(uuid:to_binary(UuidStr));
variant(_) ->
    erlang:error(badarg).


%% @doc Predicate for checking that supplied UUID is version 1.
-spec is_v1(Uuid::uuid() | uuid_string()) -> true | false.
is_v1(Uuid) -> ?UUIDv1 =:= version(Uuid).

%% @doc Predicate for checking that supplied UUID is version 3.
-spec is_v3(Uuid::uuid() | uuid_string()) -> true | false.
is_v3(Uuid) -> ?UUIDv3 =:= version(Uuid).

%% @doc Predicate for checking that supplied UUID is version 4.
-spec is_v4(Uuid::uuid() | uuid_string()) -> true | false.
is_v4(Uuid) -> ?UUIDv4 =:= version(Uuid).

%% @doc Predicate for checking that supplied UUID is version 5.
-spec is_v5(Uuid::uuid() | uuid_string()) -> true | false.
is_v5(Uuid) -> ?UUIDv5 =:= version(Uuid).


%% @doc Predicate for checking that supplied UUID is valid.
-spec is_valid(Uuid::uuid() | uuid_string()) -> true | false.
%% XXX special nil UUID is valid
is_valid(<<0:128>>) -> true;
is_valid(Uuid = <<_:128>>) ->
    is_valid(variant(Uuid), Uuid);
is_valid(UuidStr) when is_list(UuidStr) ->
    is_valid(to_binary(UuidStr));
is_valid(_) ->
    erlang:error(badarg).

%% @private
%% @doc Predicate for checking that supplied UUID is valid, takes variant as
%%      argument and returns validity depending on UUID version.
-spec is_valid(Variant::atom(), Uuid::uuid()) -> true | false.
is_valid(rfc4122, Uuid) ->
    Version = version(Uuid),
    case Version of
        ?UUIDv1 -> true;
        ?UUIDv3 -> true;
        ?UUIDv4 -> true;
        ?UUIDv5 -> true;
        _       -> false
    end;
is_valid(_, _) -> false.
