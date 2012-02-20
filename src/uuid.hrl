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
