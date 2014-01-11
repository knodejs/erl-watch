-module(watch_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, watch}
    ].

groups() ->
    [{watch, [], [{watch_test, all}]}].
