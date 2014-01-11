-module(watch_test).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [find_apps_to_watch,
     find_changed_files,
     md5sum_beam_files
    ].

find_apps_to_watch() ->
    [].

find_apps_to_watch(_Config) ->
    true = length(watch:find_apps_to_watch()) > 0.

find_changed_files() ->
    [].

find_changed_files(_Config) ->
    Cur = [{"/root/file1", 10},
           {"/root/file2", 20},
           {"/root/file3", 30},
           {"/root/file5", 50}],

    New = [{"/root/file1", 11},
           {"/root/file2", 20},
           {"/root/file3", 30},
           {"/root/file4", 40}],

    Expected = [{reload, modified, 'file1'},
                {noreload, not_changed, 'file2'},
                {noreload, not_changed, 'file3'},
                {reload, new, 'file4'}],

    Expected = watch:find_changed_files(Cur, New).

md5sum_beam_files() ->
    [].

md5sum_beam_files(_Config) ->
    [] = watch:md5sum_beam_files([]),

    ct:pal("~p", [os:cmd("pwd")]),
    Result = watch:md5sum_beam_files(["../../ebin"]),
    3 = lists:sum([1  || {_Filename, MD5} <- Result, is_binary(MD5)]),
    ok.
