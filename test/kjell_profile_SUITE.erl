%%%-------------------------------------------------------------------
%%% @author karl l <karl@ninjacontrol.com>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(kjell_profile_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    %
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    %ok = kjell_profile:stop(),
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [load_profile, set_value, get_value, get_undef_value].

set_value() ->
    [].
set_value(Config) ->
    {ok, Pid} = kjell_profile:start_link(),
    ok = kjell_profile:set_value(test, testvalue).

get_value() ->
    [].
get_value(Config) ->
    {ok, Pid} = kjell_profile:start_link(),
    ok = kjell_profile:set_value(test2, testvalue2),
    testvalue2 = kjell_profile:get_value(test2).

get_undef_value() ->
    [].
get_undef_value(Config) ->
    {ok, Pid} = kjell_profile:start_link(),
    undefined = kjell_profile:get_value(undef).

load_profile() ->
    [].
load_profile(Config) ->
    {ok, Pid} = kjell_profile:start_link(),
    DataDir = proplists:get_value(data_dir,Config),
    ok = kjell_profile:load_profile(filename:join(DataDir,"kjell.config")),
    TxtAttrs = kjell_profile:get_value('text-attr'),
    ExpectedAttrs = [{string,"\e[1;33m~ts\e[0m"},
		     {digits,"\e[1;32m~ts\e[0m"},
		     {keyword,"\e[1;35m~ts\e[0m"},
		     {warning,"\e[2;31m~ts\e[0m"},
		     {error,"\e[4;31m~ts\e[0m"},
		     {term,"\e[1;36m~ts\e[0m"}],

    ct:pal("Got state = ~p",[TxtAttrs]),
    TxtAttrs = ExpectedAttrs,
    kjell_profile:stop(),
    ok.
