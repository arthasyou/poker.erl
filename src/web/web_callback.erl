%%%-------------------------------------------------------------------
%%% @author ysx
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Oct 2019 17:26
%%%-------------------------------------------------------------------
-module(web_callback).
-author("ysx").

%% ==================================================
%% API
%% ==================================================
-export([do_something/1]).

do_something(DataIn) ->
	#{key := _Val} = DataIn,
	%% reply = {ok, map} | {error, Reason}
	Reply = {ok, #{key => val}},
	reply(Reply).

%% ==================================================
%% Internal
%% ==================================================
reply(Reply) ->
    case Reply of
        {ok, Data} ->
            #{flag => success, data => Data};
        {error, Reason} ->
            lager:info("Reson: ~p~n", [Reply]),
            #{flag => fail, reason => Reason}
    end.
