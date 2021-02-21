%%%-------------------------------------------------------------------
%%% @author ysx
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Oct 2019 15:07
%%%-------------------------------------------------------------------
-module(web_routes).
-author("ysx").

%% ==================================================
%% API
%% ==================================================
-export([routing/2]).

routing(Path, DataIn) ->
    case format_path (Path) of
		"/path" ->
            web_callback:do_something(DataIn);
        _ ->
            other()
    end.


%% ==================================================
%% Internal
%% ==================================================

other() ->
    #{error => "unknow path"}.

format_path(Path) ->
	erlang:binary_to_list(Path).
