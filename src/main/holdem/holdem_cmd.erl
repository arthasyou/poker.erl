-module(holdem_cmd).

-include("all_pb.hrl").
-include("logger.hrl").

-export([handle/3]).

handle(2000, Data, ID) ->
    holdem_callback:enter_table(Data, ID);

handle(2002, Data, ID) ->
    holdem_callback:sit_down(Data, ID);

handle(2003, Data, ID) ->
    holdem_callback:ready(Data, ID);

handle(2011, Data, ID) ->
    holdem_callback:fold(Data, ID);

handle(2012, Data, ID) ->
    holdem_callback:check_call(Data, ID);

handle(2013, Data, ID) ->
    holdem_callback:bet_raise(Data, ID);

handle(_Cmd, _Data, _ID) ->
    {error, 1}.