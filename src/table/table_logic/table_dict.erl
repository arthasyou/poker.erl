-module(table_dict).

-include("logger.hrl").

-compile(export_all).

% ====================================================================
% seat chain
% ====================================================================

put_seat_chain(SeatID, NextSeatID) ->
    put({next_seat, SeatID}, NextSeatID),
    put({pre_seat, NextSeatID}, SeatID).

get_next_seat(SeatID) ->
    get({next_seat, SeatID}).

get_pre_seat(SeatID) ->
    get({pre_seat, SeatID}).

delete_seat(SeatID) ->
    ?DEBUG("delete seat: ~p~n", [SeatID]),
    NextSeatID = get_next_seat(SeatID),
    PreSeatID = get_pre_seat(SeatID),
    put_seat_chain(PreSeatID, NextSeatID),
    erase({next_seat, SeatID}),
    erase({pre_seat, SeatID}).

% ====================================================================
% side pot
% ====================================================================

put_side_pot(SeatID, PotID) ->   
    Seq = lists:seq(1, PotID), 
    put({side_pot_seat, SeatID}, Seq),
    put({side_pot_id, PotID}, SeatID),

    PotInfo = get(side_pot_key),    
    case PotInfo of
        undefined ->            
            put(side_pot_key, {[SeatID], [PotID]});
        {SeatKeys, IdKeys} ->
            put(side_pot_key, {[SeatID | SeatKeys], [PotID | IdKeys]})
    end.

get_side_pot_ids(SeatID) ->
    get({side_pot_seat, SeatID}).

get_side_pot_seat(ID) ->
    get({side_pot_id, ID}).

clear_side_pot() ->
    case get(side_pot_key) of
        undefined ->
            ok;
        {SeatKeys, IdKeys} ->
            lists:foreach(fun(X) ->
                erase({side_pot_seat, X})
            end, SeatKeys),
            lists:foreach(fun(X) ->
                erase({side_pot_id, X})
            end, IdKeys),
            erase(side_pot_key)
    end.

get_side_pot() ->
    case get(side_pot_key) of
        undefined ->
            ok;
        {Keys, IDS} ->            
            ?DEBUG("Keys: ~p~n", [Keys]),
            ?DEBUG("IDS: ~p~n", [IDS]),
            lists:foreach(fun(X) ->
                ?DEBUG("seat_id : ~p pot_id: ~p~n", [X, get({side_pot_seat, X})])
            end, Keys),
            lists:foreach(fun(X) ->
                ?DEBUG("pot: ~p seat_id: ~p~n", [X, get({side_pot_id, X})])
            end, IDS)
    end.


% ====================================================================
% player
% ====================================================================

player_enter(ID) ->
    All = get(all_player),
    case All of
        undefined ->
            put(all_player, [ID]);
        _ ->
            case lists:member(ID, All) of
                true ->
                    ok;
                false ->
                    put(all_player, [ID | All])
            end
    end.

player_leave(ID) ->
    All = get(all_player),
    case All of
        undefined ->
            ok;
        _ ->
            put(all_player, lists:delete(ID, All))
    end.

get_all_player() ->
    All = get(all_player),
    case All of
        undefined ->
            [];
        _ ->
            All
    end.

% ====================================================================
% player win pot
% ====================================================================
%% Info -> {ActorID, Value}
put_win_pot_record(Info) ->
    case get(win_pot) of
        undefined ->
            put(win_pot, Info);
        List ->
            put(win_pot, List ++ Info)
    end.

get_win_pot_record() ->
    get(win_pot).

clear_win_pot_record() ->
    erase(win_pot).

% ====================================================================
% player seat id
% ====================================================================
put_player_seat_id(PlayerId, SeatID) ->
    put({player_seat_id, PlayerId}, SeatID).
get_player_seat_id(PlayerId) ->
    get({player_seat_id, PlayerId}).
clear_player_seat_id(PlayerId) ->
    erase({player_seat_id, PlayerId}).