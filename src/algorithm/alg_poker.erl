-module(alg_poker).

-include("logger.hrl").

-define(SUIT_COUNTER, [{s,[]},{h,[]},{d,[]},{c,[]}]).
-define(RANK_COUNTER, [{2,0},{3,0},{4,0},{5,0},{6,0},{7,0},{8,0},{9,0},{10,0},{11,0},{12,0},{13,0},{14,0}]).

-define(HIGH_CARD, 1).
-define(PAIR, 2).
-define(TOW_PAIR, 3).
-define(THREE_OF_KIND, 4).
-define(STRAIGHT, 5).
-define(FLUSH, 6).
-define(FULL_HOUSE, 7).
-define(FOUR_OF_KIND, 8).
-define(STRAIGHT_FLUSH, 9).

% ====================================================================
% API
% ====================================================================
-export([evaluate/1, get_hands_results/2, get_winner/1]).
-export([compare_hand/2]).

evaluate(Cards) ->
    Counter = format_counter(Cards, {?SUIT_COUNTER, ?RANK_COUNTER}),
    hand_evaluate(Counter).

get_hands_results(Hands, Community) ->
    CommunityCounter = community_counter(Community),
    HandCounters = format_hand_counter(Hands, CommunityCounter),
    evaluate_hand_counters(HandCounters).

get_winner(Results) ->
    evaluate_winner(Results).

compare_hand(Hands, Community) ->
    Results = get_hands_results(Hands, Community),
    Winner = evaluate_winner(Results),
    {Winner, Results}.

% ====================================================================
% Internal
% ====================================================================
evaluate_hand_counters(List) ->
    lists:map(fun({ID, Counter}) ->
        Result = hand_evaluate(Counter),
        {ID, Result}
    end, List).

community_counter(Cards) ->
    format_counter(Cards, {?SUIT_COUNTER, ?RANK_COUNTER}).



% --------------------------------------------------------------------
% evaluate_winner
% -------------------------------------------------------------------- 

evaluate_winner(List) ->
    [{ID, E} | Tail] = List,
    {Winner, _} =
    lists:foldl(fun({NID, {K, V}}, {OldID, {Ko, Vo}}) ->
        case compare_evaluate({K, V}, {Ko, Vo}) of
            true ->
                {[NID], {K, V}};
            false ->
                {OldID, {Ko, Vo}};
            equal ->
                {[NID | OldID], {Ko, Vo}}
        end
    end, {[ID], E}, Tail),
    Winner.

compare_evaluate({K1, V1}, {K2, V2}) ->
     case K1 of
        K2 ->
            compare_list(V1, V2);
        _ ->
            K1 > K2
    end.

compare_list([], []) ->
    equal;
compare_list([H1 | T1], [H2 | T2]) ->
    case H1 of
        H2 ->
            compare_list(T1, T2);
        _ ->
            H1 > H2
    end.

% --------------------------------------------------------------------
% format
% -------------------------------------------------------------------- 
format_hand_counter(Hands, CommunityCounter) ->
    lists:map(fun({ID, Cards}) ->
        Counter = format_counter(Cards, CommunityCounter),
        {ID, Counter}
    end, Hands).

format_counter(Cards, {SuitCounter, RankCounter}) ->
    lists:foldl(fun(X, {AccS, AccR}) ->
        Suit = get_suit(X),
        Rank = get_rank(X),
        NewAccS = push_counter(Suit, Rank, AccS),
        NewAccP = increase_counter(Rank, AccR),
        {NewAccS, NewAccP}
    end, {SuitCounter, RankCounter}, Cards).

increase_counter(Key, List) ->
    {Key, Count} = lists:keyfind(Key, 1, List),
    lists:keyreplace(Key, 1, List, {Key, Count + 1}).

push_counter(Key, Value, List) ->
    {Key, Acc} = lists:keyfind(Key, 1, List),
    lists:keyreplace(Key, 1, List, {Key, [Value | Acc]}).


% --------------------------------------------------------------------
% hand_evaluate
% --------------------------------------------------------------------  
hand_evaluate(Counter) ->
    {SuitCounter, RankCounter} = available_counter(Counter),
    case straight_flush_evaluate(SuitCounter, RankCounter) of
        false ->
            no_flush_evaluate(RankCounter);
        {false, FlushResult} ->
            FlushResult;
        Result ->
            Result
    end.

available_counter(Counter) ->
    {SuitCounter, RankCounter} = Counter,
    AS = available_suit_counter(SuitCounter),
    AR = available_rank_counter(RankCounter),
    {AS, AR}.
  
available_suit_counter(SuitCounter) ->
    List = lists:sort(fun({_, A}, {_, B}) ->
        length(A) > length(B)
    end, SuitCounter),    
    [{Suit, Ranks} | _] = List,
    SR = lists:sort(fun(A, B) ->
        A > B
    end,  Ranks),
    {Suit, SR}.

available_rank_counter(RankCounter) ->
    lists:foldr(fun(X, Acc) ->
        {_, C} = X,
        case C of
            0 ->
                Acc;
            _ ->
                [X | Acc]
        end
    end, [], RankCounter).

% --------------------------------------------------------------------
% no_flush_evaluate
% --------------------------------------------------------------------
sort_rank_counter(RankCounter) ->
    lists:sort(fun({K1, V1}, {K2, V2}) ->
        case V1 of
            V2 ->
                K1 > K2;
            _ ->
                V1 > V2
        end
    end, RankCounter).

no_flush_evaluate(RankCounter) ->
    SortCounter = sort_rank_counter(RankCounter),
    case four_of_kind_evaluate(SortCounter) of
        false ->
            no_flush_evaluate_1(RankCounter, SortCounter);
        Result ->
            Result
    end.

no_flush_evaluate_1(RankCounter, SortCounter) ->
    case full_house(SortCounter) of
        false ->
            no_flush_evaluate_2(RankCounter, SortCounter);
        Result ->
            Result
    end.

no_flush_evaluate_2(RankCounter, SortCounter) ->
    case straight_evaluate(RankCounter) of
        false ->
            no_flush_evaluate_3(SortCounter);
        Result ->
            Result
    end.

no_flush_evaluate_3(SortCounter) ->
    case three_of_kind_evaluate(SortCounter) of
        false ->
            no_flush_evaluate_4(SortCounter);
        Result ->
            Result
    end.

no_flush_evaluate_4(SortCounter) ->
    case tow_pair_evaluate(SortCounter) of
        false ->
            no_flush_evaluate_5(SortCounter);
        Result ->
            Result
    end.

no_flush_evaluate_5(SortCounter) ->
    case pair_evaluate(SortCounter) of
        false ->
            high_card_evaluate(SortCounter);
        Result ->
            Result
    end.

% --------------------------------------------------------------------
% straight_flush_evaluate
% --------------------------------------------------------------------
straight_flush_evaluate(SuitCounter, RankCounter) ->
    case flush_evaluate(SuitCounter) of
        {true, FlushResult} ->
            Counter = get_rank_of_suit(SuitCounter, RankCounter),
            case straight_evaluate(Counter) of
                false ->
                    {false, {?FLUSH, FlushResult}};
                {_, Result} ->
                    {?STRAIGHT_FLUSH, Result}
            end;
        false ->
            false
    end.

get_rank_of_suit(SuitCounter, RankCounter) ->
    {_, Ranks} = SuitCounter,
    lists:foldl(fun(X, Acc) ->
        case lists:keyfind(X, 1, RankCounter) of
            Item ->
                [Item | Acc];
            false ->
                Acc
        end
    end, [], Ranks).

% --------------------------------------------------------------------
% flush_evaluate
% --------------------------------------------------------------------
flush_evaluate(SuitCounter) ->
    {_, Ranks} = SuitCounter,
    case length(Ranks) > 4 of
        true ->
            {Result, _} = lists:split(5, Ranks),
            {true, Result};
        false ->
            false
    end.

% --------------------------------------------------------------------
% straight_evaluate
% --------------------------------------------------------------------
straight_evaluate(RankCounter) ->
    case length(RankCounter) > 4 of
        true ->
            {L, C} = lists:last(RankCounter),
            case L of
                14 ->
                    straight_evaluate_1([{1, C} | RankCounter]);
                _ ->
                    straight_evaluate_1(RankCounter)
            end;
        false ->
            false
    end.

straight_evaluate_1(RankCounter) ->
    {_, List} =
    lists:foldr(fun(X, {A, Acc}) ->
        B = [X | A],
        case length(B) of
            5 ->
                NewAcc = [B | Acc],
                [_ | T] = lists:reverse(B),
                NewA = lists:reverse(T),
                {NewA, NewAcc};
            _ ->
                {B, Acc}
        end
    end, {[], []}, RankCounter),
    straight_evaluate_2(lists:reverse(List)).

straight_evaluate_2([]) ->
    false;
straight_evaluate_2([RankCounter | T]) ->
    {H, _} = lists:nth(1, RankCounter),
    {L, _} = lists:last(RankCounter),
    case H + 4 of
        L ->
            {?STRAIGHT, get_straight_ranks(RankCounter)};
        _ ->            
            straight_evaluate_2(T)
    end.

get_straight_ranks(RankCounter) ->
    lists:foldl(fun({K, _}, Acc) ->
        [K | Acc]
    end, [], RankCounter).

% --------------------------------------------------------------------
% Four of kind
% --------------------------------------------------------------------
four_of_kind_evaluate(RankCounter) ->
    [{H, V1}, {K, _} | _] = RankCounter,
    case V1 of
        4 ->
            {?FOUR_OF_KIND, [H, K]};
        _ ->
            false
    end.

% --------------------------------------------------------------------
% full house
% --------------------------------------------------------------------
full_house(SortCounter) ->
    [{K1, V1}, {K2, V2} | _] = SortCounter,
    case {V1, V2} of
        {3, 2} ->
            {?FULL_HOUSE, [K1, K2]};
        _ ->
            false
    end.

% --------------------------------------------------------------------
% Three of kind
% --------------------------------------------------------------------
three_of_kind_evaluate(RankCounter) ->
    [{K1, V1}, {K2, _}, {K3, _} | _] = RankCounter,
    case V1 of
        3 ->
            {?THREE_OF_KIND, [K1, K2, K3]};
        _ ->
            false
    end.

% --------------------------------------------------------------------
% Two pair
% --------------------------------------------------------------------
tow_pair_evaluate(RankCounter) ->
    [{K1, V1}, {K2, V2}, {K3, _} | _] = RankCounter,
    case {V1, V2} of
        {2, 2} ->
            {?TOW_PAIR, [K1, K2, K3]};
        _ ->
            false
    end.

% --------------------------------------------------------------------
% Pair
% --------------------------------------------------------------------
pair_evaluate(RankCounter) ->
    [{K1, V1}, {K2, _}, {K3, _}, {K4, _} | _] = RankCounter,
    case V1 of
        2 ->
            {?PAIR, [K1, K2, K3, K4]};
        _ ->
            false
    end.

% --------------------------------------------------------------------
% High Card
% --------------------------------------------------------------------
high_card_evaluate(RankCounter) ->
    [{K1, _}, {K2, _}, {K3, _}, {K4, _}, {K5, _} | _] = RankCounter,
    {?HIGH_CARD, [K1, K2, K3, K4, K5]}.

% --------------------------------------------------------------------
% suit and rank
% --------------------------------------------------------------------
get_suit(Card) ->
    [Suit,_] = Card,
    case Suit of
        $S ->
            s;
        $H ->
            h;
        $D ->
            d;
        $C ->
            c
    end.

get_rank(Card) ->
    [_,Point] = Card,
    case Point of
        $A ->
            14;
        $K ->
            13;
        $Q ->
            12;
        $J ->
            11;
        $T ->
            10;
        $9 ->
            9;
        $8 ->
            8;
        $7 ->
            7;
        $6 ->
            6;
        $5 ->
            5;
        $4 ->
            4;
        $3 ->
            3;
        $2 ->
            2
    end.