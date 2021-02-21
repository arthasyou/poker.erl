-module(main_dict).

-compile(export_all).

% ====================================================================
% tables
% ====================================================================

enter_table(TableID) ->
    All = get(all_table),
    case All of
        undefined ->
            put(all_table, [TableID]);
        _ ->
            put(all_table, [TableID | All])
    end.

leave_table(TableID) ->
    All = get(all_table),
    case All of
        undefined ->
            ok;
        _ ->
            put(all_table, lists:delete(TableID, All))
    end.

get_all_table() ->
    All = get(all_table),
    case All of
        undefined ->
            [];
        _ ->
            All
    end.