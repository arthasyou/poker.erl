-ifndef(ERROR_CODE).
-define(ERROR_CODE, true).

%% ===================================================================
%% common error
%% ===================================================================

-define(SYS, 1).

%% ===================================================================
%% table error
%% ===================================================================

-define(ILLEGAL_SEAT, 101).
-define(SEAT_NOT_EMPTY, 102).
-define(ALREADY_HAD_SEAT, 103).
-define(NO_SEAT, 104).
-define(ILLEGAL_STATE, 105).
-define(ILLEGAL_ACTION, 106).

-endif.