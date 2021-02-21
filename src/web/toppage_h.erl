%%%-------------------------------------------------------------------
%%% @author ysx
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Oct 2019 14:40
%%%-------------------------------------------------------------------
-module(toppage_h).
-author("ysx").

-record(state, {}).

%% ==================================================
%% API
%% ==================================================
-export([init/2, content_types_provided/2, html/2, json/2, text/2, upload/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([handle/2]).

init(Req, Opts) ->
%%    {ok, Req, #state{}}.
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    case cowboy_req:method(Req) of
        <<"OPTIONS">> ->
            reply(<<"allow apply">>, Req),
            {stop, Req, State};
        _ ->
            Methods = [
                <<"GET">>,
                <<"HEAD">>,
                <<"POST">>,
                <<"PUT">>,
                <<"PATCH">>,
                <<"DELETE">>
            ],
            {Methods, Req, State}
    end.


handle(Req, State=#state{}) ->
    {ok, Req, State}.

content_types_accepted(Req, State) ->
    {[
        {<<"text/html">>, html},
        {<<"application/json">>, json},
        {<<"text/plain">>, text},
        {<<"multipart/form-data">>, upload}
    ], Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"text/html">>, html},
        {<<"application/json">>, json},
        {<<"text/plain">>, text},
        {<<"multipart/form-data">>, upload}
    ], Req, State}.

html(Req, State) ->
    Body = <<"{\"rest\": \"Hello World!\"}">>,
    {Body, Req, State}.

json(Req, State) ->
    Path = cowboy_req:path(Req),
    {ok, RawBody, _Req1} = cowboy_req:read_body(Req),
    DataIn = jsx:decode(RawBody, [return_maps]),
    Jsx = web_routes:routing(Path, DataIn),
    DataOut = jsx:encode(Jsx),
    reply_json(DataOut, Req),
    {stop, Req, State}.

upload(Req, State) ->
    case cowboy_req:read_part(Req, #{length => 3145728}) of
        {ok, Headers, Req2} ->
            Req4 =
                case cow_multipart:form_data(Headers) of
                    {data, _FieldName} ->
                        {ok, _Body, Req3} = cowboy_req:read_body(Req2),
                        Req3;
                    {file, _FieldName, FileName, _Type} ->
                        Iff = file_name(FileName),
                        Req5 = stream_file(Req2,Iff),
                        file:close(Iff),
                        Req5
                end,
            upload(Req4, State);
        {done, Req2} ->
            reply(get(url), Req2),
            Req2;
        _ ->
            reply(<<"error">>, Req),
            Req
    end.

text(Req, State) ->
    {<<"REST Hello World as text!">>, Req, State}.



%% ==================================================
%% Internal
%% ==================================================
reply(Data, Req) ->
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/x-www-form-urlencoded;charset=utf-8">>,
        <<"Access-Control-Allow-Origin">> => <<"*">>,
        <<"Access-Control-Allow-Headers">> => <<"Origin, X-Requested-With, Content-Type, Accept, Referer, User-Agent, Authorization, X-Auth-Token">>,
        <<"Access-Control-Allow-Methods">> => <<"POST, GET, PUT, DELETE, OPTIONS">>
    }, Data, Req).

reply_json(Data, Req) ->
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, Data, Req).



file_name(_Filename) ->
    List = string:tokens(binary_to_list(_Filename), "."),
    case lists:reverse(List) of
        [Type | _] ->
            put(file_type, "."++Type),
            ok;
        _ ->
            put(file_type, ""),
            ignore
    end.



stream_file(Req,Iff) ->
    case catch cowboy_req:read_part_body(Req) of
        {ok, _Body, Req2} ->
            FileName = cry_hash:sha(_Body) ++ get(file_type),
            {ok, Dir} = application:get_env(logic, upload_dir),
            {ok, Url} = application:get_env(logic, upload_url),
            file:write_file(list_to_binary(lists:concat([Dir, FileName])), _Body),
            put(url, unicode:characters_to_binary(Url++FileName)),
            Req2;
        {more, _Body, Req2} ->
            stream_file(Req2,Iff);
        _ ->
            Req
    end.