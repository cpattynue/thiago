-module(thiago_code).

-export([create/1, update/1, delete/1, get/1 ]).
-include("thiago.hrl").

create({Name, Data}) ->
	io:format("create ~p~n", [?PATH]),
	NewPath = ?PATH ++ "/" ++ Name ++ ".db",
	{ok,Name} = dets:open_file(Name,[{file, NewPath},{type, duplicate_bag}]),
	ok = dets:insert( Name, [{bucket, key}, {notification, [key1, key2, key3]}]),
	ok = dets:close( Name ). 
	
update(Args) ->
        io:format("update ~p~n", [Args]).

delete({all, Name}) ->
	NewPath = ?PATH ++ "/" ++ Name ++ ".db",
        {ok, Name}  = dets:open_file(Name,[{file, NewPath},{type, duplicate_bag}]),
	dets:delete_all_objects(Name);

delete({Name, Key}) ->
        NewPath = ?PATH ++ "/" ++ Name ++ ".db",
        {ok, Name}  = dets:open_file(Name,[{file, NewPath},{type, duplicate_bag}]),
        dets:delete(Name, Key).

get({Name, Bucket}) ->
	NewPath = ?PATH ++ "/" ++ Name ++ ".db",
        {ok, Name}  = dets:open_file(Name,[{file, NewPath},{type, duplicate_bag}]),
	Data = dets:lookup(Name, bucket),
	io:format("reciving data: ~p~n", [Data]),
	ok = dets:close( Name ).
