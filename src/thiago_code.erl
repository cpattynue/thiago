-module(thiago_code).

-export([create/1, update/1, delete/1, get/1 ]).
-include("thiago.hrl").

create({Name, Data}) ->
	io:format("create ~p~n", [?PATH]),
	NewPath = ?PATH ++ "/" ++ Name,
	{ok,Name} = dets:open_file(Name,[{file, NewPath},{type, duplicate_bag}]),
	ok =dets:insert(NewPath, [{bucket, key}, {notification, [key1, key2, key3]}]).	
update(Args) ->
        io:format("update ~p~n", [Args]).
delete(Args) ->
        io:format("delete ~p~n", [Args]).
get(Args) ->
        io:format("get  ~p~n", [Args]).
