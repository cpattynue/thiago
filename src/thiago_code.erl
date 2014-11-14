-module(thiago_code).

-export([create/1, update/1, delete/1, get/1 ]).

create({Name, Data}) ->
	{ok, Path} = application:get_env(thiago, path),
	NewPath = Path ++ "/" ++ Name ++ ".db",
	{ok, Name} = dets:open_file(Name,[{file, NewPath},{type, duplicate_bag}]),
	ok = dets:insert( Name, Data),
	ok = dets:close( Name ). 
	
update({Name, Objects}) ->
	{ok, Path} = application:get_env(thiago, path),
        NewPath = Path ++ "/" ++ Name ++ ".db",
	{ok, Name} = dets:open_file(Name,[{file, NewPath},{type, duplicate_bag}]),
        ok = dets:insert(Name, Objects),
	ok = dets:close( Name ).
	

delete({all, Name}) ->
 	{ok, Path} = application:get_env(thiago, path),
        NewPath = Path ++ "/" ++ Name ++ ".db",
        {ok, Name}  = dets:open_file(Name,[{file, NewPath},{type, duplicate_bag}]),
	dets:delete_all_objects(Name);

delete({Name, Key}) ->
	{ok, Path} = application:get_env(thiago, path),
        NewPath = Path ++ "/" ++ Name ++ ".db",
        {ok, Name}  = dets:open_file(Name,[{file, NewPath},{type, duplicate_bag}]),
        dets:delete_object(Name, Key).

get({Name, Key}) ->
	{ok, Path} = application:get_env(thiago, path),
        NewPath = Path ++ "/" ++ Name ++ ".db",
        {ok, Name}  = dets:open_file(Name,[{file, NewPath},{type, duplicate_bag}]),
	Data=dets:lookup(Name, Key),
	io:format("~p~n", [Data]),
	ok = dets:close( Name ).
