-module(thiago_code).

-export([create/1, update/1, delete/1, get/1 ]).

create(Args) ->
	io:format("create ~p~n", [Args]).
update(Args) ->
        io:format("update ~p~n", [Args]).
delete(Args) ->
        io:format("delete ~p~n", [Args]).
get(Args) ->
        io:format("get  ~p~n", [Args]).
