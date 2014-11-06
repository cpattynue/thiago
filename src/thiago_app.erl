%%
%% Copyright (C) 2014
%% Authors: Patricia Nuñez cpattynue@gmail.com<>
%% All rights reserved.
%%
%%
%%
-module(thiago_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    thiago_sup:start_link().

stop(_State) ->
    ok.
