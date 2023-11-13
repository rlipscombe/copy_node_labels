-module(copy_node_labels_app).
-behaviour(application).
-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/mutate", mutate_h, []}
        ]}
    ]),
    {ok, _} = cowboy:start_tls(
        https,
        [
            {port, 8443},
            {cacertfile, "/certs/ca.crt"},
            {certfile, "/certs/tls.crt"},
            {keyfile, "/certs/tls.key"}
        ],
        #{env => #{dispatch => Dispatch}}
    ),
    copy_node_labels_sup:start_link().

stop(_State) ->
    ok.
