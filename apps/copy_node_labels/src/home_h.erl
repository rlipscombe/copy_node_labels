-module(home_h).
-export([init/2]).

-include_lib("kernel/include/logger.hrl").

init(Req0, Opts) ->
    <<"POST">> = cowboy_req:method(Req0),
    {ok, ReqJson} = cowboy_req:read_body(Req0),
    Request = jsx:decode(ReqJson),
    ?LOG_INFO(#{request => Request}),
    #{<<"request">> := #{<<"uid">> := Uid}} = Request,
    ResBody = jsx:encode(#{
        <<"apiVersion">> => <<"admission.k8s.io/v1">>,
        <<"kind">> => <<"AdmissionReview">>,
        <<"response">> => #{
            <<"uid">> => Uid,
            <<"allowed">> => true
        }
    }),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, ResBody, Req0),
    {ok, Req, Opts}.
