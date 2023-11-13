-module(home_h).
-export([init/2]).

-include_lib("kernel/include/logger.hrl").

init(Req1, Opts) ->
    <<"POST">> = cowboy_req:method(Req1),

    {ok, ReqJson, Req2} = cowboy_req:read_body(Req1),
    Request = jsx:decode(ReqJson),
    ?LOG_DEBUG(#{request => Request}),

    #{<<"apiVersion">> := <<"admission.k8s.io/v1">>, <<"kind">> := <<"AdmissionReview">>} = Request,
    #{<<"request">> := #{<<"name">> := Name, <<"namespace">> := Namespace}} = Request,
    ?LOG_INFO(#{name => Name, namespace => Namespace}),

    handle_request(Request, Req2, Opts).

handle_request(
    _Request = #{
        <<"request">> := #{
            <<"uid">> := Uid, <<"object">> := #{<<"spec">> := #{<<"nodeName">> := NodeName}}
        }
    },
    Req,
    Opts
) ->
    ?LOG_INFO("Have nodeName: ~s", [NodeName]),

    % TODO: Go and get the node's labels/annotations.

    % TODO: If I reject it, what then? This would prove that we've still got time to add the labels/annotations...?

    ResBody = jsx:encode(#{
        <<"apiVersion">> => <<"admission.k8s.io/v1">>,
        <<"kind">> => <<"AdmissionReview">>,
        <<"response">> => #{
            <<"uid">> => Uid,
            <<"allowed">> => false
        }
    }),

    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, ResBody, Req),
    {ok, Req2, Opts};
handle_request(_Request = #{<<"request">> := #{<<"uid">> := Uid}}, Req, Opts) ->
    ResBody = jsx:encode(#{
        <<"apiVersion">> => <<"admission.k8s.io/v1">>,
        <<"kind">> => <<"AdmissionReview">>,
        <<"response">> => #{
            <<"uid">> => Uid,
            <<"allowed">> => true
        }
    }),

    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, ResBody, Req),
    {ok, Req2, Opts}.
