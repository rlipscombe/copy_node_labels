-module(home_h).
-export([init/2]).

-include_lib("kernel/include/logger.hrl").

init(Req1, Opts) ->
    <<"POST">> = cowboy_req:method(Req1),

    {ok, ReqJson, Req2} = cowboy_req:read_body(Req1),
    Request = jsx:decode(ReqJson),
    ?LOG_DEBUG(#{request => Request}),

    % Bail early if it's not an AdmissionReview
    #{<<"apiVersion">> := <<"admission.k8s.io/v1">>, <<"kind">> := <<"AdmissionReview">>} = Request,

    #{
        <<"request">> := #{
            <<"operation">> := Operation, <<"name">> := Name, <<"namespace">> := Namespace
        }
    } = Request,
    ?LOG_INFO("~s ~s (in ~s)", [Operation, Name, Namespace]),

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

    % TODO: Get our annotation from the pod, so we know what labels/annotations to copy from the node.
    

    Host = os:getenv("KUBERNETES_SERVICE_HOST"),
    Port = os:getenv("KUBERNETES_SERVICE_PORT"),
    Url = io_lib:format("https://~s:~s/api/v1/nodes/~s", [Host, Port, NodeName]),

    {ok, Token} = file:read_file("/var/run/secrets/kubernetes.io/serviceaccount/token"),
    Headers = [
        {<<"accept">>, <<"application/json">>},
        {<<"authorization">>, <<"Bearer ", Token/binary>>}
    ],
    CACertFile = "/var/run/secrets/kubernetes.io/serviceaccount/ca.crt",
    {ok, 200, _, NodeJson} = hackney:get(
        Url,
        Headers,
        <<>>,
        [
            with_body,
            {ssl_options, [{verify, verify_peer}, {cacertfile, CACertFile}]}
        ]
    ),

    % You can't patch labels during pod/status updates, which means we can only add annotations.
    % Because the things that need copying are listed in the annotations, we can assume that the annotations object is present.
    Patch = [
        #{
            <<"op">> => <<"add">>,
            <<"path">> => <<"/metadata/annotations/topology.kubernetes.io~1zone">>,
            <<"value">> => <<"eu-west-1a">>
        }
    ],
    ResBody = jsx:encode(#{
        <<"apiVersion">> => <<"admission.k8s.io/v1">>,
        <<"kind">> => <<"AdmissionReview">>,
        <<"response">> => #{
            <<"uid">> => Uid,
            <<"allowed">> => true,
            <<"patchType">> => <<"JSONPatch">>,
            <<"patch">> => base64:encode(jsx:encode(Patch))
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
