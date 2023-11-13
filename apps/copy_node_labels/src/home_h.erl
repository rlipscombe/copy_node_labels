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

    Host = os:getenv("KUBERNETES_SERVICE_HOST"),
    Port = os:getenv("KUBERNETES_SERVICE_PORT"),
    Url = io_lib:format("https://~s:~s/api/v1/nodes/~s", [Host, Port, NodeName]),

    {ok, Token} = file:read_file("/var/run/secrets/kubernetes.io/serviceaccount/token"),
    Headers = [
        {<<"accept">>, <<"application/json">>},
        {<<"authorization">>, <<"Bearer ", Token/binary>>}
    ],
    CACertFile = "/var/run/secrets/kubernetes.io/serviceaccount/ca.crt",
    {ok, 200, _, _NodeJson} = hackney:get(
        Url,
        Headers,
        <<>>,
        [
            with_body,
            {ssl_options, [{verify, verify_peer}, {cacertfile, CACertFile}]}
        ]
    ),

    % TODO: How do I know what to copy to the pod? Do I need to parse the labels/annotations on the _pod_, to know whether to populate them from the node?
    % Or do I put something in _my_ config to control it? That'd be better than abusing labels/annotations.
    % Maybe: an annotation on the pod to _enable_ copying. Then copy the configured stuff.
    % Could support multiple configurations by annotation label.
    % That could either point to a configuration file section, or to a completely different instance of _this_ "thing".

    % You can't patch labels during pod/status updates, which means we can only add annotations.

    % You can't add to non-existent objects, so if there's no annotation object, we need to create it.
    % If there _is_ one, we need to be careful not to wipe it.
    Patch = [
        #{
            <<"op">> => <<"add">>,
            <<"path">> => <<"/metadata/annotations">>,
            <<"value">> => #{}
        },
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
