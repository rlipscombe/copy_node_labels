-module(mutate_h).
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
            <<"uid">> := Uid,
            <<"object">> := #{
                <<"spec">> := #{<<"nodeName">> := NodeName},
                <<"metadata">> := #{<<"annotations">> := Annotations}
            }
        }
    },
    Req,
    Opts
) ->
    ?LOG_INFO("Have nodeName: ~s", [NodeName]),
    ?LOG_INFO("Have annotations: ~p", [Annotations]),

    % Get our annotation from the pod, so we know what labels/annotations to copy from the node.

    % TODO: Or just get _an_ annotation, so we know to apply these particular labels.
    % e.g. differentpla.net/copy-node-labels: topology

    Node = get_node(NodeName),
    #{<<"metadata">> := #{<<"labels">> := Labels}} = Node,
    #{
        <<"topology.kubernetes.io/region">> := Region,
        <<"topology.kubernetes.io/zone">> := Zone
    } = Labels,

    % You can't patch labels during pod/status updates, which means we can only add annotations.
    % Because the things that need copying are listed in the annotations, we can assume that the annotations object is present.
    Patch = [
        #{
            <<"op">> => <<"add">>,
            <<"path">> => <<"/metadata/annotations/topology.kubernetes.io~1region">>,
            <<"value">> => Region
        },
        #{
            <<"op">> => <<"add">>,
            <<"path">> => <<"/metadata/annotations/topology.kubernetes.io~1zone">>,
            <<"value">> => Zone
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

get_node(NodeName) when is_binary(NodeName) ->
    k8s_api_call(<<"/api/v1/nodes/", NodeName/binary>>).

k8s_api_call(Path) ->
    Host = os:getenv("KUBERNETES_SERVICE_HOST"),
    Port = os:getenv("KUBERNETES_SERVICE_PORT"),
    Server = io_lib:format("https://~s:~s", [Host, Port]),
    Qs = [],
    Url = hackney_url:make_url(Server, Path, Qs),

    {ok, Token} = file:read_file("/var/run/secrets/kubernetes.io/serviceaccount/token"),
    Headers = [
        {<<"accept">>, <<"application/json">>},
        {<<"authorization">>, <<"Bearer ", Token/binary>>}
    ],
    CACertFile = "/var/run/secrets/kubernetes.io/serviceaccount/ca.crt",
    {ok, 200, _, Json} = hackney:get(
        Url,
        Headers,
        <<>>,
        [
            with_body,
            {ssl_options, [
                {verify, verify_peer},
                {cacertfile, CACertFile}
            ]}
        ]
    ),
    jsx:decode(Json).
