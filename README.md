copy_node_labels
=====

An OTP application

Build
-----

    $ rebar3 compile


```sh
kubectl get pods -l app=nginx -o name | xargs kubectl delete #del-nginx
```
