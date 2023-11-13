copy_node_labels
=====

An OTP application

Build
-----

    $ rebar3 compile


```sh
kubectl get pods -l app=nginx -o name | xargs kubectl delete #del-nginx
kubectl get pods -l app=copy-node-labels -o name | xargs kubectl delete #del-copy
kubectl get pods -l app=copy-node-labels -o name | xargs kubectl logs #logs-copy
```
