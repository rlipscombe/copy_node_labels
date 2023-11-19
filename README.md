# copy_node_labels

A K8s mutating web hook that copies node topology labels to pods scheduled on that node, so they can be accessed in the
downward API.

## Motivation

Some services are "rack-aware", such as Kafka. It's a common convention to use the cloud availability zone (AZ) as a
"rack" identifier.

Unfortunately, this information is only commonly available in node labels, where the region and the zone are referred to as "topology". If a pod wants to discover its region or zone, it must use the Kubernetes API.

It would be better to inject the region and zone values into environment variables when the pod starts.

This project implements a mutating web hook which copies the topology labels from the scheduled node and adds them as
pod annotations. From there, they can be retrieved using the Kubernetes "downward API".

```sh
kubectl get pods -l app=nginx -o name | xargs kubectl delete #del-nginx
kubectl get pods -l app=copy-node-labels -o name | xargs kubectl delete #del-copy
kubectl get pods -l app=copy-node-labels -o name | xargs kubectl logs #logs-copy
```
