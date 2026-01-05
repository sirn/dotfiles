yaml-mode yaml-ts-mode

(k8s-deploy "Kubernetes Deployment"
 "apiVersion: apps/v1" n
 "kind: Deployment" n
 "metadata:" n>
 "name: " (p "name" name) n>
 "namespace: " (p "default") n
 "spec:" n>
 "replicas: " (p "1") n>
 "selector:" n>
 "matchLabels:" n>
 "app: " (s name) n>
 "template:" n>
 "metadata:" n>
 "labels:" n>
 "app: " (s name) n>
 "spec:" n>
 "containers:" n>
 "- name: " (s name) n>
 "image: " (p "image") ":" (p "tag") n>
 "ports:" n>
 "- containerPort: " (p "80") q)
