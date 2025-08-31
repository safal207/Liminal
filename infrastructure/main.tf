terraform {
  required_providers {
    kubernetes = {
      source = "hashicorp/kubernetes"
      version = "~> 2.23.0"
    }
    helm = {
      source = "hashicorp/helm"
      version = "~> 2.11.0"
    }
  }
}

provider "kubernetes" {
  config_path = "~/.kube/config"
}

provider "helm" {
  kubernetes {
    config_path = "~/.kube/config"
  }
}

# Neo4j deployment
resource "kubernetes_deployment" "neo4j" {
  metadata {
    name = "neo4j"
  }
  spec {
    replicas = 1
    selector {
      match_labels = {
        app = "neo4j"
      }
    }
    template {
      metadata {
        labels = {
          app = "neo4j"
        }
      }
      spec {
        container {
          name = "neo4j"
          image = "neo4j:5.12.0"
          env {
            name = "NEO4J_AUTH"
            value_from {
              secret_key_ref {
                name = "neo4j-secrets"
                key = "auth"
              }
            }
          }
          port {
            container_port = 7474
          }
          port {
            container_port = 7687
          }
          volume_mount {
            name = "neo4j-data"
            mount_path = "/data"
          }
        }
        volume {
          name = "neo4j-data"
          persistent_volume_claim {
            claim_name = "neo4j-pvc"
          }
        }
      }
    }
  }
}

# Redis deployment
resource "kubernetes_deployment" "redis" {
  metadata {
    name = "redis"
  }
  spec {
    replicas = 1
    selector {
      match_labels = {
        app = "redis"
      }
    }
    template {
      metadata {
        labels = {
          app = "redis"
        }
      }
      spec {
        container {
          name = "redis"
          image = "redis:7.2"
          port {
            container_port = 6379
          }
          volume_mount {
            name = "redis-data"
            mount_path = "/data"
          }
        }
        volume {
          name = "redis-data"
          persistent_volume_claim {
            claim_name = "redis-pvc"
          }
        }
      }
    }
  }
}

# Monitoring stack
resource "helm_release" "prometheus" {
  name = "prometheus"
  repository = "https://prometheus-community.github.io/helm-charts"
  chart = "kube-prometheus-stack"
  namespace = "monitoring"
  create_namespace = true
  
  set {
    name = "grafana.enabled"
    value = "true"
  }
}
