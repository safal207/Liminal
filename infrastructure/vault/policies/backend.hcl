# Политика для backend сервиса
path "secret/data/liminal/backend/*" {
  capabilities = ["read", "list"]
}

path "secret/data/liminal/shared/*" {
  capabilities = ["read", "list"]
}

path "auth/token/create" {
  capabilities = ["create", "read", "update"]
}
