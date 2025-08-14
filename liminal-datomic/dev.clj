(ns dev
  (:require [datomic.client.api :as d]))

(def client
  (d/client {:server-type :datomic-local
             :system "liminal-system"}))

(def db-name "liminal")

(when-not (some #(= db-name %) (d/list-databases client {}))
  (d/create-database client {:db-name db-name})
  (println "✅ База создана"))

(def conn (d/connect client {:db-name db-name}))

(def db (d/db conn))

(println "✅ Подключено к базе:" db-name)

;; загрузка схемы в текущую базу
(def schema
  (read-string (slurp "resources/schema.edn")))

@(d/transact conn {:tx-data schema})

(println "✅ схема:")