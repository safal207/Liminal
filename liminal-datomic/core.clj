(ns liminal.core
  (:require [datomic.client.api :as d]))

(def client
  (d/client {:server-type :datomic-local
             :system "liminal-system"}))

(def db-name "liminal")

(when-not (some #(= db-name %) (d/list-databases client {}))
  (d/create-database client {:db-name db-name}))

(def conn (d/connect client {:db-name db-name}))

(println "✅ Подключено к базе:" db-name)

(defn schema-already-loaded? [conn]
  (not-empty (d/q '[:find ?e :where [?e :db/ident :concept/name]]
                  (d/db conn))))

(defn load-schema [client db-name]
  (let [conn (d/connect client {:db-name db-name})
        schema (read-string (slurp "resources/schema.edn"))]
    (d/transact conn {:tx-data schema})
    (println "✅ Схема загружена")))

(when-not (schema-already-loaded? conn)
  (load-schema client db-name))
