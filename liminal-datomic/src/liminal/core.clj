(ns liminal.core
  (:require [datomic.client.api :as d]))

(defn -main []
  (println "✅ Привет из Datomic!")
  (let [client (d/client {:server-type :datomic-local
                          :system "dev"
                          :storage-dir :mem})]
    (println "✅ Клиент создан")
    (d/create-database client {:db-name "liminal"})
    (println "✅ База создана!!!")))

;; запуск командой: clojure -M -m liminal.core
