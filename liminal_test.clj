(require '[datomic.client.api :as d])

(def client
  (d/client {:server-type :datomic-local
             :system "dev"
             :storage-dir :mem})) ; для памяти

(println "✅ Клиент готов")

(def db-name "liminal-memory")

(try
  (d/create-database client {:db-name db-name})
  (println "✅ База создана")
  (catch Exception _ (println "⚠️ База уже существует")))

(def conn (d/connect client {:db-name db-name}))
(println "✅ База подключена")

(d/transact conn {:tx-data
  [{:db/ident :concept/name
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Имя концепта"
    :db.install/_attribute :db.part/db}

   {:db/ident :concept/lang
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Язык"
    :db.install/_attribute :db.part/db}]})

(d/transact conn {:tx-data
  [{:concept/name "страх" :concept/lang "ru"}
   {:concept/name "надежда" :concept/lang "ru"}
   {:concept/name "fear"   :concept/lang "en"}
   {:concept/name "hope"   :concept/lang "en"}]})

(println "\n🔍 Концепты:")
(d/q '[:find ?name ?lang
       :where [?c :concept/name ?name]
              [?c :concept/lang ?lang]]
     (d/db conn))

(d/transact conn {:tx-data
  [{:db/ident :concept/relates-to
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many
    :db/doc "Связь между концептами"
    :db.install/_attribute :db.part/db}

   {:db/ident :concept/translates-to
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/doc "Перевод концепта"
    :db.install/_attribute :db.part/db}]})

(def db (d/db conn))

(defn eid [name lang]
  (ffirst
    (d/q '[:find ?e
           :in $ ?name ?lang
           :where [?e :concept/name ?name]
                  [?e :concept/lang ?lang]]
         db name lang)))

(def страх (eid "страх" "ru"))
(def надежда (eid "надежда" "ru"))
(def fear   (eid "fear" "en"))
(def hope   (eid "hope" "en"))

(d/transact conn {:tx-data
  [[:db/add страх :concept/relates-to надежда]
   [:db/add страх :concept/translates-to fear]
   [:db/add надежда :concept/translates-to hope]]})

(println "\n🔗 Связи от 'страх':")
(d/q '[:find ?to-name
       :in $ ?from
       :where [?from :concept/relates-to ?to]
              [?to :concept/name ?to-name]]
     db страх)
