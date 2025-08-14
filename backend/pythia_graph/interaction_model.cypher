// Добавляем недостающие концепты
CREATE (:Concept {name: 'стыд', lang: 'ru'}),
       (:Concept {name: 'принятие', lang: 'ru'}),
       (:Concept {name: 'гордость', lang: 'ru'}),
       (:Concept {name: 'shame', lang: 'en'}),
       (:Concept {name: 'acceptance', lang: 'en'}),
       (:Concept {name: 'pride', lang: 'en'});

// Связываем русские концепты
MATCH (a:Concept {name: 'стыд', lang: 'ru'}), (b:Concept {name: 'принятие', lang: 'ru'})
CREATE (a)-[:RELATES_TO]->(b);

MATCH (a:Concept {name: 'принятие', lang: 'ru'}), (b:Concept {name: 'гордость', lang: 'ru'})
CREATE (a)-[:RELATES_TO]->(b);

// Связываем переводы
MATCH (ru:Concept {name: 'стыд', lang: 'ru'}), (en:Concept {name: 'shame', lang: 'en'})
CREATE (ru)-[:TRANSLATES_TO]->(en);

MATCH (ru:Concept {name: 'принятие', lang: 'ru'}), (en:Concept {name: 'acceptance', lang: 'en'})
CREATE (ru)-[:TRANSLATES_TO]->(en);

MATCH (ru:Concept {name: 'гордость', lang: 'ru'}), (en:Concept {name: 'pride', lang: 'en'})
CREATE (ru)-[:TRANSLATES_TO]->(en);
