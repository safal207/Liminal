import json

from neo4j import GraphDatabase

uri = "bolt://localhost:7687"
auth = ("neo4j", "NewStrongPass123!")

driver = GraphDatabase.driver(uri, auth=auth)
with driver.session() as s:
    node_count = s.run("MATCH (n:Node) RETURN count(n) as c").single()["c"]
    edge_count = s.run("MATCH ()-[r:RELATES]->() RETURN count(r) as c").single()["c"]
    nodes = s.run(
        "MATCH (n:Node) RETURN n.id as id, n.kind as kind, n.traits as traits ORDER BY n.created_at DESC LIMIT 10"
    ).data()
    rels = s.run(
        "MATCH (a)-[r:RELATES]->(b) RETURN a.id as source, b.id as target, r.kind as kind, r.weight as weight ORDER BY r.weight DESC LIMIT 10"
    ).data()
    print(
        json.dumps(
            {
                "node_count": int(node_count),
                "edge_count": int(edge_count),
                "nodes": nodes,
                "relations": rels,
            },
            ensure_ascii=False,
        )
    )

driver.close()
