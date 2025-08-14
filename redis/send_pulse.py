import json
from datetime import datetime

import redis

# Подключаемся к Redis
r = redis.Redis(host="localhost", port=6379, decode_responses=True)

# Записываем пульс
user_id = "bro123"
pulse = 74
timestamp = datetime.utcnow().isoformat()

key = f"pulse:{user_id}:{timestamp}"
value = json.dumps({"user_id": user_id, "pulse": pulse, "timestamp": timestamp})

# Записываем с TTL = 60 секунд
r.setex(key, 60, value)

# Проверка
print("✅ Записано:", r.get(key))
