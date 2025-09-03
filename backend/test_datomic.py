import os
import pytest
from datetime import datetime, timedelta
from backend.datomic_client import DatomicClient

pytestmark = pytest.mark.datomic

@pytest.mark.skipif(
    not os.getenv("DATOMIC_BASE_URL"),
    reason="Datomic not configured; set DATOMIC_BASE_URL to run this test"
)
def test_datomic_operations():
    """Tests basic operations with Datomic."""
    client = DatomicClient()
    assert client.connect(), "Failed to connect to Datomic"

    try:
        emotions = ["радость", "грусть", "гнев", "страх", "удивление", "доверие"]
        user_id = "test-user-1"

        for i, emotion in enumerate(emotions):
            timestamp = datetime.utcnow() - timedelta(days=len(emotions) - i)
            intensity = 0.1 + (i * 0.15)
            if intensity > 1.0:
                intensity = 1.0
            client.add_emotion_entry(
                user_id=user_id,
                emotion=emotion,
                intensity=intensity,
                timestamp=timestamp,
            )

        history = client.get_emotion_history(user_id)
        assert len(history) >= len(emotions)

    finally:
        client.close()
