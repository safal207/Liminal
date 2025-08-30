import unittest
from datetime import datetime

from rince.rince import RINSE


class TestRINSE(unittest.TestCase):
    def setUp(self):
        self.rince = RINSE()

    def test_process_experience(self):
        """Тест обработки опыта"""
        raw_experience = (
            "Сегодня был сложный день. Много стресса на работе. Но я понял, что могу идти вперёд"
        )
        timestamp = datetime(2025, 7, 16, 15, 39, 28)

        result = self.rince.process_experience(raw_experience, timestamp)

        assert "cleansed" in result
        assert "insight" in result
        assert "tags" in result
        assert "clarity" in result
        assert "timestamp" in result

        # Проверяем, что теги содержат ожидаемые эмоции
        expected_tags = ["страх", "решимость"]
        for tag in expected_tags:
            assert tag in result["tags"]

        # Проверяем, что ясность в правильном диапазоне
        assert result["clarity"] >= 0
        assert result["clarity"] <= 1

    def test_sentiment_analysis(self):
        """Тест анализа настроения"""
        positive_text = "Я счастлив и полон энергии"
        negative_text = "Я чувствую себя ужасно"

        pos_score = self.rince.analyze_sentiment(positive_text)
        neg_score = self.rince.analyze_sentiment(negative_text)

        assert pos_score > neg_score

    def test_emotion_classification(self):
        """Тест классификации эмоций"""
        text = "Я счастлив и полон надежды на будущее"
        emotions = self.rince.classify_emotions(text)

        assert "joy" in emotions
        assert "anticipation" in emotions


if __name__ == "__main__":
    unittest.main()
