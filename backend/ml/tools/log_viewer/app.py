#!/usr/bin/env python
"""
Панель анализа интеллектуальных логов адаптера OpenAI

Веб-приложение для визуализации и анализа:
1. Experience logs (опыт/ошибки)
2. Insight logs (инсайты/решения)
3. Karma logs (повторяющиеся ошибки)
4. Hypotheses logs (гипотезы)

С возможностью временной визуализации и обнаружения паттернов.
"""

import hashlib
import json
import os
import re
import time
from collections import defaultdict
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Optional

from endocrine import thyroid
from flask import (
    Flask,
    jsonify,
    redirect,
    render_template,
    request,
    send_from_directory,
    url_for,
)

# Для тестирования создаем заглушку вместо adapter_logger
# В production версии здесь будет импорт реального adapter_logger
print("Запуск панели анализа логов в тестовом режиме")

# Инициализация Flask приложения
app = Flask(__name__, static_url_path="/static")


class LogAnalyzer:
    """Класс для анализа и обработки интеллектуальных логов"""

    def __init__(self, log_dir=None):
        """Инициализация анализатора логов"""
        self.log_dir = log_dir or str(Path(__file__).parent / "logs" / "adapter")
        self.log_files = {
            "experience": os.path.join(self.log_dir, "experience.log"),
            "insights": os.path.join(self.log_dir, "insights.log"),
            "karma": os.path.join(self.log_dir, "karma.log"),
            "hypotheses": os.path.join(self.log_dir, "hypotheses.log"),
            "dialogues": os.path.join(self.log_dir, "dialogues.log"),
        }

    def get_log_stats(self):
        """Получает основную статистику по логам"""
        stats = {}

        for log_type, log_path in self.log_files.items():
            if os.path.exists(log_path):
                size = os.path.getsize(log_path)
                entries = self._count_log_entries(log_path)
                stats[log_type] = {
                    "exists": True,
                    "size": size,
                    "entries": entries,
                    "last_modified": datetime.fromtimestamp(
                        os.path.getmtime(log_path)
                    ).strftime("%Y-%m-%d %H:%M:%S"),
                }
            else:
                stats[log_type] = {
                    "exists": False,
                    "size": 0,
                    "entries": 0,
                    "last_modified": "N/A",
                }

        return stats

    def get_origin_stats(self):
        """Считает распределение событий по полю origin (external/internal/relay/unknown)"""
        counts = {"external": 0, "internal": 0, "relay": 0, "unknown": 0}
        for lt in self.log_files.keys():
            entries = self.parse_logs(lt)
            for e in entries:
                origin = e.get("origin", "unknown")
                if origin not in counts:
                    origin = "unknown"
                counts[origin] += 1
        return counts

    def _count_log_entries(self, log_path):
        """Подсчитывает количество записей в логе (по количеству JSON блоков)"""
        try:
            with open(log_path, "r", encoding="utf-8") as f:
                content = f.read()
                # Ищем все JSON блоки (они между ```)
                json_blocks = re.findall(r"```\n(.*?)\n```", content, re.DOTALL)
                return len(json_blocks)
        except Exception as e:
            print(f"Ошибка при подсчете записей в {log_path}: {e}")
            return 0

    def parse_logs(self, log_type):
        """Парсит файл логов указанного типа и возвращает структурированные данные"""
        log_path = self.log_files.get(log_type)
        if not log_path or not os.path.exists(log_path):
            return []

        try:
            with open(log_path, "r", encoding="utf-8") as f:
                content = f.read()
                # Находим все JSON блоки и их заголовки
                entries = []

                # Ищем заголовки (## Ошибка или ## Инсайт и т.д.)
                headers = re.findall(r"## (.*?) \((.*?)\)", content)
                # Ищем JSON блоки
                json_blocks = re.findall(r"```\n(.*?)\n```", content, re.DOTALL)

                # Объединяем заголовки и JSON
                for i, json_str in enumerate(json_blocks):
                    try:
                        data = json.loads(json_str)
                        # Добавляем тип лога
                        data["log_type"] = log_type
                        entries.append(data)
                    except json.JSONDecodeError:
                        print(f"Ошибка при декодировании JSON в {log_path}")

                return sorted(
                    entries, key=lambda x: x.get("timestamp", ""), reverse=True
                )
        except Exception as e:
            print(f"Ошибка при парсинге файла {log_path}: {e}")
            return []

    def get_all_logs(self):
        """Получает все логи всех типов в хронологическом порядке"""
        all_logs = []
        for log_type in self.log_files.keys():
            logs = self.parse_logs(log_type)
            all_logs.extend(logs)

        # Сортируем по времени
        return sorted(all_logs, key=lambda x: x.get("timestamp", ""), reverse=True)

    def find_patterns(self):
        """Находит паттерны в логах - повторяющиеся ошибки, связи между проблемами и решениями"""
        patterns = {
            "repeating_errors": [],
            "problem_solution_pairs": [],
            "unresolved_issues": [],
            "hypotheses_status": [],
        }

        # Анализируем карму для повторяющихся ошибок
        karma_logs = self.parse_logs("karma")
        if karma_logs:
            # Группируем по хешу ошибки и считаем повторения
            error_counts = {}
            for log in karma_logs:
                error_hash = log.get("error_hash", "")
                if error_hash:
                    error_counts[error_hash] = max(
                        error_counts.get(error_hash, 0), log.get("count", 0)
                    )

            # Формируем список повторяющихся ошибок
            for error_hash, count in sorted(
                error_counts.items(), key=lambda x: x[1], reverse=True
            ):
                matching_logs = [
                    log for log in karma_logs if log.get("error_hash") == error_hash
                ]
                if matching_logs:
                    latest_log = sorted(
                        matching_logs,
                        key=lambda x: x.get("timestamp", ""),
                        reverse=True,
                    )[0]
                    patterns["repeating_errors"].append(
                        {
                            "error_hash": error_hash,
                            "error_message": latest_log.get("error_message", ""),
                            "count": count,
                            "has_insight": latest_log.get("has_insight", False),
                            "last_seen": latest_log.get("timestamp", ""),
                        }
                    )

        # Находим пары проблема-решение
        experience_logs = self.parse_logs("experience")
        insights_logs = self.parse_logs("insights")

        # Строим словарь инсайтов по хешу ошибки
        insights_by_hash = {}
        for log in insights_logs:
            error_hash = log.get("error_hash", "")
            if error_hash:
                insights_by_hash[error_hash] = log

        # Для каждой ошибки проверяем, есть ли решение
        for exp_log in experience_logs:
            error_hash = exp_log.get("error_hash", "")
            if error_hash and error_hash in insights_by_hash:
                patterns["problem_solution_pairs"].append(
                    {
                        "error_hash": error_hash,
                        "error_message": exp_log.get("error_message", ""),
                        "solution": insights_by_hash[error_hash].get("solution", ""),
                        "error_time": exp_log.get("timestamp", ""),
                        "solution_time": insights_by_hash[error_hash].get(
                            "timestamp", ""
                        ),
                    }
                )
            elif error_hash:
                # Ошибки без решений
                patterns["unresolved_issues"].append(
                    {
                        "error_hash": error_hash,
                        "error_message": exp_log.get("error_message", ""),
                        "timestamp": exp_log.get("timestamp", ""),
                    }
                )

        # Анализируем гипотезы
        hypotheses_logs = self.parse_logs("hypotheses")
        # Группируем по issue
        hypothesis_by_issue = defaultdict(list)
        for log in hypotheses_logs:
            issue = log.get("issue", "")
            if issue:
                hypothesis_by_issue[issue].append(log)

        # Анализируем статус гипотез
        for issue, logs in hypothesis_by_issue.items():
            # Сортируем по времени
            sorted_logs = sorted(logs, key=lambda x: x.get("timestamp", ""))
            # Последняя запись определяет статус
            latest = sorted_logs[-1]
            patterns["hypotheses_status"].append(
                {
                    "issue": issue,
                    "hypothesis": sorted_logs[0].get("hypothesis", ""),
                    "status": latest.get("status", "open"),
                    "result": latest.get("result", ""),
                    "created": sorted_logs[0].get("timestamp", ""),
                    "last_updated": latest.get("timestamp", ""),
                }
            )

        return patterns

    def get_timeline_data(self):
        """Подготавливает данные для временной оси"""
        all_logs = self.get_all_logs()

        timeline_data = []
        for log in all_logs:
            timestamp = log.get("timestamp", "")
            log_type = log.get("log_type", "")

            item = {
                "timestamp": timestamp,
                "datetime": timestamp,  # Для отображения
                "type": log_type,
            }

            # Добавляем специфические для типа лога данные
            if log_type == "experience":
                item["title"] = "Ошибка"
                item["description"] = log.get("error_message", "")
                item["hash"] = log.get("error_hash", "")
            elif log_type == "insights":
                item["title"] = "Инсайт"
                item["description"] = log.get("solution", "")
                item["hash"] = log.get("error_hash", "")
            elif log_type == "karma":
                item["title"] = f"Карма (повтор #{log.get('count', '?')})"
                item["description"] = log.get("error_message", "")
                item["hash"] = log.get("error_hash", "")
            elif log_type == "hypotheses":
                status = log.get("status", "open")
                if status == "open":
                    item["title"] = "Гипотеза"
                    item["description"] = log.get("hypothesis", "")
                else:
                    item["title"] = f"Результат гипотезы ({status})"
                    item["description"] = log.get("result", "")
                item["hash"] = hashlib.md5(log.get("issue", "").encode()).hexdigest()

            timeline_data.append(item)

        return sorted(timeline_data, key=lambda x: x.get("timestamp", ""))


# Инициализируем анализатор логов
log_analyzer = LogAnalyzer()

# -------------------------------------------------
# Cosmic laws interpretation
# -------------------------------------------------

COSMIC_LAW_KEYS = [
    "law_of_rhythm",
    "law_of_polarity",
    "law_of_vibration",
    "law_of_correspondence",
    "law_of_cause_effect",
    "law_of_attraction",
    "law_of_transformation",
    "law_of_circulation",
    "law_of_unity",
]


def compute_cosmic_scores() -> Dict[str, int]:
    """Простая эвристика для конвертации текущих метрик в оценки (0-100) для 9 законов."""
    scores = {k: 80 for k in COSMIC_LAW_KEYS}  # базовая инициализация

    # 1. Rhythm – баланс ошибок и инсайтов
    stats = log_analyzer.get_log_stats()
    total = sum(v["entries"] for v in stats.values() if v["entries"])
    exp = stats.get("experience", {}).get("entries", 0)
    ins = stats.get("insights", {}).get("entries", 0)
    if total:
        balance = 1 - abs(exp - ins) / total  # 1 при идеальном равновесии
        scores["law_of_rhythm"] = int(balance * 100)

    # 2. Polarity – доля здоровых сервисов (yan) vs проблемных (yin)
    try:
        hb_path = Path(__file__).parent / "logs" / "health" / "heartbeat.log"
        if hb_path.exists():
            content = hb_path.read_text(encoding="utf-8")
            blocks = re.findall(r"```(.*?)```", content, re.DOTALL)
            if blocks:
                last = json.loads(blocks[-1])
                states = last.get("states", {})
                yan = list(states.values()).count("yan")
                total_svc = len(states)
                if total_svc:
                    scores["law_of_polarity"] = int((yan / total_svc) * 100)
    except Exception as e:
        print("cosmic polarity calc error", e)

    # 3. Vibration – отношение karma (повторы) к общим ошибкам
    karma = stats.get("karma", {}).get("entries", 0)
    if exp:
        scores["law_of_vibration"] = max(0, 100 - int((karma / exp) * 100))

    # 4. Correspondence – совпадение внешних и внутренних событий
    origins = log_analyzer.get_origin_stats()
    ext = origins.get("external", 0)
    internal = origins.get("internal", 0)
    if ext + internal:
        diff = abs(ext - internal) / (ext + internal)
        scores["law_of_correspondence"] = int((1 - diff) * 100)

    # 5. Cause-effect – процент закрытых гипотез
    try:
        patterns = log_analyzer.find_patterns()
        hyp = patterns.get("hypotheses_status", [])
        if hyp:
            closed = [h for h in hyp if h["status"] != "open"]
            scores["law_of_cause_effect"] = int((len(closed) / len(hyp)) * 100)
    except Exception as e:
        print("cosmic cause-effect calc error", e)

    # Остальные пока статичны
    return scores


@app.route("/api/thyroid_status")
def api_thyroid_status():
    return jsonify(thyroid.status())


@app.route("/api/cosmic_scores")
def api_cosmic_scores():
    """API возвращает текущие оценки исполнения 9 космических законов (0-100)"""
    return jsonify(compute_cosmic_scores())


@app.route("/")
def index():
    """Главная страница с обзором логов"""
    stats = log_analyzer.get_log_stats()
    patterns = log_analyzer.find_patterns()
    return render_template("index.html", stats=stats, patterns=patterns)


@app.route("/timeline")
def timeline():
    """Страница с временной осью логов"""
    timeline_data = log_analyzer.get_timeline_data()
    return render_template("timeline.html", timeline_data=timeline_data)


@app.route("/logs/<log_type>")
def view_logs(log_type):
    """Просмотр логов определенного типа"""
    if log_type not in [
        "experience",
        "insights",
        "karma",
        "hypotheses",
        "dialogues",
        "all",
    ]:
        return redirect(url_for("index"))

    if log_type == "all":
        logs = log_analyzer.get_all_logs()
        title = "Все логи"
    else:
        logs = log_analyzer.parse_logs(log_type)
        titles = {
            "experience": "Логи опыта (ошибки)",
            "insights": "Логи инсайтов (решения)",
            "karma": "Логи кармы (повторяющиеся ошибки)",
            "hypotheses": "Логи гипотез",
            "dialogues": "Логи диалогов",
        }
        title = titles.get(log_type, log_type)

    return render_template("logs.html", logs=logs, title=title, log_type=log_type)


@app.route("/api/stats")
def api_stats():
    """API для получения статистики по логам"""
    return jsonify(log_analyzer.get_log_stats())


@app.route("/api/logs/<log_type>")
def api_logs(log_type):
    """API для получения логов указанного типа"""
    if log_type not in [
        "experience",
        "insights",
        "karma",
        "hypotheses",
        "dialogues",
        "all",
    ]:
        return jsonify({"error": "Invalid log type"}), 400

    if log_type == "all":
        logs = log_analyzer.get_all_logs()
    else:
        logs = log_analyzer.parse_logs(log_type)

    return jsonify(logs)


@app.route("/api/patterns")
def api_patterns():
    """API для получения паттернов в логах"""
    return jsonify(log_analyzer.find_patterns())


@app.route("/api/timeline")
def api_timeline():
    """API для получения данных временной оси"""
    return jsonify(log_analyzer.get_timeline_data())


@app.route("/api/chart_data/<chart_id>")
def api_chart_data(chart_id):
    """Возвращает данные для графиков с фильтрацией по периоду"""
    period = request.args.get("period", "all")
    analyzer = LogAnalyzer()

    if chart_id == "errorActivityChart":
        # Получаем данные по активности ошибок
        errors = analyzer.parse_logs("experience")
        activity_data = defaultdict(int)

        # Определяем временной период
        end_date = datetime.now()
        if period == "day":
            start_date = end_date - datetime.timedelta(days=1)
        elif period == "week":
            start_date = end_date - datetime.timedelta(weeks=1)
        elif period == "month":
            start_date = end_date - datetime.timedelta(days=30)
        else:  # all
            # Используем самую раннюю дату из логов или последние 90 дней
            earliest = end_date - datetime.timedelta(days=90)
            for error in errors:
                if "timestamp" in error:
                    error_date = datetime.fromisoformat(
                        error["timestamp"].replace("Z", "+00:00")
                    )
                    if error_date < earliest:
                        earliest = error_date
            start_date = earliest

        # Фильтрация и группировка по дням
        for error in errors:
            if "timestamp" in error:
                error_date = datetime.fromisoformat(
                    error["timestamp"].replace("Z", "+00:00")
                )
                if start_date <= error_date <= end_date:
                    date_str = error_date.strftime("%Y-%m-%d")
                    activity_data[date_str] += 1

        # Сортировка по датам
        result = {k: activity_data[k] for k in sorted(activity_data.keys())}
        return jsonify(result)

    elif chart_id == "logDistributionChart":
        # Для распределения типов логов, период не имеет значения, но для совместимости
        stats = analyzer.get_log_stats()
        distribution = {
            "experience": (
                stats["experience"]["entries"] if "experience" in stats else 0
            ),
            "insights": stats["insights"]["entries"] if "insights" in stats else 0,
            "karma": stats["karma"]["entries"] if "karma" in stats else 0,
            "hypotheses": (
                stats["hypotheses"]["entries"] if "hypotheses" in stats else 0
            ),
            "dialogues": stats["dialogues"]["entries"] if "dialogues" in stats else 0,
        }
        return jsonify(distribution)

    return jsonify({"error": "Unknown chart ID"})


@app.route("/api/origin_stats")
def api_origin_stats():
    analyzer = LogAnalyzer()
    return jsonify(analyzer.get_origin_stats())


# Healthmap proxy endpoint
@app.route("/api/healthmap")
def api_healthmap():
    """Возвращает последнюю запись heartbeat от lungs.py, если файл существует"""
    try:
        hb_path = Path(__file__).parent / "logs" / "health" / "heartbeat.log"
        if hb_path.exists():
            content = hb_path.read_text(encoding="utf-8")
            # берем последний JSON блок
            import json
            import re

            blocks = re.findall(r"```(.*?)```", content, re.DOTALL)
            if blocks:
                return jsonify(json.loads(blocks[-1]))
    except Exception as e:
        print("healthmap error", e)
    return jsonify({"error": "no_heartbeat"}), 404


# Health-check endpoint
@app.route("/health")
def health_check():
    """Простой health-check для watchdog-скриптов"""
    return jsonify({"status": "ok"})


# Маршрут для доступа к статическим файлам
@app.route("/static/<path:path>")
def serve_static(path):
    return send_from_directory("static", path)


if __name__ == "__main__":
    app.run(debug=True, host="0.0.0.0", port=5000)
