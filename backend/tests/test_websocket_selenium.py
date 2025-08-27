import time

import pytest
from selenium import webdriver
from selenium.common.exceptions import TimeoutException
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from webdriver_manager.chrome import ChromeDriverManager


class TestWebSocketSelenium:
    """
    Selenium тесты для проверки WebSocket функционала через браузер.
    Тестируют страницу timeline.html с Memory Timeline.
    """

    @pytest.fixture(scope="class")
    def driver(self):
        """Настройка и создание WebDriver."""
        chrome_options = Options()
        # Раскомментируйте для headless режима (без открытия браузера)
        # chrome_options.add_argument("--headless")
        chrome_options.add_argument("--no-sandbox")
        chrome_options.add_argument("--disable-dev-shm-usage")

        driver = webdriver.Chrome(
            service=Service(ChromeDriverManager().install()), options=chrome_options
        )
        driver.implicitly_wait(10)
        yield driver
        driver.quit()

    def test_page_loads_successfully(self, driver):
        """Тест: страница загружается успешно."""
        driver.get("http://127.0.0.1:8000/static/timeline.html")

        # Проверяем, что заголовок страницы корректный
        assert "LIMINAL Memory Timeline" in driver.title

        # Проверяем наличие основных элементов
        timeline = driver.find_element(By.ID, "timeline")
        assert timeline.is_displayed()

        form = driver.find_element(By.ID, "memoryForm")
        assert form.is_displayed()

    def test_websocket_connection_established(self, driver):
        """Тест: WebSocket соединение устанавливается."""
        driver.get("http://127.0.0.1:8000/static/timeline.html")

        # Ждём подключения к WebSocket
        wait = WebDriverWait(driver, 10)

        try:
            # Ждём, пока текст изменится с "Подключение к временной шкале..."
            wait.until(
                lambda d: "Подключение к временной шкале..."
                not in d.find_element(By.ID, "timeline").text
            )

            timeline_text = driver.find_element(By.ID, "timeline").text

            # Проверяем, что соединение установлено
            assert (
                "Подключено к временной шкале" in timeline_text
                or "Нет данных в таймлайне" in timeline_text
            )

        except TimeoutException:
            pytest.fail("WebSocket соединение не было установлено в течение 10 секунд")

    def test_add_memory_form_submission(self, driver):
        """Тест: отправка формы добавления воспоминания."""
        driver.get("http://127.0.0.1:8000/static/timeline.html")

        # Ждём подключения
        wait = WebDriverWait(driver, 10)
        wait.until(
            lambda d: "Подключение к временной шкале..."
            not in d.find_element(By.ID, "timeline").text
        )

        # Находим элементы формы
        memory_type_input = driver.find_element(By.ID, "memoryType")
        memory_content_input = driver.find_element(By.ID, "memoryContent")
        submit_button = driver.find_element(By.CSS_SELECTOR, "button[type='submit']")

        # Заполняем форму
        test_type = "selenium_test"
        test_content = "Это тестовое воспоминание от Selenium"

        memory_type_input.clear()
        memory_type_input.send_keys(test_type)

        memory_content_input.clear()
        memory_content_input.send_keys(test_content)

        # Отправляем форму
        submit_button.click()

        # Ждём, пока форма очистится (признак успешной отправки)
        wait.until(
            lambda d: d.find_element(By.ID, "memoryType").get_attribute("value") == ""
        )

        # Проверяем, что поля формы очистились
        assert memory_type_input.get_attribute("value") == ""
        assert memory_content_input.get_attribute("value") == ""

    def test_memory_appears_in_timeline(self, driver):
        """Тест: добавленное воспоминание появляется в таймлайне."""
        driver.get("http://127.0.0.1:8000/static/timeline.html")

        # Ждём подключения
        wait = WebDriverWait(driver, 10)
        wait.until(
            lambda d: "Подключение к временной шкале..."
            not in d.find_element(By.ID, "timeline").text
        )

        # Запоминаем количество воспоминаний до добавления
        timeline = driver.find_element(By.ID, "timeline")
        memories_before = len(timeline.find_elements(By.CLASS_NAME, "memory"))

        # Добавляем новое воспоминание
        memory_type_input = driver.find_element(By.ID, "memoryType")
        memory_content_input = driver.find_element(By.ID, "memoryContent")
        submit_button = driver.find_element(By.CSS_SELECTOR, "button[type='submit']")

        test_type = "selenium_timeline_test"
        test_content = "Проверка появления в таймлайне"

        memory_type_input.send_keys(test_type)
        memory_content_input.send_keys(test_content)
        submit_button.click()

        # Ждём появления нового воспоминания в таймлайне
        try:
            wait.until(
                lambda d: len(
                    d.find_element(By.ID, "timeline").find_elements(
                        By.CLASS_NAME, "memory"
                    )
                )
                > memories_before
            )

            # Проверяем, что воспоминание появилось
            memories_after = len(timeline.find_elements(By.CLASS_NAME, "memory"))
            assert memories_after > memories_before

            # Проверяем содержимое последнего добавленного воспоминания
            latest_memory = timeline.find_element(By.CLASS_NAME, "memory")
            memory_text = latest_memory.text

            assert test_type in memory_text
            assert test_content in memory_text

        except TimeoutException:
            pytest.fail(
                "Новое воспоминание не появилось в таймлайне в течение 10 секунд"
            )

    def test_websocket_real_time_updates(self, driver):
        """Тест: проверка real-time обновлений через WebSocket."""
        driver.get("http://127.0.0.1:8000/static/timeline.html")

        # Ждём подключения
        wait = WebDriverWait(driver, 10)
        wait.until(
            lambda d: "Подключение к временной шкале..."
            not in d.find_element(By.ID, "timeline").text
        )

        # Добавляем воспоминание
        memory_type_input = driver.find_element(By.ID, "memoryType")
        memory_content_input = driver.find_element(By.ID, "memoryContent")
        submit_button = driver.find_element(By.CSS_SELECTOR, "button[type='submit']")

        test_content = f"Real-time test {int(time.time())}"

        memory_type_input.send_keys("realtime_test")
        memory_content_input.send_keys(test_content)
        submit_button.click()

        # Ждём, пока воспоминание появится в таймлайне через WebSocket
        try:
            wait.until(lambda d: test_content in d.find_element(By.ID, "timeline").text)

            timeline_text = driver.find_element(By.ID, "timeline").text
            assert test_content in timeline_text

        except TimeoutException:
            pytest.fail("Real-time обновление через WebSocket не сработало")

    def test_console_logs_for_websocket(self, driver):
        """Тест: проверка консольных логов WebSocket."""
        driver.get("http://127.0.0.1:8000/static/timeline.html")

        # Ждём подключения
        time.sleep(3)

        # Получаем логи браузера
        logs = driver.get_log("browser")

        # Проверяем, что есть логи о подключении к WebSocket
        websocket_logs = [
            log
            for log in logs
            if "WebSocket" in log["message"] or "Connected" in log["message"]
        ]

        # Должен быть хотя бы один лог о WebSocket
        assert len(websocket_logs) > 0, "Не найдено логов о WebSocket подключении"

    def test_form_validation(self, driver):
        """Тест: валидация формы (обязательные поля)."""
        driver.get("http://127.0.0.1:8000/static/timeline.html")

        # Пытаемся отправить пустую форму
        submit_button = driver.find_element(By.CSS_SELECTOR, "button[type='submit']")
        submit_button.click()

        # Проверяем, что браузер показывает валидационные ошибки
        memory_type_input = driver.find_element(By.ID, "memoryType")
        memory_content_input = driver.find_element(By.ID, "memoryContent")

        # Поля должны быть невалидными (HTML5 validation)
        assert not memory_type_input.get_attribute("validity").get("valid", True)
        assert not memory_content_input.get_attribute("validity").get("valid", True)


if __name__ == "__main__":
    # Запуск тестов напрямую
    pytest.main([__file__, "-v"])
