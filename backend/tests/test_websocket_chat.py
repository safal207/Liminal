import builtins
import contextlib
import time
from concurrent.futures import ThreadPoolExecutor

import pytest
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import WebDriverWait
from webdriver_manager.chrome import ChromeDriverManager


@pytest.fixture(scope="module")
def driver():
    options = webdriver.ChromeOptions()
    options.add_argument("--headless")  # Для запуска без отображения браузера
    options.add_argument("--no-sandbox")
    options.add_argument("--disable-dev-shm-usage")

    service = Service(ChromeDriverManager().install())
    driver = webdriver.Chrome(service=service, options=options)
    yield driver
    driver.quit()


def create_user_session(user_id):
    options = webdriver.ChromeOptions()
    options.add_argument("--headless")
    options.add_argument("--no-sandbox")
    options.add_argument("--disable-dev-shm-usage")

    service = Service(ChromeDriverManager().install())
    driver = webdriver.Chrome(service=service, options=options)

    try:
        driver.get("http://localhost:8000")

        # Устанавливаем user_id
        user_input = driver.find_element(By.ID, "userId")
        user_input.clear()
        user_input.send_keys(user_id)

        # Подключаемся
        connect_btn = driver.find_element(By.ID, "connect")
        connect_btn.click()

        # Ждем подключения
        WebDriverWait(driver, 5).until(EC.presence_of_element_located((By.CLASS_NAME, "system")))

        # Подписываемся на канал
        channel_input = driver.find_element(By.ID, "channel")
        channel_input.clear()
        channel_input.send_keys("test_channel")

        subscribe_btn = driver.find_element(By.ID, "subscribe")
        subscribe_btn.click()

        time.sleep(1)  # Даем время на подписку

        return driver
    except Exception as e:
        driver.quit()
        raise e


def test_multiple_users_chat():
    users = ["user1", "user2", "user3"]
    drivers = []
    messages = []

    try:
        # Создаем сессии для всех пользователей
        with ThreadPoolExecutor(max_workers=3) as executor:
            drivers = list(executor.map(create_user_session, users))

        # Отправляем сообщение от первого пользователя
        message = "Привет от user1"
        message_input = drivers[0].find_element(By.ID, "message")
        message_input.send_keys(message)
        send_btn = drivers[0].find_element(By.ID, "send")
        send_btn.click()

        # Даем время на доставку сообщений
        time.sleep(2)

        # Проверяем получение сообщения у всех пользователей
        for i, driver in enumerate(drivers):
            messages = driver.find_elements(By.CLASS_NAME, "message")
            last_message = messages[-1].text if messages else ""
            assert "Привет от user1" in last_message, (
                f"Сообщение не доставлено пользователю {users[i]}"
            )

    finally:
        # Закрываем все драйверы
        for driver in drivers:
            with contextlib.suppress(builtins.BaseException):
                driver.quit()


def test_metrics_after_chat(driver):
    """Проверяем метрики после тестирования чата"""
    driver.get("http://localhost:8001/metrics")
    metrics = driver.find_element(By.TAG_NAME, "pre").text

    # Проверяем, что метрики обновляются
    assert 'websocket_connections_total{endpoint="/ws"}' in metrics
    assert 'websocket_messages_total{endpoint="/ws",type="sent"}' in metrics
    assert 'websocket_messages_total{endpoint="/ws",type="received"}' in metrics
