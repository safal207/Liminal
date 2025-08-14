import os
import subprocess
import sys
import time
from contextlib import contextmanager

import pytest
from playwright.sync_api import expect, sync_playwright

BASE_URL = "http://127.0.0.1:8080"
AT_RISK_URL = f"{BASE_URL}/at-risk"


def wait_for_server(url: str, timeout: float = 15.0) -> None:
    import urllib.request

    start = time.time()
    while time.time() - start < timeout:
        try:
            with urllib.request.urlopen(url) as resp:  # nosec B310
                if resp.status in (200, 304):
                    return
        except Exception:
            time.sleep(0.2)
    raise TimeoutError(f"Server not responding at {url}")


@contextmanager
def run_server():
    env = os.environ.copy()
    # Ensure Python can import local scripts
    cmd = [sys.executable, "scripts/simple_server.py"]
    proc = subprocess.Popen(
        cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True
    )
    try:
        wait_for_server(AT_RISK_URL)
        yield proc
    finally:
        proc.terminate()
        try:
            proc.wait(timeout=5)
        except subprocess.TimeoutExpired:
            proc.kill()


@pytest.mark.integration
def test_at_risk_buttons_workflow(tmp_path):
    with run_server():
        with sync_playwright() as pw:
            browser = pw.chromium.launch(headless=True)
            ctx = browser.new_context()
            page = ctx.new_page()

            # Capture alert results from self-test
            dialog_messages = []
            page.on("dialog", lambda d: (dialog_messages.append(d.message), d.accept()))

            page.goto(AT_RISK_URL)

            # Click Seed Demo then Refresh
            page.get_by_role("button", name="Seed Demo").click()
            page.get_by_role("button", name="Refresh").click()

            # Expect table to be populated
            tbody = page.locator("tbody#tbody")
            expect(tbody).to_be_visible()
            # Wait for at least one non-empty row
            page.wait_for_selector("tbody#tbody tr:not(:has(.empty))")
            # Verify scores cells exist and look like 0.123
            score_cells = page.locator("td.score")
            page.wait_for_selector("td.score")
            assert score_cells.count() >= 1

            # Run built-in UI self-test button (should alert PASS)
            page.get_by_role("button", name="Run UI Self-Test").click()
            # There should be at least one dialog captured
            assert any(
                "PASS" in m for m in dialog_messages
            ), f"Dialog messages: {dialog_messages}"

            ctx.close()
            browser.close()
