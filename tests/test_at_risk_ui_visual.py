import subprocess
import sys
import time
from contextlib import contextmanager

import pytest

# Skip these tests gracefully if Playwright is not installed
pytest.importorskip("playwright.sync_api", reason="playwright not installed")
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
    cmd = [sys.executable, "-m", "uvicorn", "liminal.at_risk_server_neo4j:app", "--port", "8080"]
    proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
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
@pytest.mark.skip(reason="Missing OS-level browser dependencies in environment")
def test_visual_table_snapshot(tmp_path):
    """Visual regression of the table after Seed+Refresh using Playwright snapshots.

    First run will create snapshots when invoked with `pytest --update-snapshots`.
    Subsequent runs will compare screenshots and fail on diffs.
    """
    with run_server(), sync_playwright() as pw:
        browser = pw.chromium.launch(headless=True)
        ctx = browser.new_context(viewport={"width": 1200, "height": 800})
        page = ctx.new_page()

        page.goto(AT_RISK_URL + "?nocache=1")
        page.get_by_role("button", name="Seed Demo").click()
        page.get_by_role("button", name="Refresh").click()
        page.wait_for_selector("#results .row")

        # Focus results area to stabilize screenshot
        results = page.locator("#results")
        expect(results).to_be_visible()
        expect(page).to_have_screenshot("at_risk_table.png", full_page=False)

        ctx.close()
        browser.close()
