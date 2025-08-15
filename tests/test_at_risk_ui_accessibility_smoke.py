import os
import re
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
def test_accessibility_smoke_keyboard_and_roles():
    with run_server():
        with sync_playwright() as pw:
            browser = pw.chromium.launch(headless=True)
            ctx = browser.new_context()
            page = ctx.new_page()
            page.goto(AT_RISK_URL)

            # Buttons present and accessible by role
            seed_btn = page.get_by_role("button", name="Seed Demo")
            refresh_btn = page.get_by_role("button", name="Refresh")
            add_btn = page.get_by_role("button", name="Add Node")
            expect(seed_btn).to_be_visible()
            expect(refresh_btn).to_be_visible()
            expect(add_btn).to_be_visible()

            # Chips container exists with aria-label
            chips = page.locator("#triggerChips")
            expect(chips).to_be_visible()
            assert chips.get_attribute("aria-label") == "Sales Triggers"

            # Keyboard navigation: focus first chip and toggle with Space
            first_chip = page.locator(".chip-toggle").first
            first_chip.focus()
            page.keyboard.press(" ")
            # It should toggle 'active' class
            # Wait until class reflects activation
            page.wait_for_function(
                "el => el.classList.contains('active')",
                arg=first_chip.element_handle(),
            )

            # Run flow using keyboard: Tab to Seed, Space, then Tab to Refresh, Space
            seed_btn.focus()
            page.keyboard.press(" ")
            refresh_btn.focus()
            page.keyboard.press(" ")

            # Table should populate
            tbody = page.locator("tbody#tbody")
            page.wait_for_selector("tbody#tbody tr:not(:has(.empty))")
            score_cells = page.locator("td.score")
            page.wait_for_selector("td.score")
            assert score_cells.count() >= 1

            ctx.close()
            browser.close()
