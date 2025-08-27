import subprocess
import sys
import time
from contextlib import contextmanager

import pytest

# Skip these tests gracefully if Playwright is not installed
pytest.importorskip("playwright.sync_api", reason="playwright not installed")
from playwright.sync_api import sync_playwright

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
def test_accessibility_with_axe():
    """Run axe-core in the page and fail on serious/critical violations."""
    with run_server():
        with sync_playwright() as pw:
            browser = pw.chromium.launch(headless=True)
            ctx = browser.new_context()
            page = ctx.new_page()
            page.goto(AT_RISK_URL)

            # Inject axe-core from CDN
            page.add_script_tag(
                url="https://cdnjs.cloudflare.com/ajax/libs/axe-core/4.7.2/axe.min.js"
            )
            result = page.evaluate(
                "async () => await axe.run(document, { resultTypes: ['violations'] })"
            )
            # Filter serious/critical
            violations = [
                v
                for v in result.get("violations", [])
                if v.get("impact") in ("serious", "critical")
            ]
            if violations:
                details = "\n\n".join(
                    f"{v.get('id')} ({v.get('impact')}): {v.get('description')}\nHelp: {v.get('helpUrl')}"
                    for v in violations
                )
                pytest.fail(
                    f"axe-core found {len(violations)} serious/critical violations:\n\n{details}"
                )

            ctx.close()
            browser.close()
