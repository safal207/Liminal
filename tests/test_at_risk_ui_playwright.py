import pytest

# Skip these tests gracefully if Playwright is not installed
pytest.importorskip("playwright.sync_api", reason="playwright not installed")
from playwright.sync_api import expect, sync_playwright

BASE_URL = "http://127.0.0.1:8080"
AT_RISK_URL = f"{BASE_URL}/at-risk"


@pytest.mark.integration
@pytest.mark.skip(reason="Missing OS-level browser dependencies in environment")
def test_at_risk_buttons_workflow(tmp_path, live_server):
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

        # Expect results to be populated
        results = page.locator("#results")
        expect(results).to_be_visible()
        # Wait for at least one result row to appear
        page.wait_for_selector("#results .row")
        # Verify score badges exist
        score_badges = page.locator(".badge.danger, .badge.warning, .badge.good")
        expect(score_badges.first).to_be_visible()
        assert score_badges.count() >= 1

        # Run built-in UI self-test button (should alert PASS)
        page.get_by_role("button", name="Run UI Self-Test").click()
        # There should be at least one dialog captured
        assert any("PASS" in m for m in dialog_messages), f"Dialog messages: {dialog_messages}"

        ctx.close()
        browser.close()
