import pytest

# Skip these tests gracefully if Playwright is not installed
pytest.importorskip("playwright.sync_api", reason="playwright not installed")
from playwright.sync_api import expect, sync_playwright

BASE_URL = "http://127.0.0.1:8080"
AT_RISK_URL = f"{BASE_URL}/at-risk"


@pytest.mark.integration
@pytest.mark.skip(reason="Missing OS-level browser dependencies in environment")
def test_visual_table_snapshot(tmp_path, live_server):
    """Visual regression of the table after Seed+Refresh using Playwright snapshots.

    First run will create snapshots when invoked with `pytest --update-snapshots`.
    Subsequent runs will compare screenshots and fail on diffs.
    """
    with sync_playwright() as pw:
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
