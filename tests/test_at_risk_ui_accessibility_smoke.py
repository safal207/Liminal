import pytest

# Skip these tests gracefully if Playwright is not installed
pytest.importorskip("playwright.sync_api", reason="playwright not installed")
from playwright.sync_api import expect, sync_playwright

BASE_URL = "http://127.0.0.1:8080"
AT_RISK_URL = f"{BASE_URL}/at-risk"


@pytest.mark.integration
@pytest.mark.skip(reason="Missing OS-level browser dependencies in environment")
def test_accessibility_smoke_keyboard_and_roles(live_server):
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
        first_chip = page.locator(".chip").first
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
        page.locator("tbody#tbody")
        page.wait_for_selector("tbody#tbody tr:not(:has(.empty))")
        score_cells = page.locator("td.score")
        page.wait_for_selector("td.score")
        assert score_cells.count() >= 1

        ctx.close()
        browser.close()
