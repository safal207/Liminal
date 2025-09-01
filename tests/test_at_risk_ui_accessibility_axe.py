import pytest

# Skip these tests gracefully if Playwright is not installed
pytest.importorskip("playwright.sync_api", reason="playwright not installed")
from playwright.sync_api import sync_playwright

BASE_URL = "http://127.0.0.1:8080"
AT_RISK_URL = f"{BASE_URL}/at-risk"


@pytest.mark.integration
@pytest.mark.skip(reason="Missing OS-level browser dependencies in environment")
def test_accessibility_with_axe(live_server):
    """Run axe-core in the page and fail on serious/critical violations."""
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
