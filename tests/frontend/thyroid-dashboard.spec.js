// @ts-check
const { test, expect } = require('@playwright/test');

test.describe('Thyroid Endocrine System Dashboard', () => {
  test.beforeEach(async ({ page }) => {
    // Navigate to the dashboard
    await page.goto('/');
    
    // Wait for the dashboard to load
    await page.waitForLoadState('networkidle');
    
    // Wait for thyroid data to be fetched
    await page.waitForTimeout(2000);
  });

  test('should display thyroid status section', async ({ page }) => {
    // Check if thyroid section exists
    const thyroidSection = page.locator('.card-header:has-text("Endocrine Stress Level")');
    await expect(thyroidSection).toBeVisible();
    
    // Check if progress bar exists
    const progressBar = page.locator('#thyroidBar');
    await expect(progressBar).toBeVisible();
    
    // Check if label exists
    const statusText = page.locator('#thyroidLabel');
    await expect(statusText).toBeVisible();
    
    // Take screenshot of normal state
    await page.screenshot({ 
      path: 'test-results/thyroid-normal-state.png',
      fullPage: true 
    });
  });

  test('should show thyroid charge and threshold', async ({ page }) => {
    // Wait for thyroid data to load
    await page.waitForSelector('#thyroidBar');
    
    // Check if progress bar has width (indicating data loaded)
    const progressBar = page.locator('#thyroidBar');
    const width = await progressBar.getAttribute('style');
    expect(width).toContain('width');
    
    // Check if label shows percentage
    const labelElement = page.locator('#thyroidLabel');
    const labelText = await labelElement.textContent();
    expect(labelText).toMatch(/\d+%/); // Should contain percentage
    
    // Take screenshot showing values
    await page.screenshot({ 
      path: 'test-results/thyroid-values.png',
      clip: { x: 0, y: 400, width: 800, height: 300 }
    });
  });

  test('should simulate stress state with pulse animation', async ({ page }) => {
    // Mock high stress thyroid data
    await page.evaluate(() => {
      // Simulate API response with high charge
      // @ts-ignore
      window.thyroidData = {
        charge: 95,
        threshold: 100,
        ready: false,
        last_total_errors: 150
      };
      
      // Trigger update if function exists
      // @ts-ignore
      if (window.updateThyroidStatus) {
        // @ts-ignore
        window.updateThyroidStatus();
      }
    });
    
    // Wait for UI update
    await page.waitForTimeout(1000);
    
    // Check if progress bar has pulse class
    const progressBar = page.locator('#thyroidBar');
    const hasAnimation = await progressBar.evaluate(el => {
      const style = window.getComputedStyle(el);
      return style.animationName !== 'none' || el.classList.contains('pulse');
    });
    
    // Take screenshot of stress state
    await page.screenshot({ 
      path: 'test-results/thyroid-stress-state.png',
      fullPage: true 
    });
    
    // Verify stress indicators
    expect(hasAnimation).toBeTruthy();
  });

  test('should fetch thyroid data from API', async ({ page, request }) => {
    // Test API endpoint directly
    const response = await request.get('/thyroid/status');
    expect(response.ok()).toBeTruthy();
    
    const data = await response.json();
    expect(data).toHaveProperty('charge');
    expect(data).toHaveProperty('threshold');
    expect(data).toHaveProperty('ready');
    
    // Verify API data matches UI
    await page.goto('/');
    await page.waitForLoadState('networkidle');
    
    // Check if UI reflects API data (progress bar width should reflect charge/threshold ratio)
    const progressBar = page.locator('#thyroidBar');
    const ariaValue = await progressBar.getAttribute('aria-valuenow');
    if (ariaValue) {
      const expectedPercentage = Math.round((data.charge / data.threshold) * 100);
      expect(parseInt(ariaValue)).toBeCloseTo(expectedPercentage, 10);
    }
  });

  test('should be responsive on mobile', async ({ page }) => {
    // Set mobile viewport
    await page.setViewportSize({ width: 375, height: 667 });
    
    // Navigate and wait for load
    await page.goto('/');
    await page.waitForLoadState('networkidle');
    
    // Check if thyroid section is still visible
    const thyroidSection = page.locator('.card-header:has-text("Endocrine Stress Level")');
    await expect(thyroidSection).toBeVisible();
    
    // Take mobile screenshot
    await page.screenshot({ 
      path: 'test-results/thyroid-mobile.png',
      fullPage: true 
    });
  });

  test('should handle loading states gracefully', async ({ page }) => {
    // Intercept API call to simulate slow response
    await page.route('/thyroid/status', async route => {
      await new Promise(resolve => setTimeout(resolve, 3000));
      await route.continue();
    });
    
    await page.goto('/');
    
    // Check if loading state is handled
    const thyroidSection = page.locator('.card-header:has-text("Endocrine Stress Level")');
    await expect(thyroidSection).toBeVisible();
    
    // Wait for data to eventually load
    await page.waitForTimeout(4000);
    
    // Verify data loaded
    const progressBar = page.locator('#thyroidBar');
    await expect(progressBar).toBeVisible();
  });
});
