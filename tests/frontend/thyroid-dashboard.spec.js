#!/usr/bin/env node
const assert = require('assert');
const fs = require('fs');
const path = require('path');
const vm = require('vm');

const tests = [];
function test(name, fn) {
  tests.push({ name, fn });
}

function flushMicrotasks() {
  return new Promise(resolve => setImmediate(resolve));
}

function createDashboardSandbox(overrides = {}) {
  const bar = { style: {}, className: 'progress-bar bg-info' };
  const label = { textContent: '' };
  const fetchCalls = [];
  const listeners = {};

  const context = {
    console,
    window: {},
    setTimeout,
    clearTimeout,
    fetch: (url) => {
      fetchCalls.push(url);
      if (typeof overrides.fetch === 'function') {
        return overrides.fetch(url, { bar, label });
      }
      return Promise.resolve({
        json: () => Promise.resolve({ charge: 40, threshold: 80, ready: false })
      });
    },
    document: {
      addEventListener: (event, handler) => {
        listeners[event] = handler;
      },
      getElementById: (id) => {
        if (typeof overrides.getElementById === 'function') {
          const override = overrides.getElementById(id, { bar, label });
          if (override !== undefined) {
            return override;
          }
        }
        if (id === 'thyroidBar') return bar;
        if (id === 'thyroidLabel') return label;
        return null;
      },
      querySelectorAll: () => ({ forEach: () => {} })
    },
    Chart: function () {}
  };

  context.window.document = context.document;

  const scriptPath = path.join(__dirname, '../../backend/ml/tools/log_viewer/static/js/dashboard.js');
  const script = fs.readFileSync(scriptPath, 'utf-8');
  vm.runInNewContext(script, context);

  return { context, bar, label, fetchCalls, listeners };
}

test('Thyroid card markup exists in dashboard template', () => {
  const templatePath = path.join(__dirname, '../../backend/ml/tools/log_viewer/templates/index.html');
  const html = fs.readFileSync(templatePath, 'utf-8');

  assert.ok(html.includes('Endocrine Stress Level'), 'Card header should mention endocrine stress');
  assert.ok(/id="thyroidBar"/.test(html), 'Progress bar element is missing');
  assert.ok(/id="thyroidLabel"/.test(html), 'Progress label element is missing');
});

test('initThyroidBar fetches thyroid status and updates DOM', async () => {
  const overrides = {
    fetch: () => Promise.resolve({
      json: () => Promise.resolve({ charge: 50, threshold: 200, ready: true })
    })
  };

  const { context, bar, label, fetchCalls } = createDashboardSandbox(overrides);

  assert.strictEqual(typeof context.initThyroidBar, 'function', 'initThyroidBar should be defined');
  context.initThyroidBar();
  await flushMicrotasks();

  assert.strictEqual(fetchCalls[0], '/api/thyroid_status', 'initThyroidBar should request thyroid API endpoint');
  assert.strictEqual(bar.style.width, '25%', 'Progress bar width should reflect charge ratio');
  assert.ok(bar.className.includes('progress-bar'), 'Progress bar class should be preserved');
  assert.ok(bar.className.includes('bg-danger'), 'Ready state should apply danger styling');
  assert.ok(bar.className.includes('pulse'), 'Ready state should apply pulse animation');
  assert.strictEqual(label.textContent, '25%', 'Label should show rounded percentage');
});

test('initThyroidBar clamps percentage to 100%', async () => {
  const overrides = {
    fetch: () => Promise.resolve({
      json: () => Promise.resolve({ charge: 500, threshold: 100, ready: true })
    })
  };

  const { context, bar, label } = createDashboardSandbox(overrides);
  context.initThyroidBar();
  await flushMicrotasks();

  assert.strictEqual(bar.style.width, '100%', 'Progress bar width should not exceed 100%');
  assert.strictEqual(label.textContent, '100%', 'Label should be clamped to 100%');
});

test('DOMContentLoaded listener initialises thyroid bar when present', async () => {
  let initCalled = false;
  const overrides = {
    getElementById: (id, elements) => {
      if (id === 'thyroidBar') {
        initCalled = true;
        return elements.bar;
      }
      if (id === 'thyroidLabel') {
        return elements.label;
      }
      return undefined;
    }
  };

  const { listeners } = createDashboardSandbox(overrides);
  assert.ok(typeof listeners.DOMContentLoaded === 'function', 'DOMContentLoaded handler should be registered');

  // Simulate DOM ready
  await listeners.DOMContentLoaded();

  assert.ok(initCalled, 'initThyroidBar should be invoked when thyroid elements exist');
});

(async () => {
  let failed = 0;
  for (const { name, fn } of tests) {
    try {
      await fn();
      console.log(`\x1b[32m✓\x1b[0m ${name}`);
    } catch (error) {
      failed += 1;
      console.error(`\x1b[31m✗\x1b[0m ${name}`);
      console.error(error);
    }
  }

  if (failed) {
    console.error(`\n${failed} test(s) failed.`);
    process.exit(1);
  } else {
    console.log(`\nAll ${tests.length} test(s) passed.`);
  }
})();
