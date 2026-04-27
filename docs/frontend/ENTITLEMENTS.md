# UI и entitlements (Flutter / web)

Бэкенд отдаёт флаги подписки в:

- `GET /auth/me` — поля `subscription_tier`, `entitlements` (при валидном Bearer).
- `GET /billing/me` — то же + `stripe_customer_id`, `subscription_status`.

## Рекомендуемая логика UI

1. После логина вызывать `/auth/me` и сохранять `entitlements`.
2. Экраны BurnoutGuard (или аналитика команды): если `burnout_pro` === false, показать CTA «Оформить Pro» и вызывать `POST /billing/checkout-session`, затем открыть `url` во внешнем браузере или WebView.
3. Настройки подписки: кнопка «Управление подпиской» → `POST /billing/portal-session`.

## Коды ошибок

- `402 Payment Required` — эндпоинт требует Pro/Team (например `/api/v1/burnout/...` с глобальным guard).
