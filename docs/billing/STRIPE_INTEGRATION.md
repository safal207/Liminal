# Stripe: модель данных и webhooks

Провайдер по умолчанию: **Stripe** (Billing + Checkout + Customer Portal + webhooks).

## Переменные окружения

| Переменная | Назначение |
|------------|------------|
| `STRIPE_SECRET_KEY` | Secret key (sk_live_… / sk_test_…) |
| `STRIPE_WEBHOOK_SECRET` | Signing secret вебхука (whsec_…) |
| `STRIPE_PRICE_PRO_MONTHLY` | Price ID подписки Pro (price_…) |
| `STRIPE_SUCCESS_URL` | Редирект после успешной оплаты |
| `STRIPE_CANCEL_URL` | Редирект при отмене Checkout |
| `BILLING_STORE_PATH` | Опционально: путь к JSON-хранилищу состояния пользователей |

Если `STRIPE_SECRET_KEY` пуст, эндпоинты Checkout возвращают 503, подписка остаётся `free`.

## Связь пользователя и Stripe

1. **Checkout Session** создаётся с `client_reference_id = <user_id из JWT sub>` и `metadata.user_id` для дублирования.
2. После оплаты webhook `checkout.session.completed` и/или `customer.subscription.*` обновляют локальное состояние: `stripe_customer_id`, `tier`, `status`, `current_period_end`.

## Состояние подписки (локально)

Файл по умолчанию: `backend/app/billing/data/billing_store.json` (можно переопределить `BILLING_STORE_PATH`). Формат — словарь `user_id -> { tier, stripe_customer_id, subscription_id, status }`.

**Идемпотентность:** обработка webhook по `event.id` — пропуск повторов (хранится множество обработанных id в том же файле или в памяти на процесс; для MVP — in-memory set + лог).

## События Stripe (минимум)

- `checkout.session.completed` — привязать customer к user_id.
- `customer.subscription.updated` — обновить tier/status.
- `customer.subscription.deleted` — понизить до `free`.

Маппинг Price ID → tier: если `items[0].price.id == STRIPE_PRICE_PRO_MONTHLY`, то `tier=pro`.

## Customer Portal

`POST /billing/portal-session` создаёт сессию Billing Portal для `stripe_customer_id` текущего пользователя.

## Entitlements

Логика в коде: `backend/app/billing/entitlements.py` — из `tier` вычисляются флаги `burnout_pro`, `team_dashboard`, `api_access`.
