# Поток оплаты: Stripe Checkout (Liminal)

Один основной поток для первых продаж (B2C Pro). Техническая реализация: [docs/billing/STRIPE_INTEGRATION.md](../../../docs/billing/STRIPE_INTEGRATION.md).

## Шаги для пользователя

1. Регистрация / вход → JWT.
2. Экран «Upgrade» → `POST /billing/checkout-session` (Authorization: Bearer) → редирект на `url` от Stripe.
3. Оплата в Stripe Checkout → редирект на `STRIPE_SUCCESS_URL`.
4. Stripe шлёт webhook на `POST /billing/webhook` → обновляется локальный tier пользователя (`pro`).
5. Управление подпиской → `POST /billing/portal-session` → Customer Portal.

## Шаги для команды (один раз)

1. Создать продукт и цену (recurring) в Stripe Dashboard; скопировать Price ID в `STRIPE_PRICE_PRO_MONTHLY`.
2. Настроить webhook endpoint на публичный URL: `https://<api-host>/billing/webhook`, события: `checkout.session.completed`, `customer.subscription.updated`, `customer.subscription.deleted`.
3. Скопировать signing secret в `STRIPE_WEBHOOK_SECRET`, secret key в `STRIPE_SECRET_KEY`.

## Метрики конверсии (ручной учёт на старте)

| Метрика | Как считать |
|---------|-------------|
| Активации Checkout | число успешных `checkout.session.completed` |
| Оплаченные Pro | пользователи с `tier=pro` в `/billing/me` |
| Отмены | `customer.subscription.deleted` + портал |
| MRR | сумма активных подписок в Stripe Dashboard |

Цели PLF (см. [README](../README.md) родительской папки): список лидов до открытия продаж, затем конверсия запуска.

## Пилот

- 3–5 пользователей: тестовые ключи Stripe, сценарий «логин → оплата → BurnoutGuard API открывается (402 исчезает)».
