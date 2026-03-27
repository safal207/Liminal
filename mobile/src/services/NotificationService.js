/**
 * NotificationService — локальные уведомления для BurnoutGuard
 */

class NotificationServiceClass {
  constructor() {
    this.isInitialized = false;
  }

  async initialize() {
    this.isInitialized = true;
    console.log('NotificationService initialized (stub)');
  }

  async sendRiskAlert(riskLevel, message) {
    console.log(`[Notification] Risk alert: ${riskLevel} — ${message}`);
  }

  async sendRecommendationReminder(recommendation) {
    console.log(`[Notification] Reminder: ${recommendation.title}`);
  }

  async cancelAll() {
    console.log('[Notification] All notifications cancelled');
  }
}

export const NotificationService = new NotificationServiceClass();
