/**
 * ApiClient — HTTP-клиент для BurnoutGuard Mobile
 *
 * Маппинг методов → backend endpoints:
 *   assessBurnout        → POST /api/v1/burnout/user/{id}/assess
 *   getProgress          → GET  /api/v1/burnout/user/{id}/progress
 *   submitFeedback       → POST /api/v1/burnout/user/{id}/feedback
 *   getEmotimeStatus     → GET  /emotime/status
 *   processText          → POST /emotime/text
 */

import AsyncStorage from '@react-native-async-storage/async-storage';

const BASE_URL = 'http://localhost:8000';
const DEFAULT_USER_ID = 'mobile_user';

class ApiClientClass {
  constructor() {
    this.baseUrl = BASE_URL;
    this.userId = DEFAULT_USER_ID;
    this.isInitialized = false;
  }

  async initialize() {
    try {
      const savedUserId = await AsyncStorage.getItem('userId');
      if (savedUserId) this.userId = savedUserId;
      this.isInitialized = true;
    } catch {
      this.isInitialized = true;
    }
  }

  async _request(method, path, body = null) {
    const options = {
      method,
      headers: { 'Content-Type': 'application/json' },
    };
    if (body) options.body = JSON.stringify(body);

    const res = await fetch(`${this.baseUrl}${path}`, options);
    if (!res.ok) {
      const text = await res.text();
      throw new Error(`API ${res.status}: ${text}`);
    }
    return res.json();
  }

  // --- Burnout endpoints ---

  async analyzeBurnoutRisk(context = {}) {
    return this._request('POST', `/api/v1/burnout/user/${this.userId}/assess`, { context });
  }

  async getQuickRiskAssessment(basicContext = {}) {
    return this.analyzeBurnoutRisk(basicContext);
  }

  async getProgress(days = 30) {
    return this._request('GET', `/api/v1/burnout/user/${this.userId}/progress?days=${days}`);
  }

  async submitUserFeedback(feedback) {
    return this._request('POST', `/api/v1/burnout/user/${this.userId}/feedback`, feedback);
  }

  async logCriticalEvent(data) {
    return this._request('POST', `/api/v1/burnout/user/${this.userId}/feedback`, {
      recommendation_id: data.event_id || 'critical_event',
      action: 'viewed',
      effectiveness_rating: null,
    });
  }

  async recordBreak(duration) {
    return this._request('POST', `/api/v1/burnout/user/${this.userId}/feedback`, {
      recommendation_id: 'break',
      action: 'completed',
      effectiveness_rating: duration > 10 ? 0.9 : 0.6,
    });
  }

  // --- Emotime endpoints ---

  async getEmotimeStatus() {
    return this._request('GET', '/emotime/status');
  }

  async processText(text, typingSpeed = null) {
    return this._request('POST', '/emotime/text', {
      text,
      typing_speed: typingSpeed,
      user_id: this.userId,
    });
  }
}

export const ApiClient = new ApiClientClass();
