/**
 * 🚀🛡️ BurnoutGuard Service
 * 
 * Core service for burnout monitoring and analysis
 * Connects mobile app to BurnoutGuard backend
 */

import AsyncStorage from '@react-native-async-storage/async-storage';
import { AppState } from 'react-native';
import BackgroundTimer from 'react-native-background-timer';

import { ApiClient } from './ApiClient';
import { WebSocketService } from './WebSocketService';
import { NotificationService } from './NotificationService';

class BurnoutServiceClass {
  constructor() {
    this.isInitialized = false;
    this.isMonitoring = false;
    this.currentState = null;
    this.riskAssessment = null;
    this.recommendations = [];
    
    // Monitoring intervals
    this.activeMonitoringInterval = null;
    this.backgroundMonitoringInterval = null;
    
    // Event listeners
    this.stateChangeListeners = [];
    this.riskChangeListeners = [];
  }

  // Initialization
  async initialize() {
    try {
      if (this.isInitialized) return;

      // Load saved state
      await this.loadSavedState();
      
      // Initialize API client
      await ApiClient.initialize();
      
      // Setup WebSocket listeners
      this.setupWebSocketListeners();
      
      // Setup app state listeners
      this.setupAppStateListeners();
      
      // Start monitoring if it was previously active
      const wasMonitoring = await AsyncStorage.getItem('wasMonitoring');
      if (wasMonitoring === 'true') {
        await this.startMonitoring();
      }
      
      this.isInitialized = true;
      console.log('BurnoutService initialized successfully');
      
    } catch (error) {
      console.error('Failed to initialize BurnoutService:', error);
      throw error;
    }
  }

  // Monitoring Control
  async startMonitoring() {
    try {
      if (this.isMonitoring) return;

      this.isMonitoring = true;
      await AsyncStorage.setItem('wasMonitoring', 'true');

      // Start active monitoring (every 30 seconds)
      this.activeMonitoringInterval = setInterval(() => {
        this.performAnalysis();
      }, 30000);

      // Initial analysis
      await this.performAnalysis();
      
      // Notify listeners
      this.notifyStateChange();
      
      console.log('Burnout monitoring started');
      
    } catch (error) {
      console.error('Failed to start monitoring:', error);
      this.isMonitoring = false;
    }
  }

  async stopMonitoring() {
    try {
      this.isMonitoring = false;
      await AsyncStorage.setItem('wasMonitoring', 'false');

      // Clear intervals
      if (this.activeMonitoringInterval) {
        clearInterval(this.activeMonitoringInterval);
        this.activeMonitoringInterval = null;
      }

      if (this.backgroundMonitoringInterval) {
        BackgroundTimer.clearInterval(this.backgroundMonitoringInterval);
        this.backgroundMonitoringInterval = null;
      }

      // Notify listeners
      this.notifyStateChange();
      
      console.log('Burnout monitoring stopped');
      
    } catch (error) {
      console.error('Failed to stop monitoring:', error);
    }
  }

  async pauseMonitoring() {
    if (!this.isMonitoring) return;

    // Switch to background monitoring (every 5 minutes)
    if (this.activeMonitoringInterval) {
      clearInterval(this.activeMonitoringInterval);
      this.activeMonitoringInterval = null;
    }

    this.backgroundMonitoringInterval = BackgroundTimer.setInterval(() => {
      this.performLightAnalysis();
    }, 300000); // 5 minutes

    console.log('Switched to background monitoring');
  }

  async resumeMonitoring() {
    if (!this.isMonitoring) return;

    // Switch back to active monitoring
    if (this.backgroundMonitoringInterval) {
      BackgroundTimer.clearInterval(this.backgroundMonitoringInterval);
      this.backgroundMonitoringInterval = null;
    }

    this.activeMonitoringInterval = setInterval(() => {
      this.performAnalysis();
    }, 30000);

    // Immediate analysis
    await this.performAnalysis();

    console.log('Resumed active monitoring');
  }

  // Core Analysis
  async performAnalysis() {
    try {
      // Collect current context
      const context = await this.collectCurrentContext();
      
      // Send to backend for analysis
      const analysisResult = await ApiClient.analyzeBurnoutRisk(context);
      
      if (analysisResult) {
        await this.processAnalysisResult(analysisResult);
      }
      
    } catch (error) {
      console.error('Analysis failed:', error);
    }
  }

  async performLightAnalysis() {
    try {
      // Lighter analysis for background mode
      const basicContext = await this.collectBasicContext();
      const result = await ApiClient.getQuickRiskAssessment(basicContext);
      
      if (result && result.risk_score > 0.7) {
        // Only process high-risk situations in background
        await this.processAnalysisResult(result);
      }
      
    } catch (error) {
      console.error('Light analysis failed:', error);
    }
  }

  // Context Collection
  async collectCurrentContext() {
    const now = new Date();
    
    return {
      timestamp: now.toISOString(),
      app_usage: await this.getAppUsageData(),
      work_session: await this.getWorkSessionData(),
      user_inputs: await this.getRecentUserInputs(),
      device_metrics: await this.getDeviceMetrics(),
      time_context: {
        hour: now.getHours(),
        day_of_week: now.getDay(),
        is_weekend: now.getDay() === 0 || now.getDay() === 6,
      }
    };
  }

  async collectBasicContext() {
    const now = new Date();
    
    return {
      timestamp: now.toISOString(),
      work_session: await this.getBasicWorkSession(),
      time_context: {
        hour: now.getHours(),
        is_weekend: now.getDay() === 0 || now.getDay() === 6,
      }
    };
  }

  // Data Collection Helpers
  async getAppUsageData() {
    // In production, this would collect actual app usage data
    return {
      session_duration: await this.getCurrentSessionDuration(),
      breaks_taken: await this.getTodayBreaksCount(),
      screen_time: await this.getTodayScreenTime(),
    };
  }

  async getWorkSessionData() {
    const startTime = await AsyncStorage.getItem('workSessionStart');
    if (!startTime) return null;

    const start = new Date(startTime);
    const now = new Date();
    const duration = (now - start) / (1000 * 60 * 60); // hours

    return {
      start_time: start.toISOString(),
      duration_hours: duration,
      breaks_count: await this.getTodayBreaksCount(),
    };
  }

  async getRecentUserInputs() {
    // Collect recent user interactions for emotional analysis
    const inputs = await AsyncStorage.getItem('recentUserInputs');
    return inputs ? JSON.parse(inputs) : [];
  }

  async getDeviceMetrics() {
    // Basic device metrics that might indicate stress
    return {
      battery_level: await this.getBatteryLevel(),
      network_quality: await this.getNetworkQuality(),
    };
  }

  // Analysis Result Processing
  async processAnalysisResult(result) {
    // Update current state
    this.currentState = result.current_state;
    this.riskAssessment = result.risk_assessment;
    this.recommendations = result.recommendations || [];

    // Save state
    await this.saveCurrentState();

    // Check for critical alerts
    if (result.risk_assessment?.score >= 0.8) {
      await this.handleCriticalRisk(result);
    } else if (result.risk_assessment?.score >= 0.6) {
      await this.handleHighRisk(result);
    }

    // Notify listeners
    this.notifyStateChange();
    this.notifyRiskChange(result.risk_assessment);
  }

  // Alert Handling
  async handleCriticalRisk(result) {
    console.log('Critical burnout risk detected');
    
    // Send immediate notification
    await NotificationService.sendCriticalAlert({
      title: '🚨 Critical Burnout Alert',
      body: 'Take immediate action to prevent burnout',
      data: { risk_score: result.risk_assessment.score }
    });

    // Log critical event
    await this.logCriticalEvent(result);
  }

  async handleHighRisk(result) {
    console.log('High burnout risk detected');
    
    // Send warning notification
    await NotificationService.sendWarningAlert({
      title: '⚠️ High Burnout Risk',
      body: 'Consider taking a break soon',
      data: { risk_score: result.risk_assessment.score }
    });
  }

  // Data Persistence
  async saveCurrentState() {
    try {
      const state = {
        currentState: this.currentState,
        riskAssessment: this.riskAssessment,
        recommendations: this.recommendations,
        lastUpdate: new Date().toISOString(),
      };
      
      await AsyncStorage.setItem('burnoutState', JSON.stringify(state));
    } catch (error) {
      console.error('Failed to save state:', error);
    }
  }

  async loadSavedState() {
    try {
      const savedState = await AsyncStorage.getItem('burnoutState');
      if (savedState) {
        const state = JSON.parse(savedState);
        this.currentState = state.currentState;
        this.riskAssessment = state.riskAssessment;
        this.recommendations = state.recommendations || [];
      }
    } catch (error) {
      console.error('Failed to load saved state:', error);
    }
  }

  // WebSocket Integration
  setupWebSocketListeners() {
    WebSocketService.onBurnoutUpdate((update) => {
      this.processWebSocketUpdate(update);
    });

    WebSocketService.onCriticalAlert((alert) => {
      this.handleCriticalRisk(alert);
    });
  }

  processWebSocketUpdate(update) {
    if (update.user_id === this.getCurrentUserId()) {
      this.riskAssessment = {
        score: update.burnout_risk_score,
        level: update.burnout_risk_level,
        indicators: update.risk_indicators,
      };
      
      this.recommendations = update.recommendations || [];
      this.notifyStateChange();
    }
  }

  // App State Management
  setupAppStateListeners() {
    AppState.addEventListener('change', (nextAppState) => {
      if (nextAppState === 'active') {
        this.resumeMonitoring();
      } else if (nextAppState === 'background') {
        this.pauseMonitoring();
      }
    });
  }

  // Event Listeners
  addStateChangeListener(listener) {
    this.stateChangeListeners.push(listener);
  }

  removeStateChangeListener(listener) {
    this.stateChangeListeners = this.stateChangeListeners.filter(l => l !== listener);
  }

  addRiskChangeListener(listener) {
    this.riskChangeListeners.push(listener);
  }

  removeRiskChangeListener(listener) {
    this.riskChangeListeners = this.riskChangeListeners.filter(l => l !== listener);
  }

  notifyStateChange() {
    this.stateChangeListeners.forEach(listener => {
      try {
        listener(this.currentState);
      } catch (error) {
        console.error('Error in state change listener:', error);
      }
    });
  }

  notifyRiskChange(riskAssessment) {
    this.riskChangeListeners.forEach(listener => {
      try {
        listener(riskAssessment);
      } catch (error) {
        console.error('Error in risk change listener:', error);
      }
    });
  }

  // Public API
  getCurrentState() {
    return this.currentState;
  }

  getRiskAssessment() {
    return this.riskAssessment;
  }

  getRecommendations() {
    return this.recommendations;
  }

  isCurrentlyMonitoring() {
    return this.isMonitoring;
  }

  // Utility Methods
  async getCurrentSessionDuration() {
    const startTime = await AsyncStorage.getItem('sessionStart');
    if (!startTime) return 0;
    
    const start = new Date(startTime);
    const now = new Date();
    return (now - start) / (1000 * 60); // minutes
  }

  async getTodayBreaksCount() {
    const breaks = await AsyncStorage.getItem('todayBreaks');
    return breaks ? JSON.parse(breaks).length : 0;
  }

  async getTodayScreenTime() {
    // Placeholder - in production, would get actual screen time
    return Math.floor(Math.random() * 8) + 4; // 4-12 hours
  }

  getCurrentUserId() {
    // Get from auth service
    return 'current_user_id';
  }

  async getBatteryLevel() {
    // Placeholder - in production, would get actual battery level
    return Math.floor(Math.random() * 100);
  }

  async getNetworkQuality() {
    // Placeholder - in production, would measure network quality
    return 'good';
  }

  async logCriticalEvent(result) {
    try {
      await ApiClient.logCriticalEvent({
        timestamp: new Date().toISOString(),
        risk_score: result.risk_assessment.score,
        indicators: result.risk_assessment.indicators,
        context: await this.collectBasicContext(),
      });
    } catch (error) {
      console.error('Failed to log critical event:', error);
    }
  }

  async getBasicWorkSession() {
    const startTime = await AsyncStorage.getItem('workSessionStart');
    if (!startTime) return null;

    const start = new Date(startTime);
    const now = new Date();
    const duration = (now - start) / (1000 * 60 * 60); // hours

    return {
      duration_hours: duration,
      breaks_count: await this.getTodayBreaksCount(),
    };
  }

  // Manual Actions
  async recordBreak(duration) {
    try {
      const breaks = await AsyncStorage.getItem('todayBreaks');
      const breaksList = breaks ? JSON.parse(breaks) : [];
      
      breaksList.push({
        timestamp: new Date().toISOString(),
        duration: duration,
      });
      
      await AsyncStorage.setItem('todayBreaks', JSON.stringify(breaksList));
      
      // Notify backend
      await ApiClient.recordBreak(duration);
      
    } catch (error) {
      console.error('Failed to record break:', error);
    }
  }

  async recordUserFeedback(feedback) {
    try {
      await ApiClient.submitUserFeedback({
        timestamp: new Date().toISOString(),
        feedback: feedback,
        current_risk: this.riskAssessment?.score,
      });
    } catch (error) {
      console.error('Failed to record feedback:', error);
    }
  }
}

// Export singleton instance
export const BurnoutService = new BurnoutServiceClass();