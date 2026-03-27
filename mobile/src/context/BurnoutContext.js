/**
 * BurnoutContext — глобальное состояние BurnoutGuard
 *
 * Предоставляет всем экранам:
 *   - currentState      текущее состояние из backend
 *   - riskAssessment    последняя оценка риска
 *   - recommendations   список рекомендаций
 *   - progress          история риска
 *   - isMonitoring      активен ли мониторинг
 *   - refreshData()     принудительное обновление
 *   - startMonitoring() / stopMonitoring()
 */

import React, { createContext, useState, useEffect, useCallback, useRef } from 'react';
import { ApiClient } from '../services/ApiClient';

export const BurnoutContext = createContext({});

const POLL_INTERVAL_MS = 30000; // 30 сек

export function BurnoutProvider({ children }) {
  const [currentState, setCurrentState]       = useState(null);
  const [riskAssessment, setRiskAssessment]   = useState(null);
  const [recommendations, setRecommendations] = useState([]);
  const [progress, setProgress]               = useState(null);
  const [isMonitoring, setIsMonitoring]       = useState(false);
  const [error, setError]                     = useState(null);

  const pollRef = useRef(null);

  const fetchAssessment = useCallback(async () => {
    try {
      const data = await ApiClient.analyzeBurnoutRisk();
      if (data.status === 'assessed') {
        setCurrentState(data);
        setRiskAssessment(data.risk_assessment ?? null);
        setRecommendations(data.recommendations ?? []);
        setError(null);
      }
    } catch (err) {
      setError(err.message);
    }
  }, []);

  const fetchProgress = useCallback(async () => {
    try {
      const data = await ApiClient.getProgress(30);
      setProgress(data);
    } catch {
      // progress is non-critical
    }
  }, []);

  const refreshData = useCallback(async () => {
    await Promise.all([fetchAssessment(), fetchProgress()]);
  }, [fetchAssessment, fetchProgress]);

  const startMonitoring = useCallback(async () => {
    if (isMonitoring) return;
    setIsMonitoring(true);
    await fetchAssessment();
    pollRef.current = setInterval(fetchAssessment, POLL_INTERVAL_MS);
  }, [isMonitoring, fetchAssessment]);

  const stopMonitoring = useCallback(() => {
    setIsMonitoring(false);
    if (pollRef.current) {
      clearInterval(pollRef.current);
      pollRef.current = null;
    }
  }, []);

  // Первичная загрузка
  useEffect(() => {
    ApiClient.initialize().then(() => {
      refreshData();
    });
    return () => stopMonitoring();
  }, []);

  return (
    <BurnoutContext.Provider value={{
      currentState,
      riskAssessment,
      recommendations,
      progress,
      isMonitoring,
      error,
      refreshData,
      startMonitoring,
      stopMonitoring,
    }}>
      {children}
    </BurnoutContext.Provider>
  );
}
