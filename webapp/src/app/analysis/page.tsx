'use client';

import { useEffect, useState } from 'react';
import { api, AssessmentResult, RISK_COLORS, RISK_LABELS, fmtPct } from '@/lib/api';

const CARD: React.CSSProperties = {
  background: 'var(--surface)',
  border: '1px solid var(--border)',
  borderRadius: 'var(--radius)',
  padding: 20,
  marginBottom: 16,
};

const FACTOR_RU: Record<string, string> = {
  emotional_state:     'Эмоциональное состояние',
  behavioral_patterns: 'Поведенческие паттерны',
  temporal_analysis:   'Временной анализ',
  ml_confidence:       'ML уверенность',
};

export default function AnalysisPage() {
  const [data, setData]       = useState<AssessmentResult | null>(null);
  const [loading, setLoading] = useState(false);

  const load = async () => {
    setLoading(true);
    try { setData(await api.assess()); } catch { setData(null); } finally { setLoading(false); }
  };

  useEffect(() => { load(); }, []);

  const risk = data?.risk_assessment;
  const mode = data?.burnout_mode;
  const level = risk?.level ?? 'unknown';
  const riskColor = RISK_COLORS[level] ?? '#607D8B';

  return (
    <div style={{ padding: 28 }}>
      <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: 24 }}>
        <h1 style={{ fontSize: 22, fontWeight: 700 }}>Анализ риска</h1>
        <button onClick={load} disabled={loading} style={{
          background: 'var(--primary)', color: '#fff', border: 'none',
          borderRadius: 8, padding: '8px 18px', cursor: 'pointer', fontSize: 14, fontWeight: 600, opacity: loading ? 0.7 : 1,
        }}>{loading ? 'Загрузка…' : 'Обновить'}</button>
      </div>

      {!risk && !loading && (
        <div style={{ ...CARD, color: 'var(--text2)', textAlign: 'center' }}>Нет данных. Перейдите на Dashboard и нажмите «Обновить».</div>
      )}

      {risk && (
        <>
          {/* Overall */}
          <div style={{ ...CARD, borderLeft: `4px solid ${riskColor}` }}>
            <div style={{ fontSize: 11, color: 'var(--text2)', marginBottom: 4, textTransform: 'uppercase', letterSpacing: 0.5 }}>Общий скор</div>
            <div style={{ display: 'flex', alignItems: 'baseline', gap: 12 }}>
              <span style={{ fontSize: 52, fontWeight: 800, color: riskColor, lineHeight: 1 }}>{fmtPct(risk.score)}</span>
              <div>
                <div style={{ fontSize: 16, fontWeight: 600, color: riskColor }}>{RISK_LABELS[level]}</div>
                <div style={{ fontSize: 13, color: 'var(--text2)' }}>уверенность {fmtPct(risk.confidence)}</div>
              </div>
            </div>
          </div>

          {/* Factors */}
          <div style={CARD}>
            <h2 style={{ fontSize: 15, fontWeight: 600, marginBottom: 16 }}>Факторы риска</h2>
            {Object.entries(risk.factors).map(([key, val]) => (
              <div key={key} style={{ marginBottom: 14 }}>
                <div style={{ display: 'flex', justifyContent: 'space-between', fontSize: 13, marginBottom: 5 }}>
                  <span style={{ color: 'var(--text2)' }}>{FACTOR_RU[key] ?? key}</span>
                  <span style={{ fontWeight: 700, color: RISK_COLORS[val > 0.6 ? 'high' : val > 0.4 ? 'medium' : 'low'] }}>{fmtPct(val)}</span>
                </div>
                <div style={{ height: 8, background: 'var(--surface2)', borderRadius: 4, overflow: 'hidden' }}>
                  <div style={{ height: '100%', width: fmtPct(val), background: val > 0.6 ? '#f44336' : 'var(--primary)', borderRadius: 4, transition: 'width .4s' }} />
                </div>
              </div>
            ))}
          </div>

          {/* Duration + Trend risk */}
          <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: 16 }}>
            <div style={CARD}>
              <div style={{ fontSize: 11, color: 'var(--text2)', textTransform: 'uppercase', letterSpacing: 0.5, marginBottom: 4 }}>Риск длительности</div>
              <div style={{ fontSize: 28, fontWeight: 800, color: RISK_COLORS[risk.duration_risk > 0.6 ? 'high' : 'medium'] }}>{fmtPct(risk.duration_risk)}</div>
            </div>
            <div style={CARD}>
              <div style={{ fontSize: 11, color: 'var(--text2)', textTransform: 'uppercase', letterSpacing: 0.5, marginBottom: 4 }}>Риск тренда</div>
              <div style={{ fontSize: 28, fontWeight: 800, color: RISK_COLORS[risk.trend_risk > 0.6 ? 'high' : 'medium'] }}>{fmtPct(risk.trend_risk)}</div>
            </div>
          </div>

          {/* Indicators */}
          {risk.emotional_indicators.length > 0 && (
            <div style={CARD}>
              <h2 style={{ fontSize: 15, fontWeight: 600, marginBottom: 12 }}>Эмоциональные индикаторы</h2>
              {risk.emotional_indicators.map((ind, i) => (
                <div key={i} style={{ display: 'flex', gap: 8, marginBottom: 6, fontSize: 13, alignItems: 'flex-start' }}>
                  <span style={{ color: 'var(--primary)', marginTop: 1 }}>◆</span>
                  <span>{ind}</span>
                </div>
              ))}
            </div>
          )}

          {/* Burnout mode */}
          {mode && (
            <div style={CARD}>
              <h2 style={{ fontSize: 15, fontWeight: 600, marginBottom: 12 }}>Режим выгорания</h2>
              <div style={{ display: 'flex', gap: 16, flexWrap: 'wrap' }}>
                <div>
                  <div style={{ fontSize: 11, color: 'var(--text2)', marginBottom: 2 }}>Тип</div>
                  <div style={{ fontSize: 16, fontWeight: 600 }}>{mode.type}</div>
                </div>
                <div>
                  <div style={{ fontSize: 11, color: 'var(--text2)', marginBottom: 2 }}>Скор</div>
                  <div style={{ fontSize: 16, fontWeight: 600, color: RISK_COLORS[mode.risk_score > 0.6 ? 'high' : 'medium'] }}>{fmtPct(mode.risk_score)}</div>
                </div>
                <div>
                  <div style={{ fontSize: 11, color: 'var(--text2)', marginBottom: 2 }}>Уверенность</div>
                  <div style={{ fontSize: 16, fontWeight: 600 }}>{fmtPct(mode.confidence)}</div>
                </div>
              </div>
              {mode.primary_indicators.length > 0 && (
                <div style={{ marginTop: 10, fontSize: 13, color: 'var(--text2)' }}>
                  {mode.primary_indicators.join(' · ')}
                </div>
              )}
            </div>
          )}
        </>
      )}
    </div>
  );
}
