'use client';

import { useEffect, useState, useCallback } from 'react';
import {
  api, AssessmentResult, EmotimeStatus,
  RISK_COLORS, RISK_LABELS, EMOTIME_META, parseEmotimeMode, fmtPct,
} from '@/lib/api';

const CARD: React.CSSProperties = {
  background: 'var(--surface)',
  border: '1px solid var(--border)',
  borderRadius: 'var(--radius)',
  padding: 20,
};

export default function Dashboard() {
  const [assessment, setAssessment]   = useState<AssessmentResult | null>(null);
  const [emotime, setEmotime]         = useState<EmotimeStatus | null>(null);
  const [loading, setLoading]         = useState(false);
  const [error, setError]             = useState<string | null>(null);
  const [lastUpdated, setLastUpdated] = useState<string>('');

  const fetchAll = useCallback(async () => {
    setLoading(true);
    setError(null);
    try {
      const [a, e] = await Promise.allSettled([api.assess(), api.emotimeStatus()]);
      if (a.status === 'fulfilled') setAssessment(a.value);
      else setError('Не удалось получить оценку. Проверьте, запущен ли backend.');
      if (e.status === 'fulfilled') setEmotime(e.value);
      setLastUpdated(new Date().toLocaleTimeString('ru-RU'));
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => { fetchAll(); }, [fetchAll]);

  const risk = assessment?.risk_assessment;
  const level = risk?.level ?? 'unknown';
  const riskColor = RISK_COLORS[level] ?? RISK_COLORS.unknown;
  const emotimeMode = emotime ? parseEmotimeMode(emotime) : null;
  const emotimeMeta = emotimeMode ? EMOTIME_META[emotimeMode] : null;

  return (
    <div style={{ padding: 28 }}>
      {/* Header */}
      <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: 24 }}>
        <div>
          <h1 style={{ fontSize: 22, fontWeight: 700 }}>Dashboard</h1>
          {lastUpdated && <p style={{ fontSize: 12, color: 'var(--text2)', marginTop: 2 }}>Обновлено: {lastUpdated}</p>}
        </div>
        <button
          onClick={fetchAll}
          disabled={loading}
          style={{
            background: 'var(--primary)', color: '#fff', border: 'none',
            borderRadius: 8, padding: '8px 18px', cursor: loading ? 'not-allowed' : 'pointer',
            fontSize: 14, fontWeight: 600, opacity: loading ? 0.7 : 1,
          }}
        >
          {loading ? 'Загрузка…' : 'Обновить'}
        </button>
      </div>

      {error && (
        <div style={{ ...CARD, borderColor: '#f44336', color: '#f44336', marginBottom: 20, fontSize: 14 }}>
          {error}
        </div>
      )}

      {/* Top row */}
      <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fill, minmax(200px, 1fr))', gap: 16, marginBottom: 20 }}>
        {/* Risk score */}
        <div style={{ ...CARD, borderLeft: `3px solid ${riskColor}` }}>
          <div style={{ fontSize: 11, color: 'var(--text2)', marginBottom: 6, textTransform: 'uppercase', letterSpacing: 0.5 }}>Риск выгорания</div>
          <div style={{ fontSize: 36, fontWeight: 800, color: riskColor, lineHeight: 1 }}>
            {risk ? fmtPct(risk.score) : '—'}
          </div>
          <div style={{ fontSize: 13, color: riskColor, marginTop: 4 }}>
            {risk ? RISK_LABELS[level] : 'Нет данных'}
          </div>
        </div>

        {/* Confidence */}
        <div style={CARD}>
          <div style={{ fontSize: 11, color: 'var(--text2)', marginBottom: 6, textTransform: 'uppercase', letterSpacing: 0.5 }}>Уверенность</div>
          <div style={{ fontSize: 36, fontWeight: 800, lineHeight: 1 }}>
            {risk ? fmtPct(risk.confidence) : '—'}
          </div>
        </div>

        {/* Emotime mode */}
        <div style={{ ...CARD, borderLeft: `3px solid ${emotimeMeta?.color ?? 'var(--border)'}` }}>
          <div style={{ fontSize: 11, color: 'var(--text2)', marginBottom: 6, textTransform: 'uppercase', letterSpacing: 0.5 }}>Emotime режим</div>
          <div style={{ fontSize: 28, lineHeight: 1 }}>{emotimeMeta?.emoji ?? '—'}</div>
          <div style={{ fontSize: 14, fontWeight: 600, color: emotimeMeta?.color ?? 'var(--text2)', marginTop: 4 }}>
            {emotimeMeta?.label ?? 'Недоступен'}
          </div>
        </div>

        {/* Intervention */}
        <div style={{ ...CARD, borderLeft: `3px solid ${assessment?.intervention_needed ? '#f44336' : '#48bb78'}` }}>
          <div style={{ fontSize: 11, color: 'var(--text2)', marginBottom: 6, textTransform: 'uppercase', letterSpacing: 0.5 }}>Вмешательство</div>
          <div style={{ fontSize: 24, fontWeight: 700, color: assessment?.intervention_needed ? '#f44336' : '#48bb78' }}>
            {assessment == null ? '—' : assessment.intervention_needed ? 'Требуется' : 'Не нужно'}
          </div>
        </div>
      </div>

      {/* Risk factors */}
      {risk?.factors && (
        <div style={{ ...CARD, marginBottom: 20 }}>
          <h2 style={{ fontSize: 15, fontWeight: 600, marginBottom: 16 }}>Факторы риска</h2>
          <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fill, minmax(260px, 1fr))', gap: 12 }}>
            {Object.entries(risk.factors).map(([key, val]) => (
              <div key={key}>
                <div style={{ display: 'flex', justifyContent: 'space-between', fontSize: 13, marginBottom: 4 }}>
                  <span style={{ color: 'var(--text2)' }}>{key.replace(/_/g, ' ')}</span>
                  <span style={{ fontWeight: 600, color: RISK_COLORS[val > 0.6 ? 'high' : val > 0.4 ? 'medium' : 'low'] }}>{fmtPct(val)}</span>
                </div>
                <div style={{ height: 6, background: 'var(--border)', borderRadius: 3, overflow: 'hidden' }}>
                  <div style={{ height: '100%', width: fmtPct(val), background: val > 0.6 ? '#f44336' : 'var(--primary)', borderRadius: 3, transition: 'width .4s' }} />
                </div>
              </div>
            ))}
          </div>
        </div>
      )}

      {/* Next actions + top recommendations */}
      <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: 16 }}>
        {/* Next actions */}
        {(assessment?.next_actions ?? []).length > 0 && (
          <div style={CARD}>
            <h2 style={{ fontSize: 15, fontWeight: 600, marginBottom: 14 }}>Следующие шаги</h2>
            {assessment!.next_actions!.map((a, i) => (
              <div key={i} style={{ display: 'flex', gap: 10, marginBottom: 10, alignItems: 'flex-start' }}>
                <span style={{
                  fontSize: 10, fontWeight: 700, textTransform: 'uppercase', letterSpacing: 0.5,
                  color: a.urgency === 'immediate' ? '#f44336' : a.urgency === 'high' ? '#ed8936' : 'var(--text2)',
                  paddingTop: 2, minWidth: 64,
                }}>{a.urgency}</span>
                <span style={{ fontSize: 13, color: 'var(--text)', lineHeight: 1.4 }}>{a.action}</span>
              </div>
            ))}
          </div>
        )}

        {/* Top 2 recommendations */}
        {(assessment?.recommendations ?? []).length > 0 && (
          <div style={CARD}>
            <h2 style={{ fontSize: 15, fontWeight: 600, marginBottom: 14 }}>Топ рекомендации</h2>
            {assessment!.recommendations!.slice(0, 2).map((r, i) => (
              <div key={i} style={{ marginBottom: 12, paddingBottom: 12, borderBottom: i < 1 ? '1px solid var(--border)' : 'none' }}>
                <div style={{ fontSize: 13, fontWeight: 600, marginBottom: 2 }}>{r.title}</div>
                <div style={{ fontSize: 12, color: 'var(--text2)', lineHeight: 1.4 }}>{r.description}</div>
              </div>
            ))}
          </div>
        )}
      </div>

      {assessment?.status === 'no_data' && (
        <div style={{ ...CARD, textAlign: 'center', color: 'var(--text2)', marginTop: 20 }}>
          <p style={{ fontSize: 15, marginBottom: 6 }}>Нет данных для оценки</p>
          <p style={{ fontSize: 13 }}>Сначала отправьте данные через Emotime API (/emotime/text)</p>
        </div>
      )}
    </div>
  );
}
