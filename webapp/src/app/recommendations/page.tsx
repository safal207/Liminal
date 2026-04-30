'use client';

import { useEffect, useState } from 'react';
import { api, AssessmentResult, Recommendation, RISK_COLORS, fmtPct } from '@/lib/api';

const CARD: React.CSSProperties = {
  background: 'var(--surface)',
  border: '1px solid var(--border)',
  borderRadius: 'var(--radius)',
  padding: 20,
  marginBottom: 12,
};

const TYPE_ICONS: Record<string, string> = {
  immediate:  '⚡',
  short_term: '⏱️',
  daily:      '📅',
  weekly:     '🗓️',
  lifestyle:  '💚',
};

const TYPE_RU: Record<string, string> = {
  immediate:  'Сейчас',
  short_term: 'Сегодня',
  daily:      'Ежедневно',
  weekly:     'На неделю',
  lifestyle:  'Образ жизни',
};

type Action = 'dismissed' | 'completed';

export default function RecommendationsPage() {
  const [data, setData]       = useState<AssessmentResult | null>(null);
  const [loading, setLoading] = useState(false);
  const [done, setDone]       = useState<Set<string>>(new Set());
  const [skipped, setSkipped] = useState<Set<string>>(new Set());

  const load = async () => {
    setLoading(true);
    try { setData(await api.assess()); } catch { setData(null); } finally { setLoading(false); }
  };

  useEffect(() => { load(); }, []);

  const handleAction = async (rec: Recommendation, action: Action) => {
    try {
      await api.feedback(undefined, {
        recommendation_id: rec.id,
        action,
        effectiveness_rating: action === 'completed' ? 0.9 : undefined,
      });
      if (action === 'completed') setDone(s => new Set([...s, rec.id]));
      else setSkipped(s => new Set([...s, rec.id]));
    } catch { /* non-critical */ }
  };

  const recs = (data?.recommendations ?? []).filter(r => !skipped.has(r.id));

  return (
    <div style={{ padding: 28 }}>
      <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: 24 }}>
        <h1 style={{ fontSize: 22, fontWeight: 700 }}>Рекомендации</h1>
        <button onClick={load} disabled={loading} style={{
          background: 'var(--primary)', color: '#fff', border: 'none',
          borderRadius: 8, padding: '8px 18px', cursor: 'pointer', fontSize: 14, fontWeight: 600, opacity: loading ? 0.7 : 1,
        }}>{loading ? 'Загрузка…' : 'Обновить'}</button>
      </div>

      {!loading && recs.length === 0 && (
        <div style={{ ...CARD, textAlign: 'center', color: 'var(--text2)' }}>
          {data?.status === 'no_data' ? 'Нет данных. Сначала отправьте данные через Emotime.' : 'Все рекомендации выполнены или скрыты. Нажмите «Обновить».'}
        </div>
      )}

      {recs.map(rec => {
        const isDone = done.has(rec.id);
        return (
          <div key={rec.id} style={{ ...CARD, opacity: isDone ? 0.5 : 1, borderLeft: isDone ? '3px solid #48bb78' : `3px solid var(--border)` }}>
            <div style={{ display: 'flex', gap: 8, alignItems: 'center', marginBottom: 8 }}>
              <span style={{ fontSize: 18 }}>{TYPE_ICONS[rec.type] ?? '💡'}</span>
              <span style={{ fontSize: 11, fontWeight: 600, textTransform: 'uppercase', letterSpacing: 0.5, color: 'var(--text2)' }}>
                {TYPE_RU[rec.type] ?? rec.type}
              </span>
              <span style={{
                marginLeft: 'auto', fontSize: 11, fontWeight: 700, padding: '2px 8px',
                borderRadius: 4, background: 'var(--surface2)', color: 'var(--text2)',
              }}>P{rec.priority}</span>
            </div>

            <h3 style={{ fontSize: 15, fontWeight: 600, marginBottom: 4 }}>{rec.title}</h3>
            <p style={{ fontSize: 13, color: 'var(--text2)', lineHeight: 1.5, marginBottom: 12 }}>{rec.description}</p>

            <div style={{ display: 'flex', gap: 12, alignItems: 'center', marginBottom: 12 }}>
              <span style={{ fontSize: 12, color: 'var(--text2)' }}>⏱ {rec.estimated_time_minutes} мин</span>
              <span style={{ fontSize: 12, color: 'var(--text2)' }}>· {rec.difficulty}</span>
              <span style={{ fontSize: 12, color: '#48bb78' }}>· эффективность {fmtPct(rec.effectiveness_score)}</span>
            </div>

            {!isDone && (
              <div style={{ display: 'flex', gap: 8 }}>
                <button onClick={() => handleAction(rec, 'dismissed')} style={{
                  flex: 1, padding: '7px 0', borderRadius: 7, border: '1px solid var(--border)',
                  background: 'transparent', color: 'var(--text2)', cursor: 'pointer', fontSize: 13,
                }}>Пропустить</button>
                <button onClick={() => handleAction(rec, 'completed')} style={{
                  flex: 1, padding: '7px 0', borderRadius: 7, border: 'none',
                  background: 'var(--primary)', color: '#fff', cursor: 'pointer', fontSize: 13, fontWeight: 600,
                }}>Выполнено ✓</button>
              </div>
            )}

            {isDone && (
              <div style={{ fontSize: 13, color: '#48bb78', fontWeight: 600 }}>✓ Выполнено</div>
            )}
          </div>
        );
      })}
    </div>
  );
}
