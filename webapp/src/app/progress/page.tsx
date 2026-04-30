'use client';

import { useEffect, useState, useRef } from 'react';
import { api, ProgressResult, RISK_COLORS, TREND_META, fmtPct, fmtDate } from '@/lib/api';

const CARD: React.CSSProperties = {
  background: 'var(--surface)',
  border: '1px solid var(--border)',
  borderRadius: 'var(--radius)',
  padding: 20,
  marginBottom: 16,
};

const LEVEL_ORDER = ['very_low', 'low', 'medium', 'high', 'critical'];
const LEVEL_RU: Record<string, string> = { very_low: 'Очень низкий', low: 'Низкий', medium: 'Средний', high: 'Высокий', critical: 'Критический' };

function RiskChart({ history }: { history: ProgressResult['risk_history'] }) {
  const ref = useRef<HTMLCanvasElement>(null);

  useEffect(() => {
    const canvas = ref.current;
    if (!canvas || !history.length) return;
    const ctx = canvas.getContext('2d')!;
    const W = canvas.parentElement!.clientWidth - 40;
    const H = 120;
    canvas.width = W; canvas.height = H;
    const PAD = { t: 10, r: 10, b: 24, l: 36 };
    const cW = W - PAD.l - PAD.r, cH = H - PAD.t - PAD.b;

    ctx.clearRect(0, 0, W, H);

    // grid
    ctx.strokeStyle = '#2a2a3e'; ctx.lineWidth = 1;
    for (let i = 0; i <= 4; i++) {
      const y = PAD.t + (cH / 4) * i;
      ctx.beginPath(); ctx.moveTo(PAD.l, y); ctx.lineTo(W - PAD.r, y); ctx.stroke();
      ctx.fillStyle = '#666'; ctx.font = '10px sans-serif'; ctx.textAlign = 'right';
      ctx.fillText((1 - i / 4).toFixed(1), PAD.l - 4, y + 4);
    }

    const step = cW / Math.max(history.length - 1, 1);

    // fill
    const g = ctx.createLinearGradient(0, PAD.t, 0, PAD.t + cH);
    g.addColorStop(0, 'rgba(156,39,176,.45)'); g.addColorStop(1, 'rgba(156,39,176,0)');
    ctx.fillStyle = g;
    ctx.beginPath();
    history.forEach((p, i) => { const x = PAD.l + i * step, y = PAD.t + cH * (1 - p.risk_score); i === 0 ? ctx.moveTo(x, y) : ctx.lineTo(x, y); });
    ctx.lineTo(PAD.l + (history.length - 1) * step, PAD.t + cH);
    ctx.lineTo(PAD.l, PAD.t + cH); ctx.closePath(); ctx.fill();

    // line
    ctx.strokeStyle = '#9c27b0'; ctx.lineWidth = 2; ctx.lineJoin = 'round';
    ctx.beginPath();
    history.forEach((p, i) => { const x = PAD.l + i * step, y = PAD.t + cH * (1 - p.risk_score); i === 0 ? ctx.moveTo(x, y) : ctx.lineTo(x, y); });
    ctx.stroke();

    // dots
    history.forEach((p, i) => {
      const x = PAD.l + i * step, y = PAD.t + cH * (1 - p.risk_score);
      ctx.fillStyle = RISK_COLORS[p.risk_level] ?? '#607D8B';
      ctx.beginPath(); ctx.arc(x, y, 3, 0, Math.PI * 2); ctx.fill();
    });

    // x labels
    ctx.fillStyle = '#666'; ctx.font = '10px sans-serif'; ctx.textAlign = 'center';
    [[0, history[0]], [Math.floor(history.length / 2), history[Math.floor(history.length / 2)]], [history.length - 1, history[history.length - 1]]]
      .forEach(([i, p]) => { if (!p) return; ctx.fillText(fmtDate((p as ProgressResult['risk_history'][0]).timestamp), PAD.l + (i as number) * step, H - 4); });
  }, [history]);

  return <canvas ref={ref} style={{ display: 'block' }} />;
}

export default function ProgressPage() {
  const [data, setData]         = useState<ProgressResult | null>(null);
  const [days, setDays]         = useState(30);
  const [loading, setLoading]   = useState(false);

  const load = async (d: number) => {
    setLoading(true);
    try { setData(await api.progress(undefined, d)); } catch { setData(null); } finally { setLoading(false); }
  };

  useEffect(() => { load(days); }, []);

  const summary = data?.summary;
  const trendMeta = summary ? TREND_META[summary.trend] : null;
  const total = summary ? Object.values(summary.risk_distribution).reduce((a, b) => a + b, 0) || 1 : 1;

  return (
    <div style={{ padding: 28 }}>
      <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: 24 }}>
        <h1 style={{ fontSize: 22, fontWeight: 700 }}>Прогресс</h1>
        <div style={{ display: 'flex', gap: 8, alignItems: 'center' }}>
          {([7, 14, 30, 90] as const).map(d => (
            <button key={d} onClick={() => { setDays(d); load(d); }} style={{
              padding: '5px 12px', borderRadius: 6, border: '1px solid var(--border)',
              background: days === d ? 'var(--primary)' : 'var(--surface)', color: days === d ? '#fff' : 'var(--text2)',
              cursor: 'pointer', fontSize: 13,
            }}>{d}д</button>
          ))}
        </div>
      </div>

      {loading && <p style={{ color: 'var(--text2)' }}>Загрузка…</p>}

      {!loading && !data && (
        <div style={{ ...CARD, color: 'var(--text2)', textAlign: 'center' }}>Нет данных. Сначала выполните оценку на Dashboard.</div>
      )}

      {data && summary && (
        <>
          {/* Summary cards */}
          <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fill, minmax(160px, 1fr))', gap: 16, marginBottom: 16 }}>
            <div style={CARD}>
              <div style={{ fontSize: 11, color: 'var(--text2)', marginBottom: 4, textTransform: 'uppercase' }}>Средний риск</div>
              <div style={{ fontSize: 30, fontWeight: 800, color: 'var(--primary)' }}>{fmtPct(summary.average_risk_score)}</div>
            </div>
            <div style={CARD}>
              <div style={{ fontSize: 11, color: 'var(--text2)', marginBottom: 4, textTransform: 'uppercase' }}>Тренд</div>
              <div style={{ fontSize: 20, fontWeight: 700, color: trendMeta?.color }}>{trendMeta?.label}</div>
            </div>
            <div style={CARD}>
              <div style={{ fontSize: 11, color: 'var(--text2)', marginBottom: 4, textTransform: 'uppercase' }}>Точек данных</div>
              <div style={{ fontSize: 30, fontWeight: 800 }}>{summary.data_points}</div>
            </div>
          </div>

          {/* Chart */}
          <div style={CARD}>
            <h2 style={{ fontSize: 15, fontWeight: 600, marginBottom: 16 }}>Скор риска во времени</h2>
            {data.risk_history.length > 0 ? <RiskChart history={data.risk_history} /> : <p style={{ color: 'var(--text2)', fontSize: 13 }}>Нет истории</p>}
          </div>

          {/* Distribution */}
          <div style={CARD}>
            <h2 style={{ fontSize: 15, fontWeight: 600, marginBottom: 16 }}>Распределение уровней риска</h2>
            {LEVEL_ORDER.map(lvl => {
              const count = summary.risk_distribution[lvl] ?? 0;
              const pct = (count / total * 100).toFixed(1);
              return (
                <div key={lvl} style={{ display: 'flex', alignItems: 'center', gap: 10, marginBottom: 10 }}>
                  <div style={{ width: 96, fontSize: 12, color: RISK_COLORS[lvl], fontWeight: 500, textAlign: 'right' }}>{LEVEL_RU[lvl]}</div>
                  <div style={{ flex: 1, height: 10, background: 'var(--surface2)', borderRadius: 5, overflow: 'hidden' }}>
                    <div style={{ height: '100%', width: `${pct}%`, background: RISK_COLORS[lvl], borderRadius: 5, transition: 'width .4s' }} />
                  </div>
                  <div style={{ width: 52, fontSize: 12, color: 'var(--text2)' }}>{pct}% ({count})</div>
                </div>
              );
            })}
          </div>
        </>
      )}
    </div>
  );
}
