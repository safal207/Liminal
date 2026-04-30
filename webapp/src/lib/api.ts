const BASE = process.env.NEXT_PUBLIC_API_URL ?? 'http://localhost:8000';
const DEFAULT_USER = 'web_user';

async function req<T>(method: string, path: string, body?: unknown): Promise<T> {
  const res = await fetch(`${BASE}${path}`, {
    method,
    headers: { 'Content-Type': 'application/json' },
    body: body ? JSON.stringify(body) : undefined,
    cache: 'no-store',
  });
  if (!res.ok) {
    const text = await res.text();
    throw new Error(`API ${res.status}: ${text}`);
  }
  return res.json() as Promise<T>;
}

export const api = {
  assess: (userId = DEFAULT_USER, context?: Record<string, unknown>) =>
    req<AssessmentResult>('POST', `/api/v1/burnout/user/${userId}/assess`, { context }),

  progress: (userId = DEFAULT_USER, days = 30) =>
    req<ProgressResult>('GET', `/api/v1/burnout/user/${userId}/progress?days=${days}`),

  feedback: (userId = DEFAULT_USER, payload: FeedbackPayload) =>
    req<unknown>('POST', `/api/v1/burnout/user/${userId}/feedback`, payload),

  emotimeStatus: () =>
    req<EmotimeStatus>('GET', '/emotime/status'),
};

// ---- Types ----

export interface RiskAssessment {
  score: number;
  level: 'very_low' | 'low' | 'medium' | 'high' | 'critical';
  confidence: number;
  factors: Record<string, number>;
  emotional_indicators: string[];
  behavioral_patterns: string[];
  duration_risk: number;
  trend_risk: number;
}

export interface Recommendation {
  id: string;
  type: string;
  title: string;
  description: string;
  priority: number;
  estimated_time_minutes: number;
  difficulty: string;
  effectiveness_score: number;
}

export interface NextAction {
  action: string;
  urgency: string;
}

export interface BurnoutMode {
  type: string;
  risk_score: number;
  confidence: number;
  primary_indicators: string[];
}

export interface AssessmentResult {
  user_id: string;
  assessment_timestamp: string;
  status: 'assessed' | 'no_data';
  message?: string;
  risk_assessment?: RiskAssessment;
  burnout_mode?: BurnoutMode;
  mode_stability?: number;
  intervention_needed?: boolean;
  recommendations?: Recommendation[];
  next_actions?: NextAction[];
}

export interface ProgressPoint {
  timestamp: string;
  risk_score: number;
  risk_level: string;
  confidence: number;
}

export interface ProgressResult {
  user_id: string;
  period_days: number;
  generated_at: string;
  summary: {
    average_risk_score: number;
    trend: 'improving' | 'stable' | 'worsening';
    data_points: number;
    risk_distribution: Record<string, number>;
  };
  current_state: Record<string, unknown> | null;
  risk_history: ProgressPoint[];
}

export interface EmotimeStatus {
  status: string;
  mode?: { name: string } | string;
  features?: Record<string, number>;
  confidence?: number;
}

export interface FeedbackPayload {
  recommendation_id: string;
  action: 'viewed' | 'accepted' | 'dismissed' | 'completed';
  effectiveness_rating?: number;
}

// ---- Helpers ----

export const RISK_COLORS: Record<string, string> = {
  very_low: '#48bb78',
  low:      '#ecc94b',
  medium:   '#ed8936',
  high:     '#f56500',
  critical: '#e53e3e',
  unknown:  '#718096',
};

export const RISK_LABELS: Record<string, string> = {
  very_low: 'Очень низкий',
  low:      'Низкий',
  medium:   'Средний',
  high:     'Высокий',
  critical: 'Критический',
};

export const TREND_META: Record<string, { label: string; color: string }> = {
  improving: { label: 'Улучшается', color: '#48bb78' },
  stable:    { label: 'Стабильно',  color: '#ed8936' },
  worsening: { label: 'Ухудшается', color: '#e53e3e' },
};

export const EMOTIME_META: Record<string, { label: string; color: string; emoji: string }> = {
  calm:          { label: 'Спокойствие', color: '#4CAF50', emoji: '🌿' },
  focus:         { label: 'Фокус',       color: '#2196F3', emoji: '🎯' },
  stress:        { label: 'Стресс',      color: '#f44336', emoji: '⚡' },
  joy:           { label: 'Радость',     color: '#FFC107', emoji: '✨' },
  contemplation: { label: 'Созерцание',  color: '#9C27B0', emoji: '🌀' },
  neutral:       { label: 'Нейтральный', color: '#607D8B', emoji: '○'  },
};

export function parseEmotimeMode(status: EmotimeStatus): string {
  const raw = (typeof status.mode === 'object' ? status.mode?.name : status.mode) ?? '';
  return Object.keys(EMOTIME_META).find(k => raw.toLowerCase().includes(k)) ?? 'neutral';
}

export function fmtPct(v: number) { return `${Math.round(v * 100)}%`; }
export function fmtDate(iso: string) {
  return new Date(iso).toLocaleDateString('ru-RU', { day: 'numeric', month: 'short' });
}
