/**
 * Вспомогательные функции для BurnoutGuard Mobile
 */

/**
 * Форматирует скор риска (0.0-1.0) в читаемый процент.
 * @param {number} score
 * @returns {string}  "73%"
 */
export function formatRiskScore(score) {
  if (score == null || isNaN(score)) return '—';
  return `${Math.round(score * 100)}%`;
}

/**
 * Возвращает приветствие по времени суток.
 * @returns {string}
 */
export function getTimeOfDay() {
  const hour = new Date().getHours();
  if (hour < 6)  return 'Ночь';
  if (hour < 12) return 'Утро';
  if (hour < 17) return 'День';
  if (hour < 21) return 'Вечер';
  return 'Ночь';
}

/**
 * Возвращает цвет по уровню риска.
 * @param {string} level  very_low | low | medium | high | critical
 * @returns {string}  HEX
 */
export function getRiskLevelColor(level) {
  const map = {
    very_low: '#48bb78',
    low:      '#ecc94b',
    medium:   '#ed8936',
    high:     '#f56500',
    critical: '#e53e3e',
  };
  return map[level] || '#718096';
}

/**
 * Форматирует ISO-дату в короткий вид "27 мар".
 * @param {string} iso
 * @returns {string}
 */
export function formatShortDate(iso) {
  if (!iso) return '';
  const d = new Date(iso);
  return d.toLocaleDateString('ru-RU', { day: 'numeric', month: 'short' });
}

/**
 * Возвращает метку тренда.
 * @param {string} trend  improving | stable | worsening
 * @returns {{ label: string, color: string }}
 */
export function getTrendMeta(trend) {
  const map = {
    improving: { label: 'Улучшается', color: '#48bb78' },
    stable:    { label: 'Стабильно',  color: '#ed8936' },
    worsening: { label: 'Ухудшается', color: '#e53e3e' },
  };
  return map[trend] || map.stable;
}
