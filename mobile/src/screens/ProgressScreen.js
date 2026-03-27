/**
 * ProgressScreen — история риска выгорания пользователя
 * Потребляет GET /api/v1/burnout/user/{id}/progress
 */

import React, { useContext, useEffect, useState } from 'react';
import { View, Text, ScrollView, StyleSheet, ActivityIndicator } from 'react-native';
import { Card } from 'react-native-paper';
import { BurnoutContext } from '../context/BurnoutContext';
import { theme } from '../theme/theme';
import { formatRiskScore, formatShortDate, getTrendMeta, getRiskLevelColor } from '../utils/helpers';

export default function ProgressScreen() {
  const { progress, refreshData } = useContext(BurnoutContext);
  const [loading, setLoading] = useState(!progress);

  useEffect(() => {
    if (!progress) {
      refreshData().finally(() => setLoading(false));
    } else {
      setLoading(false);
    }
  }, []);

  if (loading) {
    return (
      <View style={styles.centered}>
        <ActivityIndicator size="large" color={theme.colors.primary} />
        <Text style={styles.loadingText}>Загрузка прогресса...</Text>
      </View>
    );
  }

  if (!progress) {
    return (
      <View style={styles.centered}>
        <Text style={styles.errorText}>Нет данных. Сначала выполните оценку.</Text>
      </View>
    );
  }

  const summary = progress.summary || {};
  const history = progress.risk_history || [];
  const trendMeta = getTrendMeta(summary.trend);

  return (
    <ScrollView style={styles.container} contentContainerStyle={styles.content}>
      <Text style={styles.title}>Прогресс за {progress.period_days} дней</Text>

      {/* Summary cards */}
      <View style={styles.row}>
        <Card style={[styles.card, styles.cardHalf]}>
          <Card.Content>
            <Text style={styles.cardLabel}>Средний риск</Text>
            <Text style={[styles.cardValue, { color: theme.colors.primary }]}>
              {formatRiskScore(summary.average_risk_score)}
            </Text>
          </Card.Content>
        </Card>

        <Card style={[styles.card, styles.cardHalf]}>
          <Card.Content>
            <Text style={styles.cardLabel}>Тренд</Text>
            <Text style={[styles.cardValue, { color: trendMeta.color }]}>
              {trendMeta.label}
            </Text>
          </Card.Content>
        </Card>
      </View>

      {/* Distribution */}
      {summary.risk_distribution && (
        <Card style={styles.card}>
          <Card.Content>
            <Text style={styles.sectionTitle}>Распределение уровней</Text>
            {Object.entries(summary.risk_distribution).map(([lvl, count]) => (
              <View key={lvl} style={styles.distRow}>
                <Text style={[styles.distLabel, { color: getRiskLevelColor(lvl) }]}>
                  {lvl.replace('_', ' ')}
                </Text>
                <View style={styles.distBarBg}>
                  <View
                    style={[
                      styles.distBar,
                      {
                        width: `${Math.round((count / (summary.data_points || 1)) * 100)}%`,
                        backgroundColor: getRiskLevelColor(lvl),
                      },
                    ]}
                  />
                </View>
                <Text style={styles.distCount}>{count}</Text>
              </View>
            ))}
          </Card.Content>
        </Card>
      )}

      {/* History list */}
      <Card style={styles.card}>
        <Card.Content>
          <Text style={styles.sectionTitle}>История</Text>
          {history.slice(-20).reverse().map((pt, i) => (
            <View key={i} style={styles.histRow}>
              <Text style={styles.histDate}>{formatShortDate(pt.timestamp)}</Text>
              <View style={styles.histBar}>
                <View
                  style={[
                    styles.histFill,
                    {
                      width: `${Math.round(pt.risk_score * 100)}%`,
                      backgroundColor: getRiskLevelColor(pt.risk_level),
                    },
                  ]}
                />
              </View>
              <Text style={[styles.histScore, { color: getRiskLevelColor(pt.risk_level) }]}>
                {formatRiskScore(pt.risk_score)}
              </Text>
            </View>
          ))}
        </Card.Content>
      </Card>
    </ScrollView>
  );
}

const styles = StyleSheet.create({
  container: { flex: 1, backgroundColor: theme.colors.background },
  content: { padding: 16, paddingBottom: 32 },
  centered: { flex: 1, justifyContent: 'center', alignItems: 'center', padding: 24 },
  loadingText: { marginTop: 12, color: theme.colors.textSecondary },
  errorText: { color: theme.colors.textSecondary, textAlign: 'center' },
  title: { fontSize: 20, fontWeight: '700', color: theme.colors.text, marginBottom: 16 },
  row: { flexDirection: 'row', gap: 12, marginBottom: 12 },
  card: { marginBottom: 12, borderRadius: 12 },
  cardHalf: { flex: 1 },
  cardLabel: { fontSize: 12, color: theme.colors.textSecondary, marginBottom: 4 },
  cardValue: { fontSize: 22, fontWeight: '700' },
  sectionTitle: { fontSize: 15, fontWeight: '600', color: theme.colors.text, marginBottom: 12 },
  distRow: { flexDirection: 'row', alignItems: 'center', marginBottom: 8, gap: 8 },
  distLabel: { width: 80, fontSize: 12, fontWeight: '500', textTransform: 'capitalize' },
  distBarBg: { flex: 1, height: 10, backgroundColor: '#e2e8f0', borderRadius: 5, overflow: 'hidden' },
  distBar: { height: '100%', borderRadius: 5 },
  distCount: { width: 28, fontSize: 12, color: theme.colors.textSecondary, textAlign: 'right' },
  histRow: { flexDirection: 'row', alignItems: 'center', marginBottom: 6, gap: 8 },
  histDate: { width: 52, fontSize: 12, color: theme.colors.textSecondary },
  histBar: { flex: 1, height: 8, backgroundColor: '#e2e8f0', borderRadius: 4, overflow: 'hidden' },
  histFill: { height: '100%', borderRadius: 4 },
  histScore: { width: 40, fontSize: 12, fontWeight: '600', textAlign: 'right' },
});
