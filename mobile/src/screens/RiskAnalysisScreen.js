/**
 * RiskAnalysisScreen — детальный анализ факторов риска
 */

import React, { useContext } from 'react';
import { View, Text, ScrollView, StyleSheet } from 'react-native';
import { Card, ProgressBar } from 'react-native-paper';
import { BurnoutContext } from '../context/BurnoutContext';
import { theme } from '../theme/theme';
import { formatRiskScore, getRiskLevelColor } from '../utils/helpers';

const FACTOR_LABELS = {
  emotional_state:      'Эмоциональное состояние',
  behavioral_patterns:  'Поведенческие паттерны',
  temporal_analysis:    'Временной анализ',
  ml_confidence:        'ML уверенность',
};

export default function RiskAnalysisScreen() {
  const { currentState, riskAssessment } = useContext(BurnoutContext);

  if (!riskAssessment) {
    return (
      <View style={styles.empty}>
        <Text style={styles.emptyText}>Нет данных для анализа. Обновите Dashboard.</Text>
      </View>
    );
  }

  const levelColor = getRiskLevelColor(riskAssessment.level);

  return (
    <ScrollView style={styles.container} contentContainerStyle={styles.content}>
      <Text style={styles.title}>Анализ риска</Text>

      {/* Overall score */}
      <Card style={styles.card}>
        <Card.Content>
          <Text style={styles.cardLabel}>Общий скор</Text>
          <Text style={[styles.bigScore, { color: levelColor }]}>
            {formatRiskScore(riskAssessment.score)}
          </Text>
          <Text style={[styles.levelBadge, { color: levelColor }]}>
            {riskAssessment.level?.replace('_', ' ').toUpperCase()}
          </Text>
          <Text style={styles.confidence}>
            Уверенность: {formatRiskScore(riskAssessment.confidence)}
          </Text>
        </Card.Content>
      </Card>

      {/* Factors */}
      <Card style={styles.card}>
        <Card.Content>
          <Text style={styles.sectionTitle}>Факторы риска</Text>
          {Object.entries(riskAssessment.factors || {}).map(([key, val]) => (
            <View key={key} style={styles.factorRow}>
              <Text style={styles.factorLabel}>{FACTOR_LABELS[key] || key}</Text>
              <ProgressBar
                progress={val}
                color={val > 0.6 ? theme.colors.error : theme.colors.primary}
                style={styles.bar}
              />
              <Text style={styles.factorVal}>{formatRiskScore(val)}</Text>
            </View>
          ))}
        </Card.Content>
      </Card>

      {/* Emotional indicators */}
      {(riskAssessment.emotional_indicators || []).length > 0 && (
        <Card style={styles.card}>
          <Card.Content>
            <Text style={styles.sectionTitle}>Эмоциональные индикаторы</Text>
            {riskAssessment.emotional_indicators.map((ind, i) => (
              <Text key={i} style={styles.listItem}>• {ind}</Text>
            ))}
          </Card.Content>
        </Card>
      )}

      {/* Behavioral patterns */}
      {(riskAssessment.behavioral_patterns || []).length > 0 && (
        <Card style={styles.card}>
          <Card.Content>
            <Text style={styles.sectionTitle}>Поведенческие паттерны</Text>
            {riskAssessment.behavioral_patterns.map((p, i) => (
              <Text key={i} style={styles.listItem}>• {p}</Text>
            ))}
          </Card.Content>
        </Card>
      )}

      {/* Next actions */}
      {(currentState?.next_actions || []).length > 0 && (
        <Card style={styles.card}>
          <Card.Content>
            <Text style={styles.sectionTitle}>Следующие шаги</Text>
            {currentState.next_actions.map((a, i) => (
              <View key={i} style={styles.actionRow}>
                <Text style={[styles.urgency, { color: a.urgency === 'immediate' ? theme.colors.error : theme.colors.warning }]}>
                  {a.urgency}
                </Text>
                <Text style={styles.actionText}>{a.action}</Text>
              </View>
            ))}
          </Card.Content>
        </Card>
      )}
    </ScrollView>
  );
}

const styles = StyleSheet.create({
  container: { flex: 1, backgroundColor: theme.colors.background },
  content: { padding: 16, paddingBottom: 32 },
  empty: { flex: 1, justifyContent: 'center', alignItems: 'center', padding: 32 },
  emptyText: { color: theme.colors.textSecondary, textAlign: 'center' },
  title: { fontSize: 20, fontWeight: '700', color: theme.colors.text, marginBottom: 16 },
  card: { marginBottom: 12, borderRadius: 12 },
  cardLabel: { fontSize: 12, color: theme.colors.textSecondary, marginBottom: 4 },
  bigScore: { fontSize: 48, fontWeight: '800', lineHeight: 56 },
  levelBadge: { fontSize: 16, fontWeight: '600', marginBottom: 4 },
  confidence: { fontSize: 13, color: theme.colors.textSecondary },
  sectionTitle: { fontSize: 15, fontWeight: '600', color: theme.colors.text, marginBottom: 12 },
  factorRow: { marginBottom: 10 },
  factorLabel: { fontSize: 13, color: theme.colors.textSecondary, marginBottom: 4 },
  bar: { height: 8, borderRadius: 4, marginBottom: 2 },
  factorVal: { fontSize: 12, color: theme.colors.textSecondary, textAlign: 'right' },
  listItem: { fontSize: 14, color: theme.colors.text, lineHeight: 22 },
  actionRow: { flexDirection: 'row', gap: 8, marginBottom: 8, alignItems: 'flex-start' },
  urgency: { fontSize: 11, fontWeight: '600', textTransform: 'uppercase', paddingTop: 2, width: 70 },
  actionText: { flex: 1, fontSize: 14, color: theme.colors.text, lineHeight: 20 },
});
