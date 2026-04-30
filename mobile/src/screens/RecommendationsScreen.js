/**
 * RecommendationsScreen — список персонализированных рекомендаций
 */

import React, { useContext } from 'react';
import { View, Text, ScrollView, StyleSheet, TouchableOpacity, Alert } from 'react-native';
import { Card, Chip } from 'react-native-paper';
import Icon from 'react-native-vector-icons/MaterialCommunityIcons';
import { BurnoutContext } from '../context/BurnoutContext';
import { ApiClient } from '../services/ApiClient';
import { theme } from '../theme/theme';

const TYPE_ICONS = {
  immediate:  'lightning-bolt',
  short_term: 'clock-fast',
  daily:      'calendar-check',
  weekly:     'calendar-week',
  lifestyle:  'heart-pulse',
};

const TYPE_LABELS = {
  immediate:  'Сейчас',
  short_term: 'Сегодня',
  daily:      'Ежедневно',
  weekly:     'На неделю',
  lifestyle:  'Образ жизни',
};

export default function RecommendationsScreen() {
  const { recommendations, refreshData } = useContext(BurnoutContext);

  const handleAction = async (rec, action) => {
    try {
      await ApiClient.submitUserFeedback({
        recommendation_id: rec.id,
        action,
        effectiveness_rating: action === 'completed' ? 0.9 : null,
      });
      if (action === 'completed') {
        Alert.alert('Отлично!', `"${rec.title}" выполнено. Так держать!`);
        refreshData();
      }
    } catch (err) {
      console.warn('Feedback error:', err.message);
    }
  };

  if (!recommendations.length) {
    return (
      <View style={styles.empty}>
        <Icon name="lightbulb-outline" size={48} color={theme.colors.textLight} />
        <Text style={styles.emptyText}>Нет рекомендаций. Выполните оценку на Dashboard.</Text>
      </View>
    );
  }

  return (
    <ScrollView style={styles.container} contentContainerStyle={styles.content}>
      <Text style={styles.title}>Рекомендации</Text>
      {recommendations.map((rec, i) => (
        <Card key={rec.id || i} style={styles.card}>
          <Card.Content>
            <View style={styles.header}>
              <Icon
                name={TYPE_ICONS[rec.type] || 'lightbulb'}
                size={20}
                color={theme.colors.primary}
              />
              <Chip
                style={styles.chip}
                textStyle={styles.chipText}
              >
                {TYPE_LABELS[rec.type] || rec.type}
              </Chip>
              <Chip style={styles.priorityChip} textStyle={styles.chipText}>
                P{rec.priority}
              </Chip>
            </View>

            <Text style={styles.recTitle}>{rec.title}</Text>
            <Text style={styles.recDesc}>{rec.description}</Text>

            <View style={styles.meta}>
              <Icon name="clock-outline" size={14} color={theme.colors.textSecondary} />
              <Text style={styles.metaText}>{rec.estimated_time_minutes} мин</Text>
              <Icon name="signal" size={14} color={theme.colors.textSecondary} style={{ marginLeft: 12 }} />
              <Text style={styles.metaText}>{rec.difficulty}</Text>
            </View>

            <View style={styles.actions}>
              <TouchableOpacity
                style={[styles.btn, styles.btnOutline]}
                onPress={() => handleAction(rec, 'dismissed')}
              >
                <Text style={styles.btnOutlineText}>Пропустить</Text>
              </TouchableOpacity>
              <TouchableOpacity
                style={[styles.btn, styles.btnPrimary]}
                onPress={() => handleAction(rec, 'completed')}
              >
                <Text style={styles.btnPrimaryText}>Выполнено</Text>
              </TouchableOpacity>
            </View>
          </Card.Content>
        </Card>
      ))}
    </ScrollView>
  );
}

const styles = StyleSheet.create({
  container: { flex: 1, backgroundColor: theme.colors.background },
  content: { padding: 16, paddingBottom: 32 },
  empty: { flex: 1, justifyContent: 'center', alignItems: 'center', padding: 32, gap: 12 },
  emptyText: { color: theme.colors.textSecondary, textAlign: 'center', lineHeight: 22 },
  title: { fontSize: 20, fontWeight: '700', color: theme.colors.text, marginBottom: 16 },
  card: { marginBottom: 12, borderRadius: 12 },
  header: { flexDirection: 'row', alignItems: 'center', gap: 8, marginBottom: 8 },
  chip: { backgroundColor: '#ede9fe', height: 24 },
  priorityChip: { backgroundColor: '#fef3c7', height: 24 },
  chipText: { fontSize: 11 },
  recTitle: { fontSize: 16, fontWeight: '600', color: theme.colors.text, marginBottom: 4 },
  recDesc: { fontSize: 14, color: theme.colors.textSecondary, lineHeight: 20, marginBottom: 8 },
  meta: { flexDirection: 'row', alignItems: 'center', gap: 4, marginBottom: 12 },
  metaText: { fontSize: 12, color: theme.colors.textSecondary },
  actions: { flexDirection: 'row', gap: 8 },
  btn: { flex: 1, paddingVertical: 8, borderRadius: 8, alignItems: 'center' },
  btnOutline: { borderWidth: 1, borderColor: theme.colors.border },
  btnOutlineText: { color: theme.colors.textSecondary, fontSize: 14 },
  btnPrimary: { backgroundColor: theme.colors.primary },
  btnPrimaryText: { color: '#fff', fontSize: 14, fontWeight: '600' },
});
