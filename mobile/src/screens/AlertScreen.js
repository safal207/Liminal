/**
 * AlertScreen — модальный экран при критическом/высоком риске
 */

import React, { useContext } from 'react';
import { View, Text, StyleSheet, TouchableOpacity, ScrollView } from 'react-native';
import Icon from 'react-native-vector-icons/MaterialCommunityIcons';
import { BurnoutContext } from '../context/BurnoutContext';
import { ApiClient } from '../services/ApiClient';
import { theme } from '../theme/theme';
import { formatRiskScore, getRiskLevelColor } from '../utils/helpers';

export default function AlertScreen({ navigation }) {
  const { riskAssessment, recommendations } = useContext(BurnoutContext);

  const level = riskAssessment?.level || 'high';
  const color = getRiskLevelColor(level);
  const immediateRecs = (recommendations || []).filter(r => r.type === 'immediate');

  const handleBreak = async () => {
    await ApiClient.recordBreak(15);
    navigation.goBack();
  };

  return (
    <ScrollView style={styles.container} contentContainerStyle={styles.content}>
      <View style={styles.iconWrap}>
        <Icon name="alert-circle" size={64} color={color} />
      </View>

      <Text style={[styles.level, { color }]}>
        {level.replace('_', ' ').toUpperCase()}
      </Text>
      <Text style={styles.score}>
        Скор риска: {formatRiskScore(riskAssessment?.score)}
      </Text>
      <Text style={styles.message}>
        Обнаружен повышенный риск выгорания. Рекомендуем принять меры прямо сейчас.
      </Text>

      {immediateRecs.length > 0 && (
        <View style={styles.section}>
          <Text style={styles.sectionTitle}>Немедленные действия</Text>
          {immediateRecs.map((r, i) => (
            <View key={i} style={styles.recRow}>
              <Icon name="lightning-bolt" size={16} color={color} />
              <Text style={styles.recText}>{r.title}</Text>
            </View>
          ))}
        </View>
      )}

      <TouchableOpacity style={[styles.btn, { backgroundColor: color }]} onPress={handleBreak}>
        <Icon name="timer" size={18} color="#fff" />
        <Text style={styles.btnText}>Взять перерыв 15 мин</Text>
      </TouchableOpacity>

      <TouchableOpacity style={styles.btnOutline} onPress={() => navigation.goBack()}>
        <Text style={styles.btnOutlineText}>Закрыть</Text>
      </TouchableOpacity>
    </ScrollView>
  );
}

const styles = StyleSheet.create({
  container: { flex: 1, backgroundColor: theme.colors.background },
  content: { padding: 24, alignItems: 'center' },
  iconWrap: { marginTop: 16, marginBottom: 12 },
  level: { fontSize: 28, fontWeight: '800', marginBottom: 4 },
  score: { fontSize: 14, color: theme.colors.textSecondary, marginBottom: 12 },
  message: { fontSize: 15, color: theme.colors.text, textAlign: 'center', lineHeight: 22, marginBottom: 24 },
  section: { width: '100%', marginBottom: 24 },
  sectionTitle: { fontSize: 15, fontWeight: '600', color: theme.colors.text, marginBottom: 10 },
  recRow: { flexDirection: 'row', gap: 8, alignItems: 'flex-start', marginBottom: 8 },
  recText: { flex: 1, fontSize: 14, color: theme.colors.text, lineHeight: 20 },
  btn: { flexDirection: 'row', gap: 8, alignItems: 'center', justifyContent: 'center', width: '100%', paddingVertical: 14, borderRadius: 12, marginBottom: 12 },
  btnText: { color: '#fff', fontSize: 16, fontWeight: '600' },
  btnOutline: { width: '100%', paddingVertical: 14, borderRadius: 12, borderWidth: 1, borderColor: theme.colors.border, alignItems: 'center' },
  btnOutlineText: { color: theme.colors.textSecondary, fontSize: 15 },
});
