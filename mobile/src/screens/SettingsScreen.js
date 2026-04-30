/**
 * SettingsScreen — настройки приложения
 */

import React, { useContext, useState } from 'react';
import { View, Text, StyleSheet, Switch, TouchableOpacity, Alert } from 'react-native';
import { Card } from 'react-native-paper';
import AsyncStorage from '@react-native-async-storage/async-storage';
import { BurnoutContext } from '../context/BurnoutContext';
import { theme } from '../theme/theme';

export default function SettingsScreen() {
  const { isMonitoring, startMonitoring, stopMonitoring } = useContext(BurnoutContext);
  const [notifications, setNotifications] = useState(true);

  const handleMonitoringToggle = async (value) => {
    if (value) {
      await startMonitoring();
    } else {
      stopMonitoring();
    }
  };

  const handleClearData = () => {
    Alert.alert(
      'Очистить данные',
      'Это сбросит все локальные настройки. Продолжить?',
      [
        { text: 'Отмена', style: 'cancel' },
        {
          text: 'Очистить',
          style: 'destructive',
          onPress: async () => {
            await AsyncStorage.clear();
            Alert.alert('Готово', 'Данные очищены. Перезапустите приложение.');
          },
        },
      ]
    );
  };

  return (
    <View style={styles.container}>
      <Text style={styles.title}>Настройки</Text>

      <Card style={styles.card}>
        <Card.Content>
          <Text style={styles.sectionTitle}>Мониторинг</Text>

          <View style={styles.row}>
            <Text style={styles.rowLabel}>Активный мониторинг</Text>
            <Switch
              value={isMonitoring}
              onValueChange={handleMonitoringToggle}
              trackColor={{ true: theme.colors.primary }}
            />
          </View>

          <View style={styles.row}>
            <Text style={styles.rowLabel}>Уведомления о риске</Text>
            <Switch
              value={notifications}
              onValueChange={setNotifications}
              trackColor={{ true: theme.colors.primary }}
            />
          </View>
        </Card.Content>
      </Card>

      <Card style={styles.card}>
        <Card.Content>
          <Text style={styles.sectionTitle}>Данные</Text>
          <TouchableOpacity style={styles.dangerBtn} onPress={handleClearData}>
            <Text style={styles.dangerText}>Очистить локальные данные</Text>
          </TouchableOpacity>
        </Card.Content>
      </Card>

      <Text style={styles.version}>BurnoutGuard v1.0.0 • Resonance Liminal</Text>
    </View>
  );
}

const styles = StyleSheet.create({
  container: { flex: 1, backgroundColor: theme.colors.background, padding: 16 },
  title: { fontSize: 20, fontWeight: '700', color: theme.colors.text, marginBottom: 16 },
  card: { marginBottom: 12, borderRadius: 12 },
  sectionTitle: { fontSize: 14, fontWeight: '600', color: theme.colors.textSecondary, marginBottom: 12, textTransform: 'uppercase', letterSpacing: 0.5 },
  row: { flexDirection: 'row', justifyContent: 'space-between', alignItems: 'center', paddingVertical: 8 },
  rowLabel: { fontSize: 15, color: theme.colors.text },
  dangerBtn: { paddingVertical: 10 },
  dangerText: { color: theme.colors.error, fontSize: 15 },
  version: { textAlign: 'center', color: theme.colors.textLight, fontSize: 12, marginTop: 'auto', paddingBottom: 16 },
});
