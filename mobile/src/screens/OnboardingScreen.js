/**
 * OnboardingScreen — первый запуск приложения
 */

import React, { useState } from 'react';
import { View, Text, StyleSheet, TouchableOpacity, Dimensions } from 'react-native';
import LinearGradient from 'react-native-linear-gradient';
import Icon from 'react-native-vector-icons/MaterialCommunityIcons';
import AsyncStorage from '@react-native-async-storage/async-storage';
import { theme } from '../theme/theme';

const { width } = Dimensions.get('window');

const SLIDES = [
  {
    icon: 'shield-check',
    title: 'Защита от выгорания',
    desc: 'BurnoutGuard отслеживает твоё эмоциональное состояние и предупреждает о риске выгорания в реальном времени.',
  },
  {
    icon: 'brain',
    title: 'AI-анализ',
    desc: 'Умный алгоритм анализирует эмоциональные паттерны и создаёт персонализированные рекомендации именно для тебя.',
  },
  {
    icon: 'trending-up',
    title: 'Прогресс',
    desc: 'Следи за своим прогрессом. Наблюдай, как твой риск снижается благодаря правильным действиям.',
  },
];

export default function OnboardingScreen({ navigation }) {
  const [step, setStep] = useState(0);

  const handleFinish = async () => {
    await AsyncStorage.setItem('hasLaunched', 'true');
    navigation.replace('Main');
  };

  const slide = SLIDES[step];
  const isLast = step === SLIDES.length - 1;

  return (
    <LinearGradient
      colors={[theme.colors.primary, theme.colors.secondary]}
      style={styles.container}
    >
      <View style={styles.inner}>
        <Icon name={slide.icon} size={80} color="rgba(255,255,255,0.9)" />
        <Text style={styles.title}>{slide.title}</Text>
        <Text style={styles.desc}>{slide.desc}</Text>

        <View style={styles.dots}>
          {SLIDES.map((_, i) => (
            <View
              key={i}
              style={[styles.dot, i === step && styles.dotActive]}
            />
          ))}
        </View>

        <TouchableOpacity
          style={styles.button}
          onPress={isLast ? handleFinish : () => setStep(s => s + 1)}
        >
          <Text style={styles.buttonText}>{isLast ? 'Начать' : 'Далее'}</Text>
        </TouchableOpacity>

        {!isLast && (
          <TouchableOpacity onPress={handleFinish}>
            <Text style={styles.skip}>Пропустить</Text>
          </TouchableOpacity>
        )}
      </View>
    </LinearGradient>
  );
}

const styles = StyleSheet.create({
  container: { flex: 1 },
  inner: { flex: 1, justifyContent: 'center', alignItems: 'center', padding: 40 },
  title: { fontSize: 26, fontWeight: '800', color: '#fff', marginTop: 24, textAlign: 'center' },
  desc: { fontSize: 16, color: 'rgba(255,255,255,0.85)', textAlign: 'center', lineHeight: 24, marginTop: 12 },
  dots: { flexDirection: 'row', gap: 8, marginTop: 32 },
  dot: { width: 8, height: 8, borderRadius: 4, backgroundColor: 'rgba(255,255,255,0.4)' },
  dotActive: { backgroundColor: '#fff', width: 20 },
  button: {
    marginTop: 32,
    backgroundColor: '#fff',
    paddingHorizontal: 48,
    paddingVertical: 14,
    borderRadius: 30,
    width: width - 80,
    alignItems: 'center',
  },
  buttonText: { color: theme.colors.primary, fontSize: 16, fontWeight: '700' },
  skip: { marginTop: 16, color: 'rgba(255,255,255,0.7)', fontSize: 14 },
});
