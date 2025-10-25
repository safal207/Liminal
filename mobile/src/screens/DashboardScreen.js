/**
 * 🚀🛡️ BurnoutGuard Dashboard Screen
 * 
 * Main dashboard showing current burnout risk status,
 * quick actions, and real-time monitoring
 */

import React, { useState, useEffect, useContext } from 'react';
import {
  View,
  Text,
  ScrollView,
  StyleSheet,
  RefreshControl,
  Alert,
  Dimensions,
} from 'react-native';
import {
  Card,
  Title,
  Paragraph,
  Button,
  Chip,
  ProgressBar,
  FAB,
} from 'react-native-paper';
import LinearGradient from 'react-native-linear-gradient';
import * as Animatable from 'react-native-animatable';
import Icon from 'react-native-vector-icons/MaterialCommunityIcons';

import { BurnoutContext } from '../context/BurnoutContext';
import { theme } from '../theme/theme';
import { formatRiskScore, getTimeOfDay } from '../utils/helpers';

const { width } = Dimensions.get('window');

export default function DashboardScreen({ navigation }) {
  const { 
    currentState, 
    riskAssessment, 
    recommendations,
    isMonitoring,
    refreshData,
    startMonitoring,
    stopMonitoring 
  } = useContext(BurnoutContext);

  const [refreshing, setRefreshing] = useState(false);
  const [timeOfDay, setTimeOfDay] = useState('');

  useEffect(() => {
    setTimeOfDay(getTimeOfDay());
    const interval = setInterval(() => {
      setTimeOfDay(getTimeOfDay());
    }, 60000); // Update every minute

    return () => clearInterval(interval);
  }, []);

  const onRefresh = async () => {
    setRefreshing(true);
    try {
      await refreshData();
    } catch (error) {
      Alert.alert('Error', 'Failed to refresh data');
    } finally {
      setRefreshing(false);
    }
  };

  const handleEmergencyAction = () => {
    Alert.alert(
      '🚨 Emergency Burnout Protocol',
      'You appear to be in a critical state. Would you like to:\n\n' +
      '• Take an immediate 15-minute break\n' +
      '• Contact your manager/HR\n' +
      '• Access crisis resources',
      [
        { text: 'Take Break', onPress: () => navigation.navigate('Recommendations') },
        { text: 'Contact Help', onPress: () => contactSupport() },
        { text: 'Cancel', style: 'cancel' },
      ]
    );
  };

  const contactSupport = () => {
    // In production, this would contact actual support
    Alert.alert('Support', 'Connecting you with support resources...');
  };

  const getRiskColor = (score) => {
    if (score >= 0.8) return theme.colors.error;
    if (score >= 0.6) return theme.colors.warning;
    if (score >= 0.4) return theme.colors.caution;
    return theme.colors.success;
  };

  const getRiskGradient = (score) => {
    if (score >= 0.8) return ['#e53e3e', '#c53030'];
    if (score >= 0.6) return ['#f56500', '#dd6b20'];
    if (score >= 0.4) return ['#ed8936', '#d69e2e'];
    return ['#48bb78', '#38a169'];
  };

  if (!currentState) {
    return (
      <View style={styles.loadingContainer}>
        <Icon name="heart-pulse" size={60} color={theme.colors.primary} />
        <Text style={styles.loadingText}>Initializing BurnoutGuard...</Text>
      </View>
    );
  }

  const riskScore = riskAssessment?.score || 0;
  const riskLevel = riskAssessment?.level || 'low';
  const topRecommendations = recommendations?.slice(0, 3) || [];

  return (
    <ScrollView
      style={styles.container}
      refreshControl={
        <RefreshControl refreshing={refreshing} onRefresh={onRefresh} />
      }
    >
      {/* Header Greeting */}
      <View style={styles.header}>
        <Text style={styles.greeting}>Good {timeOfDay}! 👋</Text>
        <Text style={styles.subtitle}>How are you feeling today?</Text>
      </View>

      {/* Main Risk Card */}
      <Animatable.View animation="fadeInUp" duration={800}>
        <Card style={styles.riskCard}>
          <LinearGradient
            colors={getRiskGradient(riskScore)}
            style={styles.riskGradient}
          >
            <View style={styles.riskContent}>
              <View style={styles.riskHeader}>
                <Icon 
                  name="shield-check" 
                  size={32} 
                  color={theme.colors.white} 
                />
                <Text style={styles.riskTitle}>Burnout Risk</Text>
              </View>
              
              <Text style={styles.riskScore}>
                {Math.round(riskScore * 100)}%
              </Text>
              
              <Text style={styles.riskLevel}>
                {riskLevel.toUpperCase()} RISK
              </Text>
              
              <ProgressBar
                progress={riskScore}
                color={theme.colors.white}
                style={styles.progressBar}
              />
              
              <Text style={styles.lastUpdate}>
                Last updated: {new Date().toLocaleTimeString()}
              </Text>
            </View>
          </LinearGradient>
        </Card>
      </Animatable.View>

      {/* Quick Status Cards */}
      <Animatable.View animation="fadeInUp" duration={800} delay={200}>
        <View style={styles.statusRow}>
          <Card style={styles.statusCard}>
            <View style={styles.statusContent}>
              <Icon name="heart-pulse" size={24} color={theme.colors.primary} />
              <Text style={styles.statusValue}>
                {isMonitoring ? 'Active' : 'Paused'}
              </Text>
              <Text style={styles.statusLabel}>Monitoring</Text>
            </View>
          </Card>
          
          <Card style={styles.statusCard}>
            <View style={styles.statusContent}>
              <Icon name="clock-outline" size={24} color={theme.colors.primary} />
              <Text style={styles.statusValue}>
                {currentState?.workHours || '8'}h
              </Text>
              <Text style={styles.statusLabel}>Work Today</Text>
            </View>
          </Card>
          
          <Card style={styles.statusCard}>
            <View style={styles.statusContent}>
              <Icon name="coffee" size={24} color={theme.colors.primary} />
              <Text style={styles.statusValue}>
                {currentState?.breaksTaken || '3'}
              </Text>
              <Text style={styles.statusLabel}>Breaks</Text>
            </View>
          </Card>
        </View>
      </Animatable.View>

      {/* Current Indicators */}
      {riskAssessment?.emotional_indicators && (
        <Animatable.View animation="fadeInUp" duration={800} delay={400}>
          <Card style={styles.indicatorsCard}>
            <Card.Content>
              <Title>Current Indicators</Title>
              <View style={styles.chipsContainer}>
                {riskAssessment.emotional_indicators.slice(0, 4).map((indicator, index) => (
                  <Chip
                    key={index}
                    mode="outlined"
                    style={styles.indicatorChip}
                    textStyle={styles.chipText}
                  >
                    {indicator}
                  </Chip>
                ))}
              </View>
            </Card.Content>
          </Card>
        </Animatable.View>
      )}

      {/* Quick Recommendations */}
      <Animatable.View animation="fadeInUp" duration={800} delay={600}>
        <Card style={styles.recommendationsCard}>
          <Card.Content>
            <View style={styles.sectionHeader}>
              <Title>Quick Actions</Title>
              <Button
                mode="text"
                onPress={() => navigation.navigate('Recommendations')}
              >
                View All
              </Button>
            </View>
            
            {topRecommendations.map((rec, index) => (
              <View key={index} style={styles.recommendationItem}>
                <Icon 
                  name="lightbulb-outline" 
                  size={20} 
                  color={theme.colors.primary} 
                />
                <Text style={styles.recommendationText}>{rec.title}</Text>
              </View>
            ))}
            
            {topRecommendations.length === 0 && (
              <Text style={styles.noRecommendations}>
                ✅ You're doing great! Keep up the good work.
              </Text>
            )}
          </Card.Content>
        </Card>
      </Animatable.View>

      {/* Emergency Alert */}
      {riskScore >= 0.8 && (
        <Animatable.View animation="pulse" iterationCount="infinite">
          <Card style={styles.emergencyCard}>
            <Card.Content>
              <View style={styles.emergencyContent}>
                <Icon name="alert" size={24} color={theme.colors.error} />
                <Text style={styles.emergencyText}>
                  Critical burnout risk detected
                </Text>
              </View>
              <Button
                mode="contained"
                onPress={handleEmergencyAction}
                buttonColor={theme.colors.error}
                style={styles.emergencyButton}
              >
                Take Action Now
              </Button>
            </Card.Content>
          </Card>
        </Animatable.View>
      )}

      <View style={styles.bottomSpacer} />

      {/* Floating Action Button */}
      <FAB
        style={styles.fab}
        icon={isMonitoring ? "pause" : "play"}
        onPress={isMonitoring ? stopMonitoring : startMonitoring}
        label={isMonitoring ? "Pause" : "Start"}
      />
    </ScrollView>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: theme.colors.background,
  },
  loadingContainer: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
    backgroundColor: theme.colors.background,
  },
  loadingText: {
    marginTop: 20,
    fontSize: 16,
    color: theme.colors.textSecondary,
  },
  header: {
    padding: 20,
    paddingBottom: 10,
  },
  greeting: {
    fontSize: 24,
    fontWeight: 'bold',
    color: theme.colors.text,
  },
  subtitle: {
    fontSize: 16,
    color: theme.colors.textSecondary,
    marginTop: 5,
  },
  riskCard: {
    margin: 20,
    marginTop: 10,
    elevation: 8,
    borderRadius: 15,
    overflow: 'hidden',
  },
  riskGradient: {
    padding: 25,
  },
  riskContent: {
    alignItems: 'center',
  },
  riskHeader: {
    flexDirection: 'row',
    alignItems: 'center',
    marginBottom: 15,
  },
  riskTitle: {
    fontSize: 18,
    fontWeight: 'bold',
    color: theme.colors.white,
    marginLeft: 10,
  },
  riskScore: {
    fontSize: 48,
    fontWeight: 'bold',
    color: theme.colors.white,
  },
  riskLevel: {
    fontSize: 16,
    fontWeight: 'bold',
    color: theme.colors.white,
    marginBottom: 15,
    letterSpacing: 1,
  },
  progressBar: {
    width: '100%',
    height: 8,
    borderRadius: 4,
    marginBottom: 10,
  },
  lastUpdate: {
    fontSize: 12,
    color: theme.colors.white,
    opacity: 0.8,
  },
  statusRow: {
    flexDirection: 'row',
    paddingHorizontal: 20,
    justifyContent: 'space-between',
  },
  statusCard: {
    width: (width - 60) / 3,
    elevation: 3,
    borderRadius: 10,
  },
  statusContent: {
    padding: 15,
    alignItems: 'center',
  },
  statusValue: {
    fontSize: 20,
    fontWeight: 'bold',
    color: theme.colors.text,
    marginTop: 8,
  },
  statusLabel: {
    fontSize: 12,
    color: theme.colors.textSecondary,
    marginTop: 4,
  },
  indicatorsCard: {
    margin: 20,
    elevation: 3,
    borderRadius: 10,
  },
  chipsContainer: {
    flexDirection: 'row',
    flexWrap: 'wrap',
    marginTop: 10,
  },
  indicatorChip: {
    margin: 4,
    backgroundColor: theme.colors.surface,
  },
  chipText: {
    fontSize: 12,
  },
  recommendationsCard: {
    margin: 20,
    marginTop: 0,
    elevation: 3,
    borderRadius: 10,
  },
  sectionHeader: {
    flexDirection: 'row',
    justifyContent: 'space-between',
    alignItems: 'center',
    marginBottom: 10,
  },
  recommendationItem: {
    flexDirection: 'row',
    alignItems: 'center',
    paddingVertical: 8,
  },
  recommendationText: {
    marginLeft: 12,
    fontSize: 14,
    color: theme.colors.text,
    flex: 1,
  },
  noRecommendations: {
    textAlign: 'center',
    color: theme.colors.textSecondary,
    fontStyle: 'italic',
    paddingVertical: 20,
  },
  emergencyCard: {
    margin: 20,
    marginTop: 0,
    elevation: 5,
    borderRadius: 10,
    borderLeftWidth: 4,
    borderLeftColor: theme.colors.error,
  },
  emergencyContent: {
    flexDirection: 'row',
    alignItems: 'center',
    marginBottom: 15,
  },
  emergencyText: {
    marginLeft: 10,
    fontSize: 16,
    fontWeight: 'bold',
    color: theme.colors.error,
  },
  emergencyButton: {
    borderRadius: 25,
  },
  bottomSpacer: {
    height: 100,
  },
  fab: {
    position: 'absolute',
    margin: 16,
    right: 0,
    bottom: 80,
    backgroundColor: theme.colors.primary,
  },
});