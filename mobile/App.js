/**
 * 🚀🛡️ BurnoutGuard Mobile App
 * 
 * AI-powered burnout prevention for individuals
 * Real-time monitoring, personalized recommendations, and wellness tracking
 * 
 * "Your personal shield against burnout" ✨
 */

import React, { useEffect, useState } from 'react';
import { NavigationContainer } from '@react-navigation/native';
import { createBottomTabNavigator } from '@react-navigation/bottom-tabs';
import { createStackNavigator } from '@react-navigation/stack';
import { StatusBar, Alert, AppState } from 'react-native';
import { Provider as PaperProvider } from 'react-native-paper';
import AsyncStorage from '@react-native-async-storage/async-storage';
import Icon from 'react-native-vector-icons/MaterialCommunityIcons';

// Screens
import DashboardScreen from './src/screens/DashboardScreen';
import RiskAnalysisScreen from './src/screens/RiskAnalysisScreen';
import RecommendationsScreen from './src/screens/RecommendationsScreen';
import ProgressScreen from './src/screens/ProgressScreen';
import SettingsScreen from './src/screens/SettingsScreen';
import OnboardingScreen from './src/screens/OnboardingScreen';
import AlertScreen from './src/screens/AlertScreen';

// Services
import { BurnoutService } from './src/services/BurnoutService';
import { NotificationService } from './src/services/NotificationService';
import { WebSocketService } from './src/services/WebSocketService';

// Context
import { BurnoutProvider } from './src/context/BurnoutContext';

// Theme
import { theme } from './src/theme/theme';

const Tab = createBottomTabNavigator();
const Stack = createStackNavigator();

// Main Tab Navigator
function MainTabs() {
  return (
    <Tab.Navigator
      initialRouteName="Dashboard"
      screenOptions={({ route }) => ({
        tabBarIcon: ({ focused, color, size }) => {
          let iconName;

          switch (route.name) {
            case 'Dashboard':
              iconName = focused ? 'view-dashboard' : 'view-dashboard-outline';
              break;
            case 'Analysis':
              iconName = focused ? 'chart-line' : 'chart-line-variant';
              break;
            case 'Recommendations':
              iconName = focused ? 'lightbulb' : 'lightbulb-outline';
              break;
            case 'Progress':
              iconName = focused ? 'trending-up' : 'chart-timeline-variant';
              break;
            case 'Settings':
              iconName = focused ? 'cog' : 'cog-outline';
              break;
            default:
              iconName = 'circle';
          }

          return <Icon name={iconName} size={size} color={color} />;
        },
        tabBarActiveTintColor: theme.colors.primary,
        tabBarInactiveTintColor: theme.colors.textSecondary,
        tabBarStyle: {
          backgroundColor: theme.colors.surface,
          borderTopWidth: 1,
          borderTopColor: theme.colors.border,
          paddingBottom: 5,
          height: 60,
        },
        headerStyle: {
          backgroundColor: theme.colors.primary,
        },
        headerTintColor: theme.colors.white,
        headerTitleStyle: {
          fontWeight: 'bold',
        },
      })}
    >
      <Tab.Screen 
        name="Dashboard" 
        component={DashboardScreen}
        options={{ title: '🛡️ BurnoutGuard' }}
      />
      <Tab.Screen 
        name="Analysis" 
        component={RiskAnalysisScreen}
        options={{ title: 'Risk Analysis' }}
      />
      <Tab.Screen 
        name="Recommendations" 
        component={RecommendationsScreen}
        options={{ title: 'Recommendations' }}
      />
      <Tab.Screen 
        name="Progress" 
        component={ProgressScreen}
        options={{ title: 'Progress' }}
      />
      <Tab.Screen 
        name="Settings" 
        component={SettingsScreen}
        options={{ title: 'Settings' }}
      />
    </Tab.Navigator>
  );
}

// Main App Component
export default function App() {
  const [isFirstLaunch, setIsFirstLaunch] = useState(null);
  const [isLoading, setIsLoading] = useState(true);

  useEffect(() => {
    checkFirstLaunch();
    initializeServices();
    setupAppStateListener();
  }, []);

  const checkFirstLaunch = async () => {
    try {
      const value = await AsyncStorage.getItem('hasLaunched');
      if (value === null) {
        setIsFirstLaunch(true);
        await AsyncStorage.setItem('hasLaunched', 'true');
      } else {
        setIsFirstLaunch(false);
      }
    } catch (error) {
      console.error('Error checking first launch:', error);
      setIsFirstLaunch(false);
    } finally {
      setIsLoading(false);
    }
  };

  const initializeServices = async () => {
    try {
      // Initialize notification service
      await NotificationService.initialize();
      
      // Initialize burnout monitoring service
      await BurnoutService.initialize();
      
      // Initialize WebSocket connection
      await WebSocketService.connect();
      
      console.log('All services initialized successfully');
    } catch (error) {
      console.error('Error initializing services:', error);
      Alert.alert(
        'Initialization Error',
        'Some features may not work properly. Please restart the app.',
        [{ text: 'OK' }]
      );
    }
  };

  const setupAppStateListener = () => {
    const handleAppStateChange = (nextAppState) => {
      if (nextAppState === 'active') {
        // App came to foreground - resume monitoring
        BurnoutService.resumeMonitoring();
        WebSocketService.reconnect();
      } else if (nextAppState === 'background') {
        // App went to background - pause intensive monitoring
        BurnoutService.pauseMonitoring();
      }
    };

    const subscription = AppState.addEventListener('change', handleAppStateChange);
    return () => subscription?.remove();
  };

  if (isLoading) {
    // In production, this would be a proper loading screen
    return null;
  }

  return (
    <PaperProvider theme={theme}>
      <BurnoutProvider>
        <NavigationContainer>
          <StatusBar 
            barStyle="light-content" 
            backgroundColor={theme.colors.primary}
          />
          <Stack.Navigator screenOptions={{ headerShown: false }}>
            {isFirstLaunch ? (
              <Stack.Screen 
                name="Onboarding" 
                component={OnboardingScreen}
              />
            ) : null}
            <Stack.Screen 
              name="Main" 
              component={MainTabs}
            />
            <Stack.Screen 
              name="Alert" 
              component={AlertScreen}
              options={{
                presentation: 'modal',
                headerShown: true,
                headerTitle: 'Burnout Alert',
                headerStyle: {
                  backgroundColor: theme.colors.error,
                },
                headerTintColor: theme.colors.white,
              }}
            />
          </Stack.Navigator>
        </NavigationContainer>
      </BurnoutProvider>
    </PaperProvider>
  );
}