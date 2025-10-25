/**
 * 🚀🛡️ BurnoutGuard Theme
 * 
 * Unified theme configuration for the mobile app
 * Colors, typography, and component styling
 */

import { DefaultTheme } from 'react-native-paper';

const colors = {
  // Primary Colors
  primary: '#667eea',       // Purple-blue gradient start
  primaryDark: '#5a67d8',   // Darker primary
  secondary: '#764ba2',     // Purple-blue gradient end
  
  // Status Colors
  success: '#48bb78',       // Green for good status
  warning: '#ed8936',       // Orange for warnings
  error: '#e53e3e',         // Red for errors/critical
  caution: '#ecc94b',       // Yellow for caution
  
  // Text Colors
  text: '#2d3748',          // Primary text
  textSecondary: '#718096', // Secondary text
  textLight: '#a0aec0',     // Light text
  white: '#ffffff',
  
  // Background Colors
  background: '#f7fafc',    // App background
  surface: '#ffffff',       // Card/surface background
  surfaceAlt: '#edf2f7',    // Alternative surface
  border: '#e2e8f0',        // Borders
  
  // Gradient Colors
  gradientStart: '#667eea',
  gradientEnd: '#764ba2',
  
  // Risk Level Colors
  riskCritical: '#e53e3e',
  riskHigh: '#f56500',
  riskMedium: '#ed8936',
  riskLow: '#ecc94b',
  riskVeryLow: '#48bb78',
  
  // Transparency overlays
  overlay: 'rgba(0, 0, 0, 0.5)',
  overlayLight: 'rgba(0, 0, 0, 0.3)',
};

const typography = {
  // Font sizes
  h1: 32,
  h2: 28,
  h3: 24,
  h4: 20,
  h5: 18,
  h6: 16,
  body: 16,
  bodySmall: 14,
  caption: 12,
  
  // Font weights
  light: '300',
  regular: '400',
  medium: '500',
  semiBold: '600',
  bold: '700',
  
  // Line heights
  lineHeightTight: 1.2,
  lineHeightNormal: 1.4,
  lineHeightRelaxed: 1.6,
};

const spacing = {
  xs: 4,
  sm: 8,
  md: 16,
  lg: 24,
  xl: 32,
  xxl: 48,
};

const borderRadius = {
  sm: 4,
  md: 8,
  lg: 12,
  xl: 16,
  xxl: 24,
  full: 999,
};

const shadows = {
  small: {
    shadowOffset: { width: 0, height: 1 },
    shadowOpacity: 0.18,
    shadowRadius: 1.0,
    elevation: 1,
  },
  medium: {
    shadowOffset: { width: 0, height: 2 },
    shadowOpacity: 0.23,
    shadowRadius: 2.62,
    elevation: 4,
  },
  large: {
    shadowOffset: { width: 0, height: 4 },
    shadowOpacity: 0.30,
    shadowRadius: 4.65,
    elevation: 8,
  },
};

// React Native Paper theme configuration
export const theme = {
  ...DefaultTheme,
  colors: {
    ...DefaultTheme.colors,
    ...colors,
  },
  typography,
  spacing,
  borderRadius,
  shadows,
  
  // Custom theme extensions
  custom: {
    riskColors: {
      critical: colors.riskCritical,
      high: colors.riskHigh,
      medium: colors.riskMedium,
      low: colors.riskLow,
      veryLow: colors.riskVeryLow,
    },
    gradients: {
      primary: [colors.gradientStart, colors.gradientEnd],
      critical: [colors.riskCritical, '#c53030'],
      high: [colors.riskHigh, '#dd6b20'],
      medium: [colors.riskMedium, '#d69e2e'],
      low: [colors.riskLow, '#d69e2e'],
      veryLow: [colors.riskVeryLow, '#38a169'],
    },
    animations: {
      fast: 200,
      normal: 300,
      slow: 500,
    },
  },
};

// Helper functions for theme usage
export const getRiskColor = (riskScore) => {
  if (riskScore >= 0.8) return colors.riskCritical;
  if (riskScore >= 0.6) return colors.riskHigh;
  if (riskScore >= 0.4) return colors.riskMedium;
  if (riskScore >= 0.2) return colors.riskLow;
  return colors.riskVeryLow;
};

export const getRiskGradient = (riskScore) => {
  if (riskScore >= 0.8) return ['#e53e3e', '#c53030'];
  if (riskScore >= 0.6) return ['#f56500', '#dd6b20'];
  if (riskScore >= 0.4) return ['#ed8936', '#d69e2e'];
  if (riskScore >= 0.2) return ['#ecc94b', '#d69e2e'];
  return ['#48bb78', '#38a169'];
};

export const getRiskLevelText = (riskScore) => {
  if (riskScore >= 0.8) return 'CRITICAL';
  if (riskScore >= 0.6) return 'HIGH';
  if (riskScore >= 0.4) return 'MEDIUM';
  if (riskScore >= 0.2) return 'LOW';
  return 'VERY LOW';
};

// Component-specific styles
export const componentStyles = {
  card: {
    borderRadius: borderRadius.lg,
    backgroundColor: colors.surface,
    ...shadows.medium,
  },
  
  button: {
    borderRadius: borderRadius.xl,
    paddingVertical: spacing.md,
    paddingHorizontal: spacing.lg,
  },
  
  input: {
    borderRadius: borderRadius.md,
    borderWidth: 1,
    borderColor: colors.border,
    paddingHorizontal: spacing.md,
    paddingVertical: spacing.sm,
  },
  
  chip: {
    borderRadius: borderRadius.full,
    paddingHorizontal: spacing.md,
    paddingVertical: spacing.xs,
  },
  
  fab: {
    borderRadius: borderRadius.full,
    ...shadows.large,
  },
};