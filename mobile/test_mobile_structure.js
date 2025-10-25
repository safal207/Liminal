/**
 * 🚀🛡️ BurnoutGuard Mobile App Test
 * 
 * Basic validation test for mobile app components and structure
 */

const fs = require('fs');
const path = require('path');

// Test configuration
const mobileDir = path.join(__dirname, '..');
const requiredFiles = [
  'package.json',
  'App.js',
  'src/screens/DashboardScreen.js',
  'src/services/BurnoutService.js',
  'src/theme/theme.js'
];

const requiredDirectories = [
  'src',
  'src/screens',
  'src/services',
  'src/theme',
  'src/context',
  'src/utils'
];

console.log('🚀🛡️ BurnoutGuard Mobile App - Structure Validation Test');
console.log('=' * 60);

// Test 1: Check file structure
console.log('\n📁 Testing file structure...');
let structureValid = true;

requiredFiles.forEach(file => {
  const filePath = path.join(mobileDir, file);
  if (fs.existsSync(filePath)) {
    console.log(`✅ ${file} - Found`);
  } else {
    console.log(`❌ ${file} - Missing`);
    structureValid = false;
  }
});

// Test 2: Check directories
console.log('\n📂 Testing directory structure...');
requiredDirectories.forEach(dir => {
  const dirPath = path.join(mobileDir, dir);
  if (fs.existsSync(dirPath)) {
    console.log(`✅ ${dir}/ - Found`);
  } else {
    console.log(`❌ ${dir}/ - Missing`);
    structureValid = false;
  }
});

// Test 3: Validate package.json
console.log('\n📦 Testing package.json...');
try {
  const packageJsonPath = path.join(mobileDir, 'package.json');
  if (fs.existsSync(packageJsonPath)) {
    const packageJson = JSON.parse(fs.readFileSync(packageJsonPath, 'utf8'));
    
    // Check required dependencies
    const requiredDeps = [
      'react',
      'react-native',
      '@react-navigation/native',
      'react-native-paper',
      'react-native-vector-icons'
    ];
    
    let depsValid = true;
    requiredDeps.forEach(dep => {
      if (packageJson.dependencies && packageJson.dependencies[dep]) {
        console.log(`✅ Dependency: ${dep}`);
      } else {
        console.log(`❌ Missing dependency: ${dep}`);
        depsValid = false;
      }
    });
    
    if (depsValid) {
      console.log('✅ All required dependencies found');
    }
  }
} catch (error) {
  console.log(`❌ Package.json validation failed: ${error.message}`);
  structureValid = false;
}

// Test 4: Basic component validation
console.log('\n🧪 Testing component structure...');
try {
  const appJsPath = path.join(mobileDir, 'App.js');
  if (fs.existsSync(appJsPath)) {
    const appContent = fs.readFileSync(appJsPath, 'utf8');
    
    // Check for essential imports and components
    const requiredElements = [
      'NavigationContainer',
      'createBottomTabNavigator',
      'BurnoutProvider',
      'DashboardScreen'
    ];
    
    requiredElements.forEach(element => {
      if (appContent.includes(element)) {
        console.log(`✅ Component: ${element}`);
      } else {
        console.log(`⚠️  Component may be missing: ${element}`);
      }
    });
  }
} catch (error) {
  console.log(`❌ Component validation failed: ${error.message}`);
}

// Test 5: Theme validation
console.log('\n🎨 Testing theme configuration...');
try {
  const themePath = path.join(mobileDir, 'src/theme/theme.js');
  if (fs.existsSync(themePath)) {
    const themeContent = fs.readFileSync(themePath, 'utf8');
    
    const themeElements = [
      'primary',
      'success',
      'warning', 
      'error',
      'getRiskColor',
      'componentStyles'
    ];
    
    themeElements.forEach(element => {
      if (themeContent.includes(element)) {
        console.log(`✅ Theme element: ${element}`);
      } else {
        console.log(`⚠️  Theme element may be missing: ${element}`);
      }
    });
  }
} catch (error) {
  console.log(`❌ Theme validation failed: ${error.message}`);
}

// Test 6: Service validation
console.log('\n🔧 Testing service structure...');
try {
  const servicePath = path.join(mobileDir, 'src/services/BurnoutService.js');
  if (fs.existsSync(servicePath)) {
    const serviceContent = fs.readFileSync(servicePath, 'utf8');
    
    const serviceMethods = [
      'initialize',
      'startMonitoring',
      'stopMonitoring',
      'performAnalysis',
      'getCurrentState',
      'getRiskAssessment'
    ];
    
    serviceMethods.forEach(method => {
      if (serviceContent.includes(method)) {
        console.log(`✅ Service method: ${method}`);
      } else {
        console.log(`⚠️  Service method may be missing: ${method}`);
      }
    });
  }
} catch (error) {
  console.log(`❌ Service validation failed: ${error.message}`);
}

// Summary
console.log('\n' + '=' * 60);
if (structureValid) {
  console.log('✅ Mobile App Structure Validation: PASSED');
  console.log('\n💡 Next steps:');
  console.log('   1. Run `npm install` to install dependencies');
  console.log('   2. Configure React Native environment');
  console.log('   3. Connect to BurnoutGuard backend');
  console.log('   4. Test on Android/iOS devices');
  console.log('\n🎯 Mobile app structure is ready for development!');
} else {
  console.log('❌ Mobile App Structure Validation: FAILED');
  console.log('\n⚠️  Some files or directories are missing.');
  console.log('   Please ensure all required components are created.');
}

console.log('\n🚀 BurnoutGuard Mobile App validation completed!');