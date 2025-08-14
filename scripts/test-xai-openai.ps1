# Test script for XAI + OpenAI integration
# This script tests the new natural language explanation feature
# It makes predictions and requests explanations with various methods

# Make sure to set OPENAI_API_KEY in .env file before running

Write-Host "Testing XAI + OpenAI Integration" -ForegroundColor Green

# Base URL for API
$baseUrl = "http://localhost:5000"

# Test 1: Make a prediction
Write-Host "Step 1: Making a prediction for anomaly detection..." -ForegroundColor Cyan
$features = @{
    "messages_per_minute" = 120
    "connection_duration" = 3600
    "error_rate" = 0.05
    "unique_ips" = 15
    "failed_auth_attempts" = 3
}

$predictionData = @{
    "features" = $features
}

try {
    $predictionResponse = Invoke-RestMethod -Uri "$baseUrl/ml/predict/anomaly_detection" -Method Post -Body ($predictionData | ConvertTo-Json) -ContentType "application/json"
    $predictionId = $predictionResponse.prediction_id
    Write-Host "Success! Prediction ID: $predictionId" -ForegroundColor Green
    Write-Host "Prediction result: $($predictionResponse | ConvertTo-Json -Depth 3)" -ForegroundColor Green
}
catch {
    Write-Host "Error making prediction: $_" -ForegroundColor Red
    exit 1
}

# Test 2: Get traditional XAI explanation (SHAP)
Write-Host "`nStep 2: Getting SHAP explanation..." -ForegroundColor Cyan
try {
    $shapUrl = "$baseUrl/ml/explain/$predictionId`?method=shap"
    $shapResponse = Invoke-RestMethod -Uri $shapUrl -Method Get
    Write-Host "Success! Got SHAP explanation" -ForegroundColor Green
    Write-Host "SHAP visualization available: $($shapResponse.visualization.Length -gt 0)" -ForegroundColor Green
}
catch {
    Write-Host "Error getting SHAP explanation: $_" -ForegroundColor Red
}

# Test 3: Get natural language explanation
Write-Host "`nStep 3: Getting natural language explanation..." -ForegroundColor Cyan
try {
    $nlUrl = "$baseUrl/ml/explain/$predictionId`?method=natural_language"
    $nlResponse = Invoke-RestMethod -Uri $nlUrl -Method Get
    Write-Host "Success! Got natural language explanation" -ForegroundColor Green
    Write-Host "Summary: $($nlResponse.summary)" -ForegroundColor Yellow
    Write-Host "Full explanation: $($nlResponse.explanation)" -ForegroundColor Yellow
    Write-Host "Confidence: $($nlResponse.confidence)" -ForegroundColor Yellow
    if ($nlResponse.recommendations) {
        Write-Host "Recommendations: $($nlResponse.recommendations)" -ForegroundColor Yellow
    }
}
catch {
    Write-Host "Error getting natural language explanation: $_" -ForegroundColor Red
}

# Test 4: Get combined explanation (SHAP + natural language)
Write-Host "`nStep 4: Getting combined explanation (SHAP + natural language)..." -ForegroundColor Cyan
try {
    $combinedUrl = "$baseUrl/ml/explain/$predictionId`?method=shap&include_natural_language=true"
    $combinedResponse = Invoke-RestMethod -Uri $combinedUrl -Method Get
    Write-Host "Success! Got combined explanation" -ForegroundColor Green
    Write-Host "SHAP visualization available: $($combinedResponse.visualization.Length -gt 0)" -ForegroundColor Green
    if ($combinedResponse.natural_language_explanation) {
        Write-Host "Natural language summary: $($combinedResponse.natural_language_explanation.summary)" -ForegroundColor Yellow
    }
}
catch {
    Write-Host "Error getting combined explanation: $_" -ForegroundColor Red
}

# Test 5: Get available models with explanation methods
Write-Host "`nStep 5: Getting available models with explanation methods..." -ForegroundColor Cyan
try {
    $modelsUrl = "$baseUrl/ml_models"
    $modelsResponse = Invoke-RestMethod -Uri $modelsUrl -Method Get
    Write-Host "Success! Got available models" -ForegroundColor Green
    Write-Host "Models with explanation methods:" -ForegroundColor Yellow
    foreach ($model in $modelsResponse) {
        Write-Host "- $($model.name): $($model.explanation_methods -join ', ')" -ForegroundColor Yellow
    }
}
catch {
    Write-Host "Error getting models: $_" -ForegroundColor Red
}

Write-Host "`nTest completed!" -ForegroundColor Green
