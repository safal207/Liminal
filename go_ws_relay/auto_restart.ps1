# Скрипт для автоматического перезапуска Go WebSocket relay сервера
# Воплощает принцип "эволюция через резонанс" - постоянное обновление системы

# Устанавливаем кодировку UTF-8
$OutputEncoding = [System.Text.Encoding]::UTF8
[Console]::OutputEncoding = [System.Text.Encoding]::UTF8

Write-Host "[WAVE] Запуск Resonance Liminal Go WebSocket relay с автоперезапуском"
Write-Host "[BRAIN] Путь Чистоты: эволюция через резонанс активирована"

$restartInterval = 30 # секунды между перезапусками

while ($true) {
    # Остановка существующего процесса на порту 8080 (если есть)
    try {
        $process = Get-Process -Id (Get-NetTCPConnection -LocalPort 8080 -ErrorAction SilentlyContinue).OwningProcess -ErrorAction SilentlyContinue
        if ($process) {
            Write-Host "[RELOAD] Останавливаем предыдущий процесс (PID: $($process.Id))"
            Stop-Process -Id $process.Id -Force
            Start-Sleep -Seconds 1
        }
    } catch {
        # Порт свободен или ошибка - продолжаем
    }
    
    # Запуск сервера
    Write-Host "[ROCKET] Запуск Go WebSocket relay сервера ($(Get-Date -Format 'HH:mm:ss'))"
    
    # Запускаем процесс в фоновом режиме
    $process = Start-Process -FilePath "go" -ArgumentList "run main.go neo4j_api.go" -WorkingDirectory "." -PassThru -NoNewWindow
    
    Write-Host "[CHECK] Сервер запущен с PID: $($process.Id)"
    Write-Host "[ANTENNA] WebSocket: ws://localhost:8080/ws"
    Write-Host "[PLUG] REST API: http://localhost:8080/api/consciousness"
    Write-Host "[GLOBE] GraphQL: http://localhost:8080/graphql"
    Write-Host "[CLOCK] Следующий перезапуск через $restartInterval секунд..."
    
    # Ждем указанное время
    Start-Sleep -Seconds $restartInterval
    
    # Останавливаем процесс перед новым запуском
    Write-Host "[RELOAD] Перезапуск сервера..."
    Stop-Process -Id $process.Id -Force
}
