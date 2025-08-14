/**
 * Intelligent Log Analysis Dashboard
 * Main JavaScript utilities for dashboard interaction
 * Resonance Liminal ML Backend - 2025
 */

// Инициализация при загрузке страницы
document.addEventListener('DOMContentLoaded', function() {
    if (document.getElementById('cosmicRadarChart')) initCosmicRadar();
    // Инициализация графика источников энергии
    if (document.getElementById('thyroidBar')) initThyroidBar();
    if (document.getElementById('originFlowChart')) {
        initOriginChart();
    }
    // Инициализация графиков если мы на главной странице
    if (document.getElementById('logDistributionChart')) {
        initCharts();
    }

    // Инициализация временной оси если мы на странице timeline
    if (document.getElementById('timelineContainer')) {
        initTimeline();
    }

    // Обработчик для всех карточек статистики
    document.querySelectorAll('.stat-card').forEach(card => {
        card.addEventListener('click', function() {
            const logType = this.getAttribute('data-log-type');
            if (logType) {
                window.location.href = `/logs/${logType}`;
            }
        });
    });

    // Обработчик для переключения периода на графиках
    document.querySelectorAll('.period-selector').forEach(selector => {
        selector.addEventListener('click', function(e) {
            e.preventDefault();
            
            const period = this.getAttribute('data-period');
            const chartId = this.closest('.card').querySelector('canvas').id;
            
            // Выделяем активный период
            this.closest('.btn-group').querySelectorAll('.period-selector').forEach(btn => {
                btn.classList.remove('active');
            });
            this.classList.add('active');
            
            // Обновляем данные графика
            updateChartPeriod(chartId, period);
        });
    });
});

/**
 * Инициализация основных графиков на главной странице
 */
function initThyroidBar() {
    const bar = document.getElementById('thyroidBar');
    const label = document.getElementById('thyroidLabel');
    fetch('/api/thyroid_status').then(r=>r.json()).then(st=>{
        const percent = Math.min(100, (st.charge / st.threshold) * 100);
        bar.style.width = percent + '%';
        bar.className = 'progress-bar' + (st.ready ? ' bg-danger pulse' : ' bg-info');
        label.textContent = percent.toFixed(0) + '%';
    });
}

function initCosmicRadar() {
    const canvas = document.getElementById('cosmicRadarChart');
    if (!canvas) return;
    fetch('/api/cosmic_scores').then(r=>r.json()).then(scores=>{
        const labels = Object.keys(scores).map(k=>k.replace('law_of_','').replace(/_/g,' '));
        const data = Object.values(scores);
        new Chart(canvas, {
            type: 'radar',
            data: {
                labels,
                datasets: [{
                    label: 'Соблюдение законов (%)',
                    data,
                    backgroundColor: 'rgba(40, 167, 69, 0.2)',
                    borderColor: '#28a745'
                }]
            },
            options: {
                responsive:true,
                scales: {
                    r: { suggestedMin: 0, suggestedMax: 100 }
                }
            }
        });
    });
}

function initOriginChart() {
    const canvas = document.getElementById('originFlowChart');
    if (!canvas) return;
    fetch('/api/origin_stats')
        .then(r => r.json())
        .then(data => {
            new Chart(canvas, {
                type: 'doughnut',
                data: {
                    labels: Object.keys(data),
                    datasets: [{
                        data: Object.values(data),
                        backgroundColor: ['#6f42c1', '#20c997', '#fd7e14', '#6c757d']
                    }]
                },
                options: {
                    responsive: true,
                    plugins: {
                        legend: { position: 'right' },
                        title: {
                            display: true,
                            text: 'Потоки энергии системы'
                        }
                    }
                }
            });
        });
}

function initCharts() {
    // Получение данных из атрибутов data на странице
    const distributionElement = document.getElementById('logDistributionChart');
    const activityElement = document.getElementById('errorActivityChart');
    
    if (!distributionElement || !activityElement) return;
    
    const distributionData = JSON.parse(distributionElement.getAttribute('data-distribution'));
    const activityData = JSON.parse(activityElement.getAttribute('data-activity'));
    
    // График распределения типов логов (круговая диаграмма)
    new Chart(distributionElement, {
        type: 'pie',
        data: {
            labels: Object.keys(distributionData),
            datasets: [{
                data: Object.values(distributionData),
                backgroundColor: [
                    '#dc3545', // errors - red
                    '#28a745', // insights - green
                    '#ffc107', // karma - yellow
                    '#17a2b8'  // hypotheses - blue
                ],
                borderWidth: 1
            }]
        },
        options: {
            responsive: true,
            maintainAspectRatio: false,
            plugins: {
                legend: {
                    position: 'right'
                },
                tooltip: {
                    callbacks: {
                        label: function(context) {
                            const label = context.label || '';
                            const value = context.parsed || 0;
                            const total = context.dataset.data.reduce((a, b) => a + b, 0);
                            const percentage = Math.round(value / total * 100);
                            return `${label}: ${value} (${percentage}%)`;
                        }
                    }
                }
            }
        }
    });
    
    // График активности ошибок (линейный график)
    const activityLabels = Object.keys(activityData);
    const activityValues = Object.values(activityData);
    
    new Chart(activityElement, {
        type: 'line',
        data: {
            labels: activityLabels,
            datasets: [{
                label: 'Количество ошибок',
                data: activityValues,
                borderColor: '#dc3545',
                backgroundColor: 'rgba(220, 53, 69, 0.1)',
                borderWidth: 2,
                fill: true,
                tension: 0.2,
                pointRadius: 3,
                pointBackgroundColor: '#dc3545'
            }]
        },
        options: {
            responsive: true,
            maintainAspectRatio: false,
            scales: {
                y: {
                    beginAtZero: true,
                    grid: {
                        color: 'rgba(0, 0, 0, 0.05)'
                    }
                },
                x: {
                    grid: {
                        display: false
                    }
                }
            },
            plugins: {
                legend: {
                    display: false
                },
                tooltip: {
                    mode: 'index',
                    intersect: false
                }
            }
        }
    });
}

/**
 * Обновление периода для графиков
 */
function updateChartPeriod(chartId, period) {
    // Отправляем запрос на сервер для получения данных за указанный период
    fetch(`/api/chart_data/${chartId}?period=${period}`)
        .then(response => response.json())
        .then(data => {
            // Найдем объект графика и обновим данные
            const chartElement = document.getElementById(chartId);
            const chart = Chart.getChart(chartId);
            
            if (chart) {
                if (chart.config.type === 'line') {
                    // Для линейного графика
                    chart.data.labels = Object.keys(data);
                    chart.data.datasets[0].data = Object.values(data);
                } else if (chart.config.type === 'pie') {
                    // Для кругового графика
                    chart.data.labels = Object.keys(data);
                    chart.data.datasets[0].data = Object.values(data);
                }
                
                chart.update();
            }
        })
        .catch(error => {
            console.error('Error fetching chart data:', error);
        });
}

/**
 * Инициализация временной оси для страницы timeline
 */
function initTimeline() {
    const container = document.getElementById('timelineContainer');
    if (!container) return;
    
    // Получение данных для временной оси
    const timelineData = JSON.parse(container.getAttribute('data-timeline'));
    
    // Подготовка данных в формате vis.js Timeline
    const items = new vis.DataSet(formatTimelineData(timelineData));
    
    // Определение групп для временной оси
    const groups = new vis.DataSet([
        { id: 'experience', content: 'Ошибки', className: 'timeline-group-errors' },
        { id: 'insights', content: 'Решения', className: 'timeline-group-insights' },
        { id: 'karma', content: 'Повторы', className: 'timeline-group-karma' },
        { id: 'hypotheses', content: 'Гипотезы', className: 'timeline-group-hypotheses' }
    ]);
    
    // Настройка опций временной оси
    const options = {
        height: '500px',
        minHeight: '350px',
        maxHeight: '800px',
        stack: true,
        horizontalScroll: true,
        zoomKey: 'ctrlKey',
        format: {
            minorLabels: {
                minute: 'HH:mm',
                hour: 'HH:mm',
                day: 'D MMM',
                week: 'D MMM'
            },
            majorLabels: {
                minute: 'DD MMMM YYYY',
                hour: 'DD MMMM YYYY',
                day: 'MMMM YYYY',
                week: 'MMMM YYYY'
            }
        },
        groupOrder: function(a, b) {
            const order = { 'experience': 1, 'karma': 2, 'insights': 3, 'hypotheses': 4 };
            return order[a.id] - order[b.id];
        },
        orientation: 'top'
    };
    
    // Создание временной оси
    const timeline = new vis.Timeline(container, items, groups, options);
    
    // Обработчики событий для временной оси
    timeline.on('select', function(properties) {
        if (properties.items.length) {
            showEventDetails(properties.items[0], timelineData);
        }
    });
    
    // Кнопки управления временной осью
    document.getElementById('zoomIn').addEventListener('click', function() {
        timeline.zoomIn(0.5);
    });
    
    document.getElementById('zoomOut').addEventListener('click', function() {
        timeline.zoomOut(0.5);
    });
    
    document.getElementById('moveLeft').addEventListener('click', function() {
        const window = timeline.getWindow();
        const interval = window.end - window.start;
        timeline.setWindow(window.start - interval * 0.3, window.end - interval * 0.3);
    });
    
    document.getElementById('moveRight').addEventListener('click', function() {
        const window = timeline.getWindow();
        const interval = window.end - window.start;
        timeline.setWindow(window.start + interval * 0.3, window.end + interval * 0.3);
    });
    
    document.getElementById('fit').addEventListener('click', function() {
        timeline.fit();
    });
    
    // Фильтрация по типу логов
    document.querySelectorAll('.log-type-filter').forEach(checkbox => {
        checkbox.addEventListener('change', function() {
            const logType = this.getAttribute('data-type');
            const isVisible = this.checked;
            
            const group = groups.get(logType);
            group.visible = isVisible;
            groups.update(group);
        });
    });
    
    // Фильтрация по временному периоду
    document.getElementById('timeRangeSelector').addEventListener('change', function() {
        const value = this.value;
        let start, end;
        
        const now = new Date();
        end = now;
        
        switch(value) {
            case 'hour':
                start = new Date(now - 60 * 60 * 1000);
                break;
            case 'day':
                start = new Date(now - 24 * 60 * 60 * 1000);
                break;
            case 'week':
                start = new Date(now - 7 * 24 * 60 * 60 * 1000);
                break;
            case 'month':
                start = new Date(now - 30 * 24 * 60 * 60 * 1000);
                break;
            case 'all':
            default:
                timeline.fit();
                return;
        }
        
        timeline.setWindow(start, end);
    });
}

/**
 * Форматирование данных для временной оси
 */
function formatTimelineData(timelineData) {
    return timelineData.map(item => {
        // Базовый объект для элемента временной оси
        const timelineItem = {
            id: item.id,
            group: item.log_type,
            start: new Date(item.timestamp),
            title: getItemTitle(item),
            className: getItemClass(item)
        };
        
        // Дополнительные свойства
        if (item.content) {
            timelineItem.content = item.content;
        }
        
        if (item.end_time) {
            timelineItem.end = new Date(item.end_time);
            timelineItem.type = 'range';
        } else {
            timelineItem.type = 'point';
        }
        
        return timelineItem;
    });
}

/**
 * Получение заголовка для элемента временной оси
 */
function getItemTitle(item) {
    switch(item.log_type) {
        case 'experience':
            return `Ошибка: ${item.error_message || ''}`;
        case 'insights':
            return `Решение: ${item.solution || ''}`;
        case 'karma':
            return `Повторная ошибка (${item.count}x): ${item.error_message || ''}`;
        case 'hypotheses':
            return `Гипотеза: ${item.hypothesis || item.result || ''}`;
        default:
            return item.content || '';
    }
}

/**
 * Получение CSS класса для элемента временной оси
 */
function getItemClass(item) {
    switch(item.log_type) {
        case 'experience':
            return 'timeline-item-error';
        case 'insights':
            return 'timeline-item-insight';
        case 'karma':
            return 'timeline-item-karma';
        case 'hypotheses':
            return 'timeline-item-hypothesis';
        default:
            return '';
    }
}

/**
 * Отображение подробностей о событии временной оси
 */
function showEventDetails(itemId, timelineData) {
    const detailsContainer = document.getElementById('eventDetails');
    if (!detailsContainer) return;
    
    // Найдем событие по ID
    const event = timelineData.find(item => item.id === itemId);
    if (!event) return;
    
    // Формируем HTML для отображения деталей
    let html = `<div class="card">`;
    
    // Заголовок карточки зависит от типа события
    if (event.log_type === 'experience') {
        html += `<div class="card-header bg-danger text-white">
            <h5><i class="fas fa-exclamation-triangle"></i> Ошибка</h5>
        </div>`;
    } else if (event.log_type === 'insights') {
        html += `<div class="card-header bg-success text-white">
            <h5><i class="fas fa-lightbulb"></i> Решение</h5>
        </div>`;
    } else if (event.log_type === 'karma') {
        html += `<div class="card-header bg-warning">
            <h5><i class="fas fa-sync"></i> Повторная ошибка</h5>
        </div>`;
    } else if (event.log_type === 'hypotheses') {
        html += `<div class="card-header bg-info text-white">
            <h5><i class="fas fa-flask"></i> Гипотеза</h5>
        </div>`;
    }
    
    html += `<div class="card-body">
        <p><strong>Время:</strong> ${new Date(event.timestamp).toLocaleString()}</p>`;
    
    // Специфичное содержимое в зависимости от типа события
    if (event.log_type === 'experience') {
        html += `
            <p><strong>Сообщение об ошибке:</strong></p>
            <div class="alert alert-danger">${event.error_message || 'Нет сообщения'}</div>`;
        
        if (event.error_hash) {
            html += `<p><strong>Хеш ошибки:</strong> <code>${event.error_hash}</code></p>`;
            
            // Добавляем кнопку для поиска связанных событий
            html += `<button class="btn btn-sm btn-primary find-related-events" data-hash="${event.error_hash}">
                <i class="fas fa-search"></i> Найти связанные события
            </button>`;
        }
    } else if (event.log_type === 'insights') {
        html += `
            <p><strong>Решение:</strong></p>
            <div class="alert alert-success">${event.solution || 'Нет описания решения'}</div>`;
        
        if (event.error_hash) {
            html += `<p><strong>Хеш связанной ошибки:</strong> <code>${event.error_hash}</code></p>`;
            
            // Добавляем кнопку для поиска связанных событий
            html += `<button class="btn btn-sm btn-primary find-related-events" data-hash="${event.error_hash}">
                <i class="fas fa-search"></i> Найти связанные события
            </button>`;
        }
    } else if (event.log_type === 'karma') {
        html += `
            <p><strong>Повторная ошибка:</strong></p>
            <div class="alert alert-warning">${event.error_message || 'Нет сообщения'}</div>
            <p><strong>Количество повторений:</strong> ${event.count}</p>`;
        
        if (event.error_hash) {
            html += `<p><strong>Хеш ошибки:</strong> <code>${event.error_hash}</code></p>`;
            
            // Добавляем кнопку для поиска связанных событий
            html += `<button class="btn btn-sm btn-primary find-related-events" data-hash="${event.error_hash}">
                <i class="fas fa-search"></i> Найти связанные события
            </button>`;
        }
    } else if (event.log_type === 'hypotheses') {
        if (event.hypothesis) {
            html += `
                <p><strong>Гипотеза:</strong></p>
                <div class="alert alert-info">${event.hypothesis}</div>`;
        }
        
        if (event.result) {
            html += `
                <p><strong>Результат:</strong></p>
                <div class="alert alert-light">${event.result}</div>`;
        }
        
        if (event.status) {
            let statusBadge = '';
            if (event.status === 'confirmed') {
                statusBadge = '<span class="badge bg-success">Подтверждена</span>';
            } else if (event.status === 'rejected') {
                statusBadge = '<span class="badge bg-danger">Отклонена</span>';
            } else {
                statusBadge = '<span class="badge bg-warning">В процессе</span>';
            }
            
            html += `<p><strong>Статус:</strong> ${statusBadge}</p>`;
        }
    }
    
    html += `</div></div>`;
    
    // Обновляем контейнер с деталями события
    detailsContainer.innerHTML = html;
    
    // Обработчик для кнопки поиска связанных событий
    detailsContainer.querySelectorAll('.find-related-events').forEach(button => {
        button.addEventListener('click', function() {
            const hash = this.getAttribute('data-hash');
            findRelatedEvents(hash, timelineData);
        });
    });
}

/**
 * Поиск и отображение связанных событий по хешу ошибки
 */
function findRelatedEvents(hash, timelineData) {
    const relatedContainer = document.getElementById('relatedEvents');
    if (!relatedContainer) return;
    
    // Находим все события с указанным хешем
    const relatedEvents = timelineData.filter(event => event.error_hash === hash);
    
    if (relatedEvents.length === 0) {
        relatedContainer.innerHTML = `<div class="alert alert-info">Связанные события не найдены для хеша <code>${hash}</code></div>`;
        return;
    }
    
    // Группировка событий по типу
    const errors = relatedEvents.filter(e => e.log_type === 'experience');
    const insights = relatedEvents.filter(e => e.log_type === 'insights');
    const karma = relatedEvents.filter(e => e.log_type === 'karma');
    const hypotheses = relatedEvents.filter(e => e.log_type === 'hypotheses');
    
    let html = `<div class="card mb-4">
        <div class="card-header bg-primary text-white">
            <h5><i class="fas fa-link"></i> Связанные события для хеша <code>${hash}</code></h5>
        </div>
        <div class="card-body">`;
    
    // Сводка найденных событий
    html += `<div class="row mb-3">
        <div class="col-md-3">
            <div class="alert alert-danger text-center mb-0">
                <h6>Ошибки</h6>
                <h4>${errors.length}</h4>
            </div>
        </div>
        <div class="col-md-3">
            <div class="alert alert-success text-center mb-0">
                <h6>Решения</h6>
                <h4>${insights.length}</h4>
            </div>
        </div>
        <div class="col-md-3">
            <div class="alert alert-warning text-center mb-0">
                <h6>Повторы</h6>
                <h4>${karma.length}</h4>
            </div>
        </div>
        <div class="col-md-3">
            <div class="alert alert-info text-center mb-0">
                <h6>Гипотезы</h6>
                <h4>${hypotheses.length}</h4>
            </div>
        </div>
    </div>`;
    
    // Таблица связанных событий
    html += `<div class="table-responsive">
        <table class="table table-striped table-hover">
            <thead>
                <tr>
                    <th>Время</th>
                    <th>Тип</th>
                    <th>Содержание</th>
                </tr>
            </thead>
            <tbody>`;
    
    // Сортировка событий по времени
    relatedEvents.sort((a, b) => new Date(a.timestamp) - new Date(b.timestamp));
    
    relatedEvents.forEach(event => {
        // Определяем стиль и значок в зависимости от типа события
        let badgeClass = '';
        let icon = '';
        let content = '';
        
        if (event.log_type === 'experience') {
            badgeClass = 'bg-danger';
            icon = 'fas fa-exclamation-triangle';
            content = event.error_message || '';
        } else if (event.log_type === 'insights') {
            badgeClass = 'bg-success';
            icon = 'fas fa-lightbulb';
            content = event.solution || '';
        } else if (event.log_type === 'karma') {
            badgeClass = 'bg-warning text-dark';
            icon = 'fas fa-sync';
            content = `${event.error_message || ''} (x${event.count})`;
        } else if (event.log_type === 'hypotheses') {
            badgeClass = 'bg-info';
            icon = 'fas fa-flask';
            content = event.hypothesis || event.result || '';
        }
        
        html += `<tr>
            <td>${new Date(event.timestamp).toLocaleString()}</td>
            <td><span class="badge ${badgeClass}"><i class="${icon}"></i></span></td>
            <td>${content}</td>
        </tr>`;
    });
    
    html += `</tbody></table></div>`;
    
    // Добавляем анализ паттерна если есть инсайты и повторы
    if (insights.length > 0 && karma.length > 0) {
        html += `<div class="pattern-container mt-3">
            <div class="pattern-title"><i class="fas fa-chart-line"></i> Анализ паттерна</div>
            <div class="pattern-description">`;
        
        if (insights.length > 0 && errors.length + karma.length > insights.length) {
            html += `<p>Обнаружены повторяющиеся ошибки даже после предложенного решения. Возможно, решение не полностью устраняет проблему или применяется не во всех случаях.</p>`;
        } else if (insights.length > 0) {
            html += `<p>Для данной проблемы имеется решение, которое помогло устранить повторение ошибок.</p>`;
        }
        
        if (karma.length > 3) {
            html += `<p class="text-danger"><strong>Внимание!</strong> Высокая частота повторения этой ошибки (${karma.length} раз) указывает на критическую проблему, требующую немедленного решения.</p>`;
        }
        
        html += `</div></div>`;
    }
    
    html += `</div></div>`;
    
    // Обновляем контейнер со связанными событиями
    relatedContainer.innerHTML = html;
}

/**
 * Функция для форматирования JSON в удобочитаемом виде с подсветкой синтаксиса
 */
function formatJSON(json) {
    if (typeof json !== 'string') {
        json = JSON.stringify(json, null, 2);
    }
    
    // Замена символов для безопасного отображения в HTML
    json = json.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
    
    // Подсветка синтаксиса с помощью регулярных выражений
    return json.replace(/("(\\u[a-zA-Z0-9]{4}|\\[^u]|[^\\"])*"(\s*:)?|\b(true|false|null)\b|-?\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?)/g, function(match) {
        let cls = 'json-number';
        if (/^"/.test(match)) {
            if (/:$/.test(match)) {
                cls = 'json-key';
            } else {
                cls = 'json-string';
            }
        } else if (/true|false/.test(match)) {
            cls = 'json-boolean';
        } else if (/null/.test(match)) {
            cls = 'json-null';
        }
        return '<span class="' + cls + '">' + match + '</span>';
    });
}
