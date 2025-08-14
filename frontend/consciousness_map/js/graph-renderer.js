/**
 * Graph Renderer using D3.js Force Layout
 * MIT Media Lab inspired visualization techniques
 */

class GraphRenderer {
  constructor(containerId) {
    this.containerId = containerId;
    this.container = document.getElementById(containerId);
    this.width = this.container.clientWidth;
    this.height = this.container.clientHeight;
    this.nodes = [];
    this.links = [];
    this.simulation = null;
    this.svg = null;
    this.linkGroup = null;
    this.nodeGroup = null;
    this.labelGroup = null;
    this.tooltip = null;
    
    this.activeState = null;
    this.activeTransition = null;
    
    this.init();
    this.setupResizeListener();
  }
  
  /**
   * Инициализация графа
   */
  init() {
    // Создание SVG контейнера
    this.svg = d3.select(`#${this.containerId}`)
      .append('svg')
      .attr('width', this.width)
      .attr('height', this.height);
      
    // Группы для разных элементов визуализации
    this.linkGroup = this.svg.append('g').attr('class', 'links');
    this.nodeGroup = this.svg.append('g').attr('class', 'nodes');
    this.labelGroup = this.svg.append('g').attr('class', 'labels');
    
    // Настройка инструмента зума и перемещения
    const zoom = d3.zoom()
      .scaleExtent([0.5, 3])
      .on('zoom', (event) => {
        this.linkGroup.attr('transform', event.transform);
        this.nodeGroup.attr('transform', event.transform);
        this.labelGroup.attr('transform', event.transform);
      });
      
    this.svg.call(zoom);
    
    // Создание всплывающей подсказки
    this.tooltip = d3.select('body').append('div')
      .attr('class', 'tooltip')
      .style('opacity', 0);
    
    // Настройка физической симуляции
    this.simulation = d3.forceSimulation()
      .force('link', d3.forceLink().id(d => d.id).distance(CONFIG.graph.simulation.distance))
      .force('charge', d3.forceManyBody().strength(CONFIG.graph.simulation.strength))
      .force('center', d3.forceCenter(this.width / 2, this.height / 2))
      .force('collide', d3.forceCollide(CONFIG.graph.nodes.defaultRadius * 1.2))
      .on('tick', () => this.tick());
  }
  
  /**
   * Настройка обработчика изменения размера окна
   */
  setupResizeListener() {
    window.addEventListener('resize', () => {
      this.width = this.container.clientWidth;
      this.height = this.container.clientHeight;
      
      this.svg
        .attr('width', this.width)
        .attr('height', this.height);
        
      this.simulation
        .force('center', d3.forceCenter(this.width / 2, this.height / 2))
        .restart();
    });
  }
  
  /**
   * Обновление данных графа
   */
  updateData(data) {
    this.nodes = data.nodes;
    this.links = data.links;
    this.activeState = data.activeState;
    this.activeTransition = data.activeTransition;
    
    this.render();
  }
  
  /**
   * Рендеринг графа
   */
  render() {
    // Обновление связей
    const link = this.linkGroup
      .selectAll('.link')
      .data(this.links, d => d.id);
      
    // Удаление старых связей
    link.exit().remove();
    
    // Создание новых связей
    const linkEnter = link.enter()
      .append('line')
      .attr('class', 'link')
      .attr('stroke-width', d => 1 + Math.sqrt(d.count))
      .attr('stroke', d => this.getLinkColor(d))
      .on('mouseover', (event, d) => this.showLinkTooltip(event, d))
      .on('mouseout', () => this.hideTooltip());
      
    // Обновление существующих связей
    const linkUpdate = linkEnter.merge(link)
      .attr('stroke-width', d => {
        // Увеличиваем толщину для активного перехода
        if (this.activeTransition && 
            d.source.id === this.activeTransition.fromState && 
            d.target.id === this.activeTransition.toState) {
          return CONFIG.graph.links.activeWidth;
        }
        return 1 + Math.sqrt(d.count);
      })
      .attr('stroke', d => this.getLinkColor(d));
      
    // Обновление узлов
    const node = this.nodeGroup
      .selectAll('.node')
      .data(this.nodes, d => d.id);
      
    // Удаление старых узлов
    node.exit().remove();
    
    // Создание новых узлов
    const nodeEnter = node.enter()
      .append('circle')
      .attr('class', d => `node ${d.colorClass}`)
      .attr('r', CONFIG.graph.nodes.defaultRadius)
      .call(d3.drag()
        .on('start', (event, d) => this.dragstarted(event, d))
        .on('drag', (event, d) => this.dragged(event, d))
        .on('end', (event, d) => this.dragended(event, d)))
      .on('click', (event, d) => this.nodeClicked(d))
      .on('mouseover', (event, d) => this.showNodeTooltip(event, d))
      .on('mouseout', () => this.hideTooltip());
      
    // Обновление существующих узлов
    const nodeUpdate = nodeEnter.merge(node)
      .attr('r', d => {
        return d.id === this.activeState ? 
          CONFIG.graph.nodes.activeRadius : CONFIG.graph.nodes.defaultRadius;
      })
      .classed('node-selected', d => d.id === this.activeState);
      
    // Обновление меток
    const label = this.labelGroup
      .selectAll('.node-label')
      .data(this.nodes, d => d.id);
      
    // Удаление старых меток
    label.exit().remove();
    
    // Создание новых меток
    const labelEnter = label.enter()
      .append('text')
      .attr('class', 'node-label')
      .attr('text-anchor', 'middle')
      .attr('dy', d => CONFIG.graph.nodes.labelOffset)
      .style('font-size', `${CONFIG.graph.nodes.fontSize}px`)
      .text(d => d.label)
      .on('click', (event, d) => this.nodeClicked(d));
      
    // Добавляем узлы и связи в симуляцию
    this.simulation
      .nodes(this.nodes)
      .force('link').links(this.links);
      
    // Перезапускаем симуляцию
    this.simulation.alpha(0.3).restart();
    
    // Если активен переход, анимируем его
    if (this.activeTransition) {
      this.animateTransition(this.activeTransition);
    }
  }
  
  /**
   * Функция анимации перехода между состояниями
   */
  animateTransition(transition) {
    const fromNode = this.nodes.find(n => n.id === transition.fromState);
    const toNode = this.nodes.find(n => n.id === transition.toState);
    
    if (!fromNode || !toNode) return;
    
    // Анимируем переход с использованием временного узла
    const tempNode = {
      x: fromNode.x,
      y: fromNode.y,
      id: 'temp-transition-node'
    };
    
    // Создаем временный узел для анимации
    const transitionCircle = this.svg.append('circle')
      .attr('class', 'transition-node')
      .attr('r', 10)
      .attr('cx', fromNode.x)
      .attr('cy', fromNode.y)
      .attr('fill', '#ffffff')
      .attr('stroke', '#333333')
      .attr('stroke-width', 2)
      .attr('opacity', 0.8);
      
    // Анимируем движение от исходного к целевому узлу
    transitionCircle.transition()
      .duration(CONFIG.graph.links.transitionSpeed)
      .attr('cx', toNode.x)
      .attr('cy', toNode.y)
      .on('end', function() {
        // Удаляем временный узел
        d3.select(this).remove();
      });
  }
  
  /**
   * Функция обновления положений на каждом тике симуляции
   */
  tick() {
    // Обновляем положение связей
    this.linkGroup.selectAll('.link')
      .attr('x1', d => d.source.x)
      .attr('y1', d => d.source.y)
      .attr('x2', d => d.target.x)
      .attr('y2', d => d.target.y);
      
    // Обновляем положение узлов
    this.nodeGroup.selectAll('.node')
      .attr('cx', d => d.x)
      .attr('cy', d => d.y);
      
    // Обновляем положение меток
    this.labelGroup.selectAll('.node-label')
      .attr('x', d => d.x)
      .attr('y', d => d.y);
  }
  
  /**
   * Обработчики начала перетаскивания
   */
  dragstarted(event, d) {
    if (!event.active) this.simulation.alphaTarget(0.3).restart();
    d.fx = d.x;
    d.fy = d.y;
  }
  
  /**
   * Обработчик перетаскивания
   */
  dragged(event, d) {
    d.fx = event.x;
    d.fy = event.y;
  }
  
  /**
   * Обработчик окончания перетаскивания
   */
  dragended(event, d) {
    if (!event.active) this.simulation.alphaTarget(0);
    d.fx = null;
    d.fy = null;
  }
  
  /**
   * Обработчик клика по узлу
   */
  nodeClicked(d) {
    // Активируем выбранное состояние через сервис данных
    dataService.activateState(d.id);
    
    // Отображаем метрики состояния
    this.updateStateMetrics(d);
  }
  
  /**
   * Обновление метрик состояния в UI
   */
  updateStateMetrics(state) {
    if (!state || !state.metrics) return;
    
    // Обновляем индикаторы метрик
    document.getElementById('presence-level').style.width = 
      `${state.metrics.presence_level * 100}%`;
      
    document.getElementById('harmony-index').style.width = 
      `${state.metrics.harmony_index * 100}%`;
      
    document.getElementById('authenticity-score').style.width = 
      `${state.metrics.authenticity_score * 100}%`;
      
    document.getElementById('emotional-charge').style.width = 
      `${state.metrics.emotional_charge}%`;
      
    // Отображаем инсайт в панели
    const insightPanel = document.getElementById('current-insight');
    insightPanel.innerHTML = `
      <h4>${state.label}</h4>
      <p>${state.description}</p>
    `;
  }
  
  /**
   * Получение цвета для связи
   */
  getLinkColor(link) {
    // Определяем цвет связи на основе состояний и активности
    const sourceColor = d3.select(`.${link.source.colorClass}`).style('fill');
    const targetColor = d3.select(`.${link.target.colorClass}`).style('fill');
    
    // Если связь активна, используем более яркий цвет
    if (this.activeTransition && 
        link.source.id === this.activeTransition.fromState && 
        link.target.id === this.activeTransition.toState) {
      return targetColor || '#333333';
    }
    
    // Иначе используем градиент между цветами
    return sourceColor || '#999999';
  }
  
  /**
   * Отображение всплывающей подсказки для узла
   */
  showNodeTooltip(event, d) {
    this.tooltip.transition()
      .duration(200)
      .style('opacity', .9);
      
    this.tooltip.html(`
      <strong>${d.label}</strong>
      <p>${d.description}</p>
      <hr>
      <p>Присутствие: ${Math.round(d.metrics.presence_level * 100)}%</p>
      <p>Гармония: ${Math.round(d.metrics.harmony_index * 100)}%</p>
    `)
      .style('left', (event.pageX + 10) + 'px')
      .style('top', (event.pageY - 28) + 'px');
  }
  
  /**
   * Отображение всплывающей подсказки для связи
   */
  showLinkTooltip(event, d) {
    // Получаем инсайт для перехода
    const insightKey = `${d.source.id}_${d.target.id}`;
    const insight = CONFIG.transitionInsights[insightKey] || 
                    d.lastTransitionData?.insight || 
                    "Переход между состояниями сознания";
    
    this.tooltip.transition()
      .duration(200)
      .style('opacity', .9);
      
    this.tooltip.html(`
      <strong>${d.source.label} → ${d.target.label}</strong>
      <p>Триггер: ${d.triggerLabel}</p>
      <hr>
      <em>"${insight}"</em>
      <p>Количество переходов: ${d.count}</p>
    `)
      .style('left', (event.pageX + 10) + 'px')
      .style('top', (event.pageY - 28) + 'px');
  }
  
  /**
   * Скрытие всплывающей подсказки
   */
  hideTooltip() {
    this.tooltip.transition()
      .duration(500)
      .style('opacity', 0);
  }
}

// Функция инициализации после загрузки DOM
function initGraphRenderer() {
  const graphRenderer = new GraphRenderer('consciousness-graph');
  
  // Подписка на изменения данных
  dataService.subscribe(data => {
    graphRenderer.updateData(data);
    
    // Обновляем метрики для активного состояния
    if (data.activeState) {
      const activeNode = data.nodes.find(n => n.id === data.activeState);
      if (activeNode) {
        graphRenderer.updateStateMetrics(activeNode);
      }
    }
  });
}
