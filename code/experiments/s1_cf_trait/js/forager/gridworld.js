/**
 * Class representing the Gridworld environment.
 * @param {Object} data - Data for setting up the gridworld.
 * @param {boolean} isPredictTrial - Indicates if it is a predict trial.
 * @param {Array} [treesData=null] - Optional existing tree data.
 */
class Gridworld {
  constructor(data, isPredictTrial, treesData = null) {
    this.data = data;
    this.isPredictTrial = isPredictTrial;
    this.canvas = document.getElementById('gridworldCanvas');
    this.ctx = this.canvas.getContext('2d');
    this.cellSize = this.canvas.width / this.data.rows;

    this.agentStartPosition = [this.data.agent_start_position[0] - 1, this.data.agent_start_position[1] - 1];
    this.agent = new Agent(this.agentStartPosition[0], this.agentStartPosition[1], this.data.agent_type, this.cellSize);
    this.totalSteps = data.total_steps;
    this.remainingSteps = data.total_steps;

    this.tileInfo = {
      baseTileSize: gs.tile.size.base,
      pathTileSize: gs.tile.size.path,
      realTileSize: gs.tile.size.real,
      baseColor: gs.tile.colors.default,
      pathColor: gs.tile.colors[`${this.data.agent_type}_path`],
      cornerColorN: gs.tile.colors.Ncorner,
      cornerColorS: gs.tile.colors.Scorner
    }

    this.trees = treesData ? treesData.map(treeData =>
      new Tree(treeData.x, treeData.y, treeData.reward, treeData.isVisible, this.cellSize, treeData.leaves, treeData.berries)
    ) : this.data.tree_positions.map((location, index) =>
      new Tree(location[0] - 1, location[1] - 1, this.data.tree_rewards[index], this.data.tree_visibility[index], this.cellSize)
    );

    this.trees.forEach(tree => {
      tree.borderColor = isPredictTrial ? gs.tree.colors[`${this.data.agent_type}_border`] : gs.tree.colors[`${this.data.agent_type}_border_predict`];
      tree.actualBorderColor = this.agent.fillStyle;
    });

    this.basket = !isPredictTrial ? new Basket('basketCanvas') : null;
    this.animationFrameId = null;

    this.treeTrajectory = [];
    this.coordinateTrajectory = [];
    this.coordinateTrajectoryQueue = [];
    this.coordinateTrajectoryUndoQueue = [];
    this.actualTreeTrajectory = [];
    this.actualTrajectory = [];
    this.actualTrajectoryQueue = [];
    this.clickEvents = [];
    this.inObservation = true;
    this.clickDebounceTimeout = null;

    this.init();
  }

  init() {
    this.animationFrameId = requestAnimationFrame(this.loop.bind(this));

    if (this.isPredictTrial) {
      this.canvas.addEventListener('click', this.handleCanvasClick.bind(this));
      this.canvas.addEventListener('mousemove', this.handleCanvasHover.bind(this));
      this.canvas.addEventListener('mouseout', this.handleCanvasHoverout.bind(this));
    } else {
      this.canvas.addEventListener('mousemove', this.handleCanvasHover.bind(this));
    }
  }

  collectBerries(n) {
    this.basket.addBerries(n);
  }

  handleCanvasClick(event) {
    if (this.clickDebounceTimeout) {
      clearTimeout(this.clickDebounceTimeout);
    }
    this.clickDebounceTimeout = setTimeout(() => {
      this.processCanvasClick(event);
    }, 25);
  }

  loop() {
    this.updateCanvas();
    this.updateTrees();
    this.updateSubmitButton();
    this.animationFrameId = requestAnimationFrame(this.loop.bind(this));
  }

  updateTrees() {
    this.trees.forEach(tree => {
      const lastTreePosition = this.treeTrajectory.length > 0 ? this.treeTrajectory[this.treeTrajectory.length - 1] : this.agentStartPosition;
      tree.updatePathStatus(this.treeTrajectory);
      tree.updateActualPathStatus(this.actualTreeTrajectory);
      tree.updateReachableStatus(lastTreePosition, this.remainingSteps);
    });
  }

  updateSubmitButton() {
    const submitBtn = document.getElementById('submitBtn');
    if (submitBtn) {
      submitBtn.disabled = this.isPredictTrial ? this.canReachMoreTrees() : this.inObservation;
      submitBtn.style.opacity = !(this.isPredictTrial ? 0 : 1) | 0;
    }
  }

  updateCanvas() {
    this.ctx.clearRect(0, 0, this.canvas.width, this.canvas.height);
    this.ctx.fillStyle = gs.background_color;
    this.ctx.fillRect(0, 0, this.canvas.width, this.canvas.height);

    this.drawGrid();
    this.drawTrees();
    this.drawAgent();
    this.updateRemainingStepsDisplay();
  }

  drawGrid() {
    for (let i = 0; i < this.data.rows; i++) {
      for (let j = 0; j < this.data.cols; j++) {
        const isCornerTile = (i === 0 && j === 0) || (i === this.data.rows - 1 && j === this.data.cols - 1);
        const cornerColor = (i === 0 && j === 0) ? this.tileInfo.cornerColorN : this.tileInfo.cornerColorS;
        let fillStyle = isCornerTile ? convertToHSL(cornerColor) : convertToHSL(this.tileInfo.baseColor);
        let tileSize = this.tileInfo.baseTileSize;

        if (this.coordinateTrajectoryQueue.some(coord => coord[0] === j && coord[1] === i)) {
          const coordIndex = this.coordinateTrajectoryQueue.findIndex(coord => coord[0] === j && coord[1] === i);
          [fillStyle, tileSize] = this.transitionTile(coordIndex);
        } else if (this.coordinateTrajectoryUndoQueue.some(coord => coord[0] === j && coord[1] === i)) {
          const coordIndex = this.coordinateTrajectoryUndoQueue.findIndex(coord => coord[0] === j && coord[1] === i);
          [fillStyle, tileSize] = this.transitionTile(coordIndex, true);
        } else if (this.coordinateTrajectory.some(coord => coord[0] === j && coord[1] === i)) {
          const coordIndex = this.coordinateTrajectory.findIndex(coord => coord[0] === j && coord[1] === i);
          [fillStyle, tileSize] = this.isPredictTrial ?
            this.getPulsingPathProperties(coordIndex) :
            [convertToHSL(gs.tile.colors[`${this.data.agent_type}_path_predict`]), this.tileInfo.pathTileSize];

          if (!this.isPredictTrial) {
            drawRoundedRect(
              this.ctx,
              j * this.cellSize + (this.cellSize * (1 - tileSize) / 2),
              i * this.cellSize + (this.cellSize * (1 - tileSize) / 2),
              this.cellSize * tileSize,
              this.cellSize * tileSize,
              gs.tile.corner_radius,
              fillStyle
            );
          }
        }

        if (this.actualTrajectoryQueue.some(coord => coord[0] === j && coord[1] === i)) {
          const coordIndex = this.actualTrajectoryQueue.findIndex(coord => coord[0] === j && coord[1] === i);
          tileSize = this.transitionActualTile(coordIndex);
          fillStyle = convertToHSL(gs.tile.colors[`${this.data.agent_type}_path_actual`]);
        } else if (this.actualTrajectory.some(coord => coord[0] === j && coord[1] === i)) {
          tileSize = this.tileInfo.realTileSize;
          fillStyle = convertToHSL(gs.tile.colors[`${this.data.agent_type}_path_actual`]);
        }

        drawRoundedRect(
          this.ctx,
          j * this.cellSize + (this.cellSize * (1 - tileSize) / 2),
          i * this.cellSize + (this.cellSize * (1 - tileSize) / 2),
          this.cellSize * tileSize,
          this.cellSize * tileSize,
          gs.tile.corner_radius,
          fillStyle
        );
      }
    }
  }

  destroy() {
    cancelAnimationFrame(this.animationFrameId);
    this.canvas.removeEventListener('click', this.handleCanvasClick);
    this.canvas.removeEventListener('mousemove', this.handleCanvasHover);
    this.canvas.removeEventListener('mouseout', this.handleCanvasHoverout);

    this.trees.forEach(tree => tree.destroy());
    this.trees = null;
    if (this.basket) {
      this.basket.destroy();
      this.basket = null;
    }
    this.agent = null;
    this.treeTrajectory = null;
    this.coordinateTrajectory = null;
    this.coordinateTrajectoryQueue = null;
    this.coordinateTrajectoryUndoQueue = null;
    this.actualTreeTrajectory = null;
    this.actualTrajectory = null;
    this.actualTrajectoryQueue = null;
    this.clickEvents = null;

    if (this.clickDebounceTimeout) {
      clearTimeout(this.clickDebounceTimeout);
      this.clickDebounceTimeout = null;
    }
  }

  transitionTile(coordIndex, isUndo = false) {
    const totalFrames = gs.tile.animations.transition.frames;
    let queue = isUndo ? this.coordinateTrajectoryUndoQueue : this.coordinateTrajectoryQueue;
    const frame = queue[coordIndex][2];

    if (frame === totalFrames) {
      if (isUndo) {
        this.coordinateTrajectory.pop();
        this.coordinateTrajectoryUndoQueue.shift();
      } else {
        this.coordinateTrajectory.push([queue[coordIndex][0], queue[coordIndex][1]]);
        this.coordinateTrajectoryQueue.shift();
      }
      return isUndo ? [convertToHSL(this.tileInfo.baseColor), this.tileInfo.baseTileSize] :
                      this.getPulsingPathProperties(this.coordinateTrajectory.length - 1);
    } else {
      queue[coordIndex][2] += 1;
      const progress = isUndo ? (totalFrames - frame) / totalFrames : frame / totalFrames;
      const tileSize = this.tileInfo.baseTileSize + (this.tileInfo.pathTileSize - this.tileInfo.baseTileSize) * progress;

      const h = interpolateHue(this.tileInfo.baseColor.h, this.tileInfo.pathColor.h, progress);
      const s = this.tileInfo.baseColor.s + (this.tileInfo.pathColor.s - this.tileInfo.baseColor.s) * progress;
      const l = this.tileInfo.baseColor.l + (this.tileInfo.pathColor.l - this.tileInfo.baseColor.l) * progress;
      const fillStyle = convertToHSL({ h, s, l });

      return [fillStyle, tileSize];
    }
  }

  transitionActualTile(coordIndex) {
    const totalFrames = gs.tile.animations.transition.frames;
    const frame = this.actualTrajectoryQueue[coordIndex][2];

    if (frame === totalFrames) {
      this.actualTrajectory.push([this.actualTrajectoryQueue[coordIndex][0], this.actualTrajectoryQueue[coordIndex][1]]);
      this.actualTrajectoryQueue.shift();
      return this.tileInfo.realTileSize;
    } else {
      this.actualTrajectoryQueue[coordIndex][2] += 1;
      const progress = frame / totalFrames;
      const tileSize = this.tileInfo.baseTileSize + (this.tileInfo.realTileSize - this.tileInfo.baseTileSize) * progress;
      return tileSize;
    }
  }

  getPulsingPathProperties(coordIndex) {
    const time = Date.now();
    const period = gs.tile.animations.pulse.period;
    const separation = gs.tile.animations.pulse.separation;
    const saturationRange = gs.tile.animations.pulse.saturation_range;
    const tileRange = gs.tile.animations.pulse.tile_range;

    const wavePosition = 2 * Math.PI * (time / period - coordIndex / this.data.total_steps);

    const saturation = this.tileInfo.pathColor.s + ((wavePosition % (separation * Math.PI)) < Math.PI) * Math.sin(wavePosition) * saturationRange;
    const tileSize = this.tileInfo.pathTileSize + ((wavePosition % (separation * Math.PI)) < Math.PI) * Math.sin(wavePosition) * tileRange;

    return [`hsl(${this.tileInfo.pathColor.h}, ${saturation}%, ${this.tileInfo.pathColor.l}%)`, tileSize];
  }

  drawTrees() {
    this.trees.forEach(tree => tree.draw(this.ctx));
  }

  drawAgent() {
    this.agent.draw(this.ctx);
  }

  processCanvasClick(event) {
    const rect = this.canvas.getBoundingClientRect();
    const x = event.clientX - rect.left;
    const y = event.clientY - rect.top;
    const gridX = Math.floor(x / this.cellSize);
    const gridY = Math.floor(y / this.cellSize);

    const treeIndex = this.trees.findIndex(tree => tree.x === gridX && tree.y === gridY);
    const trajectoryIndex = this.treeTrajectory.findIndex(coords => coords[0] === gridX && coords[1] === gridY);
    const lastPlannedTree = this.treeTrajectory[this.treeTrajectory.length - 1];

    let clickEvent = {
      click_location: [gridX, gridY],
      click_time: Date.now(),
      is_tree: treeIndex !== -1,
      is_agent: gridX === this.agentStartPosition[0] && gridY === this.agentStartPosition[1],
      is_undo_tree: trajectoryIndex !== -1,
      is_reachable: null,
      remaining_steps_before: this.remainingSteps,
      remaining_steps_after: null,
    }

    if (lastPlannedTree && lastPlannedTree[0] === gridX && lastPlannedTree[1] === gridY) {
      // Do nothing if the clicked tree is the current planned tree
    } else if (clickEvent.is_agent) {
      const pathToUndo = this.coordinateTrajectory.slice().reverse();
      this.enqueuePathAnimation(pathToUndo, true);

      this.treeTrajectory = [];
      this.remainingSteps = this.totalSteps;
    } else if (clickEvent.is_undo_tree) {
      const coordinateTrajectoryIndex = this.coordinateTrajectory.findIndex(coords => coords[0] === gridX && coords[1] === gridY);
      const pathToUndo = this.coordinateTrajectory.slice(coordinateTrajectoryIndex + 1).reverse();
      this.enqueuePathAnimation(pathToUndo, true);

      this.treeTrajectory = this.treeTrajectory.slice(0, trajectoryIndex + 1);
      this.remainingSteps = this.remainingSteps + pathToUndo.length;
    } else if (clickEvent.is_tree) {
      const lastTree = this.treeTrajectory[this.treeTrajectory.length - 1] || this.agentStartPosition;
      const distance = Math.abs(lastTree[0] - gridX) + Math.abs(lastTree[1] - gridY);
      if (distance <= this.remainingSteps) {
        this.updateGridPath(gridX, gridY);
        this.remainingSteps -= distance;
        clickEvent.is_reachable = true;
      } else {
        this.trees[treeIndex].errorShake();
        clickEvent.is_reachable = false;
      }
    }
    clickEvent.remaining_steps_after = this.remainingSteps;
    this.clickEvents.push(clickEvent);
  }

  handleCanvasHover(event) {
    const rect = this.canvas.getBoundingClientRect();
    const mouseX = event.clientX - rect.left;
    const mouseY = event.clientY - rect.top;
    const gridX = Math.floor(mouseX / this.cellSize);
    const gridY = Math.floor(mouseY / this.cellSize);

    const isOverStart = this.isMouseOverStart(mouseX, mouseY);
    if (isOverStart && !this.agent.isHovered) {
      this.agent.isHovered = true;
      this.agent.animateHoverEffect();
    } else if (!isOverStart && this.agent.isHovered) {
      this.agent.isHovered = false;
    }

    this.trees.forEach(tree => {
      const isOverTree = this.isMouseOverTree(mouseX, mouseY, tree);
      if (isOverTree && !tree.isHovered) {
        tree.isHovered = true;

        if (this.treeTrajectory.length > 0) {
          const lastTree = this.treeTrajectory[this.treeTrajectory.length - 1];
          if (tree.x === lastTree[0] && tree.y === lastTree[1]) return;
        }
        tree.animateHoverEffect(this.isPredictTrial);

      } else if (!isOverTree && tree.isHovered) {
        tree.isHovered = false;
      }
    });
  }

  handleCanvasHoverout(event) {
    this.trees.forEach(tree => { tree.isHovered = false });
    this.agent.isHovered = false;
  }

  isMouseOverTree(mouseX, mouseY, tree) {
    const centerX = tree.x * this.cellSize + this.cellSize / 2;
    const centerY = tree.y * this.cellSize + this.cellSize / 2;
    const treeRadius = this.cellSize / 2;
    const dx = mouseX - centerX;
    const dy = mouseY - centerY;
    return dx ** 4 + dy ** 4 < treeRadius ** 4;
  }

  isMouseOverStart(mouseX, mouseY) {
    const centerX = this.agentStartPosition[0] * this.cellSize + this.cellSize / 2;
    const centerY = this.agentStartPosition[1] * this.cellSize + this.cellSize / 2;
    const dx = mouseX - centerX;
    const dy = mouseY - centerY;
    return dx ** 4 + dy ** 4 < (this.cellSize / 2) ** 4;
  }

  updateGridPath(gridX, gridY) {
    const lastTree = this.treeTrajectory.length > 0 ? this.treeTrajectory[this.treeTrajectory.length - 1] : this.agentStartPosition;
    const path = this.generatePath(lastTree[0], lastTree[1], gridX, gridY);

    path.shift();
    path.push([gridX, gridY]);

    this.enqueuePathAnimation(path);
    this.treeTrajectory.push([gridX, gridY]);
  }

  enqueuePathAnimation(path, isUndo = false) {
    path.forEach((coords, index) => {
      setTimeout(() => {
        isUndo ? this.coordinateTrajectoryUndoQueue.push([coords[0], coords[1], 0]) :
                 this.coordinateTrajectoryQueue.push([coords[0], coords[1], 0]);
        if (index === path.length - 1) {
          if (this.treeTrajectory.length > 0) {
            const lastCoord = this.treeTrajectory[this.treeTrajectory.length - 1];
            const lastTree = this.trees.find(tree => tree.x === lastCoord[0] && tree.y === lastCoord[1]);
            lastTree.inAnimation = false;
          }
        }
      }, index * gs.tile.animations.transition.delay);
    });
  }

  canReachMoreTrees() {
    const lastTreeCoords = this.treeTrajectory.length > 0 ? this.treeTrajectory[this.treeTrajectory.length - 1] : this.agentStartPosition;

    return this.trees.some(tree => {
      if (!this.coordinateTrajectory.some(coord => coord[0] === tree.x && coord[1] === tree.y)) {
        const distance = Math.abs(lastTreeCoords[0] - tree.x) + Math.abs(lastTreeCoords[1] - tree.y);
        return distance <= this.remainingSteps;
      }
      return false;
    });
  }

  generatePath(startX, startY, endX, endY) {
    const path = [];
    let currentX = startX;
    let currentY = startY;

    while (currentX !== endX) {
      path.push([currentX, currentY]);
      currentX += (endX > currentX) ? 1 : -1;
    }

    while (currentY !== endY) {
      path.push([currentX, currentY]);
      currentY += (endY > currentY) ? 1 : -1;
    }

    return path;
  }

  updateRemainingStepsDisplay() {
    const remainingStepsElement = document.getElementById('remainingSteps');
    if (remainingStepsElement) {
      remainingStepsElement.innerText = 'Remaining Steps: ' + this.remainingSteps;
    }
  }

  animateAgentMovement(start, end, speed, isUndo = false, view_only = false, callback) {
    var dx = end[0] - start[0];
    var dy = end[1] - start[1];

    var agentSpeed = speed;
    var totalHorizontalSteps = Math.abs(dx) * agentSpeed;
    var totalVerticalSteps = Math.abs(dy) * agentSpeed;

    var sx = dx !== 0 ? (dx > 0 ? 1 : -1) / agentSpeed : 0;
    var sy = dy !== 0 ? (dy > 0 ? 1 : -1) / agentSpeed : 0;

    var currentHorizontalStep = 0;
    var currentVerticalStep = 0;

    const updatePosition = (axis, step) => {
      this.agent[axis] += step;
      if ((axis === 'x' ? currentHorizontalStep++ : currentVerticalStep++) % agentSpeed === Math.round(agentSpeed / 2) && !view_only) {
        this.remainingSteps += isUndo ? 1 : -1;

        const gridX = Math.round(this.agent.x);
        const gridY = Math.round(this.agent.y);

        this.actualTrajectoryQueue.push([gridX, gridY, 0]);

        if (this.data.tree_positions.some(treePos => treePos[0] - 1 === gridX && treePos[1] - 1 === gridY)) {
          this.actualTreeTrajectory.push([gridX, gridY]);
        }
      }
    };

    const animateStep = () => {
      if (!isUndo ? currentHorizontalStep < totalHorizontalSteps : currentVerticalStep < totalVerticalSteps) {
        updatePosition(!isUndo ? 'x' : 'y', !isUndo ? sx : sy);
      } else if (!isUndo ? currentVerticalStep < totalVerticalSteps : currentHorizontalStep < totalHorizontalSteps) {
        if ((!isUndo ? currentVerticalStep : currentHorizontalStep) === 0) {
          this.agent[!isUndo ? 'x' : 'y'] = end[!isUndo ? 0 : 1];
        }
        updatePosition(!isUndo ? 'y' : 'x', !isUndo ? sy : sx);
      }

      if (currentHorizontalStep < totalHorizontalSteps || currentVerticalStep < totalVerticalSteps) {
        requestAnimationFrame(animateStep);
      } else {
        this.agent.x = end[0];
        this.agent.y = end[1];

        if (callback && typeof callback === 'function') {
          callback();
        }
      }
    };

    requestAnimationFrame(animateStep);
  }
}