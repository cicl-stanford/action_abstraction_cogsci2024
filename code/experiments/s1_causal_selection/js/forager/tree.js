/**
 * Class representing a Tree in the Gridworld.
 * @param {number} x - The x-coordinate of the tree on the grid.
 * @param {number} y - The y-coordinate of the tree on the grid.
 * @param {number} reward - The number of berries on the tree.
 * @param {boolean} isVisible - Determines if the berries on the tree are visible.
 * @param {number} cellSize - The size of a grid cell in pixels.
 * @param {Array} leaves - Array of leaf objects for the tree, if any.
 * @param {Array} berries - Array of berry objects for the tree, if any.
 */
class Tree {
  constructor(x, y, reward, isVisible, cellSize, leaves = [], berries = []) {
    this.canvas = document.getElementById('gridworldCanvas');
    this.ctx = this.canvas.getContext('2d');
    this.x = x;
    this.y = y;
    this.reward = reward;
    this.isVisible = isVisible;
    this.cellSize = cellSize;

    // Tree and berry colors
    this.treeColor = this.isVisible ? gs.tree.colors.visible : gs.tree.colors.invisible;
    this.berryColor = gs.tree.colors.berry;

    // Border properties for animation and path tracking
    this.borderColor = null;
    this.actualBorderColor = null;

    // Path and reachability properties
    this.isOnPath = false;
    this.isOnActualPath = false;
    this.isReachable = false;

    // Leaf and berry initialization
    this.leaves = leaves.length > 0 ? leaves : this.generateLeaves();
    this.berries = berries.length > 0 ? berries : this.generateBerries();

    // Hover and animation properties
    this.hoverScale = gs.tree.animations.hover.default_scale;
    this.hoverScaleMax = gs.tree.animations.hover.magnify_scale;
    this.hoverScaleMin = gs.tree.animations.hover.minimize_scale;
    this.hoverSaturation = this.treeColor.s;
    this.hoverBrightness = this.treeColor.l;
    this.hoverBerrySaturation = this.berryColor.s;
    this.isHovered = false;
    this.inAnimation = false;

    // Timeout properties for hover effects
    this.hoverTimeout = null;
    this.unhoverTimeout = null;

    // Position offsets for shaking animation
    this.offsetX = 0;
    this.offsetY = 0;
  }

  generateLeaves() {
    const maxRadius = this.cellSize * gs.tree.size.max_leaf_radius;
    const leafCount = gs.tree.min_leaves + Math.floor(Math.random() * (gs.tree.max_leaves - gs.tree.min_leaves + 1));

    let leaves = [];
    for (let i = 0; i < leafCount; i++) {
      const angle = Math.random() * Math.PI;
      const radiusX = maxRadius * (gs.tree.size.min_leaf_radius + Math.random() * (1 - gs.tree.size.min_leaf_radius));
      const radiusY = maxRadius * (gs.tree.size.min_leaf_radius + Math.random() * (1 - gs.tree.size.min_leaf_radius));
      const offsetX = (Math.random() - 0.5) * maxRadius;
      const offsetY = (Math.random() - 0.5) * maxRadius;
      leaves.push({ offsetX, offsetY, radiusX, radiusY, angle });
    }

    return leaves;
  }

  generateBerries() {
    const foliageRadius = this.cellSize * gs.tree.size.max_leaf_radius;
    const berryRadius = foliageRadius * gs.tree.size.berry_radius;
    const numCandidates = 30;

    let berries = [];
    for (let i = 0; i < this.reward; i++) {
      let bestCandidate, bestDistance = 0;
      for (let j = 0; j < numCandidates; ++j) {
        const angle = Math.random() * Math.PI * 2;
        const distance = Math.random() * foliageRadius;
        const candidate = {
          offsetX: distance * Math.cos(angle),
          offsetY: distance * Math.sin(angle),
          radius: berryRadius
        };

        let d = findClosestDistance(berries, candidate);
        if (d > bestDistance) {
          bestDistance = d;
          bestCandidate = candidate;
        }
      }
      berries.push(bestCandidate);
    }

    return berries;
  }

  draw(ctx) {
    const centerX = this.x * this.cellSize + this.cellSize / 2 + this.offsetX;
    const centerY = this.y * this.cellSize + this.cellSize / 2 + this.offsetY;

    const treeColor = this.inAnimation ? { h: this.treeColor.h, s: this.hoverSaturation, l: this.hoverBrightness } : this.treeColor;
    const berryColor = this.inAnimation ? { h: this.berryColor.h, s: this.hoverBerrySaturation, l: this.berryColor.l } : this.berryColor;
    const borderScale = this.inAnimation ? this.hoverScale : this.hoverScaleMax;

    const borderFill = this.inAnimation ? this.borderColor :
                       this.isOnActualPath ? this.actualBorderColor :
                       this.isOnPath ? this.borderColor : null;

    if (borderFill) this.drawLeaves(ctx, centerX, centerY, convertToHSL(borderFill), borderScale);

    this.drawLeaves(ctx, centerX, centerY, convertToHSL(treeColor));

    if (this.isVisible) {
      if (this.isHovered) {
        this.drawBerryNumbers(ctx, centerX, centerY, convertToHSL(berryColor));
      } else {
        this.drawBerries(ctx, centerX, centerY, convertToHSL(berryColor));
      }
    } else {
      this.drawQuestionMark(ctx, centerX, centerY, convertToHSL(berryColor));
    }
  }

  drawLeaves(ctx, centerX, centerY, leafColor, scale = 1.0) {
    ctx.fillStyle = leafColor;
    this.leaves.forEach(leaf => {
      ctx.beginPath();
      ctx.ellipse(centerX + leaf.offsetX * scale, centerY + leaf.offsetY * scale, leaf.radiusX * scale, leaf.radiusY * scale, leaf.angle, 0, Math.PI * 2);
      ctx.fill();
    });
  }

  drawBerries(ctx, centerX, centerY, berryColor) {
    this.berries.forEach(berry => {
      ctx.fillStyle = convertToHSL(gs.tree.colors.berry_border);
      ctx.beginPath();
      ctx.arc(centerX + berry.offsetX, centerY + berry.offsetY, berry.radius + gs.tree.size.berry_border, 0, Math.PI * 2);
      ctx.fill();

      ctx.fillStyle = berryColor;
      ctx.beginPath();
      ctx.arc(centerX + berry.offsetX, centerY + berry.offsetY, berry.radius, 0, Math.PI * 2);
      ctx.fill();
    });
  }

  drawBerryNumbers(ctx, centerX, centerY, numberColor) {
    ctx.fillStyle = numberColor;
    ctx.font = `${this.cellSize / 1.8}px Luminari`;
    ctx.textAlign = 'center';
    ctx.textBaseline = 'middle';
    ctx.fillText(this.reward, centerX, centerY);
  }

  drawQuestionMark(ctx, centerX, centerY, questionMarkColor) {
    ctx.fillStyle = questionMarkColor;
    ctx.font = `${this.cellSize / 1.6}px Luminari`;
    ctx.textAlign = 'center';
    ctx.textBaseline = 'middle';
    ctx.fillText('?', centerX, centerY);
  }

  shakeTree(callback) {
    this.isVisible = true;

    const startTime = Date.now();
    const duration = gs.tree.animations.shake.duration;
    const shakeAmplitude = gs.tree.animations.shake.amplitude;

    const shake = () => {
      const elapsed = Date.now() - startTime;
      const progress = elapsed / duration;

      if (progress < 1) {
        this.offsetX = Math.sin(progress * Math.PI * gs.tree.animations.shake.speed) * shakeAmplitude;
        requestAnimationFrame(shake);
      } else {
        this.offsetX = 0;
        this.berryColor = gs.tree.colors.berry_picked;
        if (callback) callback();
      }
    };
    shake();
    return this.berries.length;
  }

  errorShake() {
    const startTime = Date.now();
    const duration = gs.tree.animations.shake.duration;
    const shakeAmplitude = gs.tree.animations.shake.amplitude;

    const shake = () => {
      const elapsed = Date.now() - startTime;
      const progress = elapsed / duration;

      if (progress < 1) {
        this.offsetX = Math.sin(progress * Math.PI * gs.tree.animations.shake.speed_error) * shakeAmplitude;
        requestAnimationFrame(shake);
      } else {
        this.offsetX = 0;
      }
    };
    shake();
  }

  updatePathStatus(treeTrajectory) {
    this.isOnPath = treeTrajectory.some(coord => coord[0] === this.x && coord[1] === this.y);
  }

  updateActualPathStatus(treeTrajectory) {
    this.isOnActualPath = treeTrajectory.some(coord => coord[0] === this.x && coord[1] === this.y);
  }

  updateReachableStatus(lastTreePosition, remainingSteps) {
    const distance = Math.abs(lastTreePosition[0] - this.x) + Math.abs(lastTreePosition[1] - this.y);
    this.isReachable = distance <= remainingSteps;
  }

  controlTreeProperty(property, start, stop, duration, frameRate = 5) {
    makeRangeArr(start, stop, duration / frameRate).forEach((value, index) => {
      setTimeout(() => {
        this[property] = value;
      }, index * frameRate);
    });
  }

  animateHoverEffect(isPredictTrial) {
    clearTimeout(this.hoverTimeout);
    clearTimeout(this.unhoverTimeout);
    const animationDuration = gs.tree.animations.hover.duration;

    if (isPredictTrial) {
      this.inAnimation = true;

      const targetScale = this.isOnPath ? gs.tree.animations.hover.default_scale : this.isReachable ? this.hoverScaleMax : this.hoverScaleMin;
      this.hoverScale = this.isOnPath ? this.hoverScaleMax : this.hoverScale;
      const targetSaturation = this.isReachable || this.isOnPath ? this.treeColor.s : gs.tree.colors.unreachable.s;
      const targetBrightness = this.isReachable || this.isOnPath ? this.treeColor.l : gs.tree.colors.unreachable.l;
      const targetBerrySaturation = this.isReachable || this.isOnPath ? this.berryColor.s : gs.tree.colors.unreachable.s;

      this.controlTreeProperty('hoverScale', this.hoverScale, targetScale, animationDuration);
      this.controlTreeProperty('hoverSaturation', this.hoverSaturation, targetSaturation, animationDuration);
      this.controlTreeProperty('hoverBrightness', this.hoverBrightness, targetBrightness, animationDuration);
      this.controlTreeProperty('hoverBerrySaturation', this.hoverBerrySaturation, targetBerrySaturation, animationDuration);
    }

    this.hoverTimeout = setTimeout(() => {
      const startUnhover = setInterval(() => {
        if (!this.isHovered) {
          this.animateUnhoverEffect(isPredictTrial);
          clearInterval(startUnhover);
        }
      }, 25);
    }, animationDuration);
  }

  animateUnhoverEffect(isPredictTrial) {
    const animationDuration = gs.tree.animations.hover.duration;

    if (isPredictTrial) {
      this.controlTreeProperty('hoverScale', this.hoverScale, this.isOnPath ? this.hoverScaleMax : gs.tree.animations.hover.default_scale, animationDuration);
      this.controlTreeProperty('hoverSaturation', this.hoverSaturation, this.treeColor.s, animationDuration);
      this.controlTreeProperty('hoverBrightness', this.hoverBrightness, this.treeColor.l, animationDuration);
      this.controlTreeProperty('hoverBerrySaturation', this.hoverBerrySaturation, this.berryColor.s, animationDuration);
    }

    this.unhoverTimeout = setTimeout(() => {
      this.inAnimation = false;
    }, animationDuration);
  }

  destroy() {
    if (this.hoverTimeout) {
      clearTimeout(this.hoverTimeout);
      this.hoverTimeout = null;
    }
    if (this.unhoverTimeout) {
      clearTimeout(this.unhoverTimeout);
      this.unhoverTimeout = null;
    }

    this.canvas = null;
    this.ctx = null;
    this.leaves = null;
    this.berries = null;
  }
}