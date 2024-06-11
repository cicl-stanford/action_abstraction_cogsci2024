/**
 * Class representing an Agent in the Gridworld.
 * @param {number} x - The x-coordinate of the agent on the grid.
 * @param {number} y - The y-coordinate of the agent on the grid.
 * @param {string} agentType - The type of agent ('optimist' or 'pessimist').
 * @param {number} cellSize - The size of a grid cell in pixels.
 */
class Agent {
  constructor(x, y, agentType, cellSize) {
    this.x = x;
    this.y = y;
    this.agentType = agentType;
    this.cellSize = cellSize;
    this.fillStyle = gs.agent.colors[agentType];

    // Animation effects
    this.isHovered = false;
    this.inAnimation = false;
    this.hoverTimeout = null;
    this.unhoverTimeout = null;
    this.brightnessChange = 0;
  }

  draw(ctx) {
    const pixelX = (this.x + 0.5) * this.cellSize;
    const pixelY = (this.y + 0.5) * this.cellSize;
    const radius = this.cellSize * gs.agent.size.radius;

    ctx.fillStyle = convertToHSL({ h: this.fillStyle.h, s: this.fillStyle.s, l: this.fillStyle.l + this.brightnessChange });
    ctx.beginPath();
    ctx.arc(pixelX, pixelY, radius, 0, Math.PI * 2);
    ctx.fill();

    // Eyes
    const eyeRadius = radius * gs.agent.size.eye_radius;
    const eyeOffsetX = radius / 3;
    const eyeOffsetY = radius / 3;

    ctx.fillStyle = 'white';
    ctx.beginPath();
    ctx.arc(pixelX - eyeOffsetX, pixelY - eyeOffsetY, eyeRadius, 0, Math.PI * 2);
    ctx.fill();
    ctx.beginPath();
    ctx.arc(pixelX + eyeOffsetX, pixelY - eyeOffsetY, eyeRadius, 0, Math.PI * 2);
    ctx.fill();

    // Pupils
    const pupilRadius = eyeRadius * gs.agent.size.pupil_radius;
    const pupilOffsetX = eyeRadius / 4;
    const pupilOffsetY = eyeRadius / 6;

    ctx.fillStyle = 'black';
    ctx.beginPath();
    ctx.arc(pixelX - eyeOffsetX + pupilOffsetX, pixelY - eyeOffsetY + pupilOffsetY, pupilRadius, 0, Math.PI * 2);
    ctx.fill();
    ctx.beginPath();
    ctx.arc(pixelX + eyeOffsetX + pupilOffsetX, pixelY - eyeOffsetY + pupilOffsetY, pupilRadius, 0, Math.PI * 2);
    ctx.fill();

    // Smile
    const smileRadius = radius * gs.agent.size.smile_radius;
    const smileStartY = pixelY + radius / 5;

    ctx.fillStyle = 'white';
    ctx.beginPath();
    ctx.arc(pixelX, smileStartY, smileRadius, 0, Math.PI, false);
    ctx.lineTo(pixelX, smileStartY);
    ctx.closePath();
    ctx.fill();
  }

  controlAgentBrightness(start, stop, duration, frameRate = 5) {
    makeRangeArr(start, stop, duration / frameRate).forEach((value, index) => {
      setTimeout(() => {
        this.brightnessChange = value;
      }, index * frameRate);
    });
  }

  animateHoverEffect() {
    clearTimeout(this.hoverTimeout);
    clearTimeout(this.unhoverTimeout);
    const animationDuration = gs.agent.animations.hover.duration;

    this.inAnimation = true;

    this.controlAgentBrightness(0, gs.agent.colors[`${this.agentType}_hover`].l, animationDuration);

    this.hoverTimeout = setTimeout(() => {
      const startUnhover = setInterval(() => {
        if (!this.isHovered) {
          this.animateUnhoverEffect();
          clearInterval(startUnhover);
        }
      }, 25);
    }, animationDuration);
  }

  animateUnhoverEffect() {
    const animationDuration = gs.agent.animations.hover.duration;

    this.controlAgentBrightness(gs.agent.colors[`${this.agentType}_hover`].l, 0, animationDuration);

    this.unhoverTimeout = setTimeout(() => {
      this.inAnimation = false;
    }, animationDuration);
  }
}