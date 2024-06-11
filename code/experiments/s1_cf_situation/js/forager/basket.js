/**
 * Class representing a Basket in the Gridworld.
 * @param {string} canvasId - The ID of the canvas element where the basket is rendered.
 * 
 * @property {Matter.Engine} engine - The Matter.js engine.
 * @property {Matter.World} world - The Matter.js world.
 * @property {Matter.Render} render - The Matter.js renderer.
 * @property {Matter.Body} basket - The compound body representing the basket.
 * @property {number} berriesCollected - The number of berries collected in the basket.
 */
class Basket {
  constructor(canvasId) {
    this.engine = Matter.Engine.create();
    this.world = this.engine.world;
    this.render = Matter.Render.create({
      canvas: document.getElementById(canvasId),
      engine: this.engine,
      options: {
        width: gs.basket.canvas_width,
        height: gs.basket.canvas_height,
        wireframes: false,
        background: 'transparent'
      }
    });

    // Define the parts of the basket
    const bottom = Matter.Bodies.rectangle(
      gs.basket.position.x,
      gs.basket.position.y,
      gs.basket.size.bottom_width,
      gs.basket.size.thickness, { isStatic: true }
    );
    const leftWall = Matter.Bodies.rectangle(
      gs.basket.position.x - gs.basket.position.wall_offset_x,
      gs.basket.canvas_width / 2,
      gs.basket.size.thickness,
      gs.basket.size.wall_height, {
        isStatic: true,
        angle: -Math.PI / 12
      }
    );
    const rightWall = Matter.Bodies.rectangle(
      gs.basket.position.x + gs.basket.position.wall_offset_x,
      gs.basket.canvas_width / 2,
      gs.basket.size.thickness,
      gs.basket.size.wall_height, {
        isStatic: true,
        angle: Math.PI / 12
      }
    );

    Matter.Body.setPosition(leftWall, {
      x: leftWall.position.x,
      y: leftWall.position.y + gs.basket.position.wall_offset_y
    });
    Matter.Body.setPosition(rightWall, {
      x: rightWall.position.x,
      y: rightWall.position.y + gs.basket.position.wall_offset_y
    });

    this.basket = Matter.Body.create({
      parts: [bottom, leftWall, rightWall],
      isStatic: true
    });

    Matter.World.add(this.world, this.basket);

    Matter.Runner.run(this.engine);
    Matter.Render.run(this.render);

    this.berriesCollected = 0;
    this.updateCounter();
  }

  updateCounter() {
    const counterElement = document.getElementById('berriesCounter');
    counterElement.innerHTML = `Berries: ${this.berriesCollected}<br\>${this.berriesNeeded ? "Needed: " + this.berriesNeeded : ""}`;
  }

  showBerryAddedAnimation(n) {
    const animationElement = document.createElement('div');
    animationElement.innerText = `+${n}`;
    animationElement.style.position = 'absolute';
    animationElement.style.top = '200px';
    animationElement.style.left = '72.5%';
    animationElement.style.transform = 'translateX(-50%)';
    animationElement.style.fontSize = '26px';
    animationElement.style.color = '#C00000';
    animationElement.style.opacity = '1';
    animationElement.style.transition = 'opacity 2s, top 2s';

    document.getElementById('basketContainer').appendChild(animationElement);

    setTimeout(() => {
      animationElement.style.top = '170px';
      animationElement.style.opacity = '0';
    }, 100);

    setTimeout(() => {
      animationElement.remove();
    }, 2100);
  }

  addBerries(n) {
    for (let i = 0; i < n; i++) {
      const xPosition = gs.basket.berry.x + (Math.random() - 0.5) * gs.basket.berry.x_spread;
      const yPosition = gs.basket.berry.y + (Math.random() - 0.5) * gs.basket.berry.y_spread;
      const berrySize = gs.basket.berry.size_min + Math.random() * (gs.basket.berry.size_max - gs.basket.berry.size_min);

      ((x, y, size) => {
        setTimeout(() => {
          const berry = Matter.Bodies.circle(x, y, size, {
            restitution: gs.basket.berry.restitution,
            density: gs.basket.berry.density,
            render: { fillStyle: '#C00000' }
          });
          Matter.World.add(this.world, berry);
        }, i * 25);
      })(xPosition, yPosition, berrySize);
    }

    this.berriesCollected += n;
    this.updateCounter();
    this.showBerryAddedAnimation(n);
  }

  destroy() {
    Matter.World.clear(this.world);
    Matter.Engine.clear(this.engine);
    Matter.Render.stop(this.render);

    const canvas = document.getElementById(this.render.canvas.id);
    if (canvas) {
      canvas.remove();
    }

    this.engine = null;
    this.world = null;
    this.render = null;
    this.basket = null;
  }
}