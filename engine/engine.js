const PSIZE = 5;

class Engine {
  constructor(gameBuilder) {
    this.canvas = document.getElementById("canvas");
    this.ctx = canvas.getContext("2d");
    const game = gameBuilder(this);
    this.updateFn = game.update;
    this.drawFn = game.draw;
    this.setupInput();
  }

  run() {
    requestAnimationFrame(() => this.tick());
  }

  setupInput() {
    this.clearInput();
    window.addEventListener("keydown", (e) => {
      if (e.key === "ArrowLeft") this.input[0] = true;
      if (e.key === "ArrowRight") this.input[1] = true;
      if (e.key === "ArrowUp") this.input[2] = true;
      if (e.key === "ArrowDown") this.input[3] = true;
    });
    window.addEventListener("keyup", (e) => {
      if (e.key === "ArrowLeft") this.input[0] = false;
      if (e.key === "ArrowRight") this.input[1] = false;
      if (e.key === "ArrowUp") this.input[2] = false;
      if (e.key === "ArrowDown") this.input[3] = false;
    })
  }

  clearInput() {
    this.input = [false, false, false, false];
  }

  tick(timestamp) {
    this.updateFn();
    this.drawFn();
    requestAnimationFrame(() => this.tick());
  }

  pset(x, y, color) {
    this.ctx.fillStyle = color;
    this.ctx.strokeStyle = color;
    this.ctx.fillRect(x * PSIZE, y * PSIZE, PSIZE, PSIZE);
  }

  button(id) {
    return this.input[id];
  }
}
