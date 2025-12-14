const PSIZE = 5;

class Engine {
  constructor(gameBuilder) {
    this.canvas = document.getElementById("canvas");
    this.ctx = canvas.getContext("2d");
    gameBuilder(this);
  }

  pset(x, y, color) {
    this.ctx.fillStyle = color;
    this.ctx.strokeStyle = color;
    this.ctx.fillRect(x * PSIZE, y * PSIZE, PSIZE, PSIZE);
  }
}
