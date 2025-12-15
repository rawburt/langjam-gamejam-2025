const PSIZE = 5;

class Engine {
  constructor(gameBuilder) {
    this.canvas = document.getElementById("canvas");
    this.ctx = canvas.getContext("2d");
    this.setupCtx();
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
      if (e.key === "ArrowLeft") this.input[0] = { state: true, repeat: e.repeat };
      if (e.key === "ArrowRight") this.input[1] = { state: true, repeat: e.repeat };
      if (e.key === "ArrowUp") this.input[2] = { state: true, repeat: e.repeat };
      if (e.key === "ArrowDown") this.input[3] = { state: true, repeat: e.repeat };
    });
    window.addEventListener("keyup", (e) => {
      if (e.key === "ArrowLeft") this.input[0] = { state: false, repeat: false };
      if (e.key === "ArrowRight") this.input[1] = { state: false, repeat: false };
      if (e.key === "ArrowUp") this.input[2] = { state: false, repeat: false };
      if (e.key === "ArrowDown") this.input[3] = { state: false, repeat: false };
    })
  }

  clearInput() {
    this.input = [
      { state: false, repeat: false },
      { state: false, repeat: false },
      { state: false, repeat: false },
      { state: false, repeat: false }
    ];
  }

  tick(timestamp) {
    this.updateFn();
    this.drawFn();
    this.clearInput();
    requestAnimationFrame(() => this.tick());
  }

  pset(x, y, color) {
    this.ctx.fillStyle = color;
    this.ctx.strokeStyle = color;
    this.ctx.fillRect(x * PSIZE, y * PSIZE, PSIZE, PSIZE);
  }

  button(id) {
    return this.input[id].state;
  }

  buttonp(id) {
    return this.input[id].state && !this.input[id].repeat;
  }

  clear() {
    this.ctx.clearRect(0, 0, this.canvas.width, this.canvas.height);
  }

  text(str, x, y, size, color) {
    this.ctx.strokeStyle = undefined;
    this.ctx.fillStyle = color;
    this.ctx.font = (size * PSIZE) + 'px GameFont';
    this.ctx.fillText(str, x * PSIZE, y * PSIZE);
  }

  debug(s) {
    console.log(s);
  }

  setupCtx() {
    this.ctx.imageSmoothingEnabled = false;
    this.ctx.mozImageSmoothingEnabled = false;
    this.ctx.webkitImageSmoothingEnabled = false;
    this.ctx.msImageSmoothingEnabled = false;
    this.ctx.textBaseline = 'top';
  }
}
