const PSIZE = 4;

const Key = { Left: 0, Right: 1, Up: 2, Down: 3, Z: 4, X: 5 }

function len(e) {
  return e.length;
}

function push(x, xs) {
  return xs.push(x);
}

function pop(xs) {
  return xs.pop();
}

function list_delete(i, xs) {
  xs.splice(i, 1)
}

function rand(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

async function loadAndDecodeImages(images) {
  const promises = images.map(image => {
    return new Promise((resolve, reject) => {
      image.onload = () => {
        image.decode()
          .then(() => resolve(image))
          .catch(error => reject(new Error(`failed to decode image: ${error}`)));
      }
      image.onerror = () => {
        reject(new Error(`failed to load image: ${image.src}`))
      }
    });
  });
  await Promise.all(promises);
  return images;
}

class Engine {
  constructor() {
    this.images = [];
    this.canvas = document.getElementById("canvas");
    this.ctx = canvas.getContext("2d");
    this.setupCtx();
    this.setupInput();
  }

  async init(gameBuilder) {
    const game = gameBuilder(this);
    this.updateFn = game.update;
    this.drawFn = game.draw;
    await this.loadImages();
  }

  run() {
    requestAnimationFrame(() => this.tick());
  }

  preload(src) {
    const id = this.images.length;
    const image = new Image();
    image.src = src;
    this.images.push(image);
    return id;
  }

  async loadImages() {
    this.images = await loadAndDecodeImages(this.images);
  }

  setupInput() {
    this.clearInput();
    window.addEventListener("keydown", (e) => {
      if (e.key === "ArrowLeft") this.input[0] = { state: true, repeat: e.repeat };
      if (e.key === "ArrowRight") this.input[1] = { state: true, repeat: e.repeat };
      if (e.key === "ArrowUp") this.input[2] = { state: true, repeat: e.repeat };
      if (e.key === "ArrowDown") this.input[3] = { state: true, repeat: e.repeat };
      if (e.key === "z") this.input[4] = { state: true, repeat: e.repeat };
      if (e.key === "x") this.input[5] = { state: true, repeat: e.repeat };
    });
    window.addEventListener("keyup", (e) => {
      if (e.key === "ArrowLeft") this.input[0] = { state: false, repeat: false };
      if (e.key === "ArrowRight") this.input[1] = { state: false, repeat: false };
      if (e.key === "ArrowUp") this.input[2] = { state: false, repeat: false };
      if (e.key === "ArrowDown") this.input[3] = { state: false, repeat: false };
      if (e.key === "z") this.input[4] = { state: false, repeat: false };
      if (e.key === "x") this.input[5] = { state: false, repeat: false };
    })
  }

  clearInput() {
    this.input = [
      { state: false, repeat: false },
      { state: false, repeat: false },
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

  render(id, sx, sy, sw, sh, dx, dy, dw, dh) {
    const image = this.images[id];
    if (!image) {
      console.error("image not found: id=", id);
    } else {
      this.ctx.drawImage(image, sx, sy, sw, sh, dx * PSIZE, dy * PSIZE, dw * PSIZE, dh * PSIZE);
    }
  }

  renderOverlay(color, x, y, w, h) {
    this.ctx.fillStyle = color;
    this.ctx.globalCompositeOperation = 'source-atop';
    this.ctx.fillStyle = color;
    this.ctx.fillRect(x * PSIZE, y * PSIZE, w * PSIZE, h * PSIZE);
    this.ctx.globalCompositeOperation = 'source-over';
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
