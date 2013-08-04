var PROJECTILE_RADIUS = 3;
var STROKE_WIDTH = 2;
var WIDTH = 600;
var HEIGHT = 600;

function Asteroid(x, y, size) {
    this.x = x;
    this.y = y;
    this.size = size;
};

Asteroid.prototype.draw = function(ctx) {
    var k = this.size / 4;
    ctx.beginPath();
    ctx.translate(this.x, this.y);
    ctx.moveTo(0 * k, 4 * k);
    ctx.lineTo(-4 * k, 0 * k);
    ctx.lineTo(-2 * k, -3 * k);
    ctx.lineTo(-2 * k, -5 * k);
    ctx.lineTo(-1 * k, -3 * k);
    ctx.lineTo(3 * k, -3 * k);
    ctx.lineTo(3 * k, 1 * k);
    ctx.lineTo(1 * k, 2 * k);
    ctx.closePath();
    ctx.lineWidth = STROKE_WIDTH;
    ctx.strokeStyle = 'white';
    ctx.stroke();
};

function Spaceship(x, y, rot, id) {
    this.x = x;
    this.y = y;
    this.rot = rot;
    this.id = id;
};

Spaceship.prototype.spaceship_contour = function(ctx) {
    ctx.beginPath();
    ctx.translate(this.x, this.y);
    ctx.rotate(- Math.PI / 2);
    ctx.rotate(this.rot);
    ctx.moveTo(-10, -10);
    ctx.lineTo(0, 20);
    ctx.lineTo(10, -10);
    ctx.closePath();
}

Spaceship.prototype.draw = function(ctx) {
    this.spaceship_contour(ctx);
    ctx.lineWidth = STROKE_WIDTH;
    ctx.strokeStyle = 'white';
    ctx.stroke();
};

function MySpaceship(x, y, rot, id) {
    Spaceship.call(this, x, y, rot, id);
}

MySpaceship.prototype = Object.create(Spaceship.prototype, {
    draw : {
        value : function(ctx) {
            this.spaceship_contour(ctx);
            ctx.lineWidth = STROKE_WIDTH;
            ctx.strokeStyle = 'red';
            ctx.stroke();
        },
        enumerable: true, 
        configurable: true, 
        writable: true 
    }
});

function Projectile(x, y) {
    this.x = x;
    this.y = y;
};

Projectile.prototype.draw = function(ctx) {
    ctx.beginPath();
    ctx.arc(this.x, this.y, PROJECTILE_RADIUS, 0, 2 * Math.PI, false);
    ctx.closePath();
    ctx.fillStyle = 'white';
    ctx.fill();
}

function Explosion(x, y) {
    this.x = x;
    this.y = y;
};

Explosion.prototype.draw = function(ctx) {
    ctx.translate(this.x, this.y);
    ctx.beginPath();
    ctx.moveTo(0, 6);
    ctx.lineTo(-6, 18);
    ctx.lineTo(-12, 6);
    ctx.lineTo(-24, 0);
    ctx.lineTo(-12, -6);
    ctx.lineTo(-18, -18);
    ctx.lineTo(0, -6);
    ctx.lineTo(6, -18);
    ctx.lineTo(12, -6);
    ctx.lineTo(18, -6);
    ctx.lineTo(12, 0);
    ctx.lineTo(18, 6);
    ctx.closePath();
    ctx.fillStyle = 'red';
    ctx.fill();
};

function rect(x,y,w,h, ctx) {
  ctx.beginPath();
  ctx.rect(x,y,w,h);
  ctx.closePath();
  ctx.fill();
};

function clear(ctx) {
  ctx.clearRect(0, 0, WIDTH, HEIGHT);
  rect(0,0,WIDTH,HEIGHT, ctx);
};

function drawState(arr) {
	if (arr != null) {
		var ctx = document.getElementById('mycanvas').getContext('2d');
		ctx.fillStyle = 'black'
		clear(ctx);
		for (i = 0; i < arr.length; ++i) {
			ctx.save();
			arr[i].draw(ctx);
			ctx.restore();
		}
	}
};

function Welcome() {
    this.text = "Asterblaster";
};

Welcome.prototype.draw = function(ctx) {
    ctx.fillStyle = 'white';
    ctx.font = '41pt Arial';
    ctx.textAlign = 'center';
    ctx.textBaseline = 'bottom';
    ctx.fillText(this.text, WIDTH / 2, HEIGHT - 100);

};

var bullet_sound = function() {
    var snd = new Audio("media/bullet.mp3");
    snd.play();
};

// var arr1 = [new Asteroid(200, 136, 50), new Spaceship(180, 180, Math.PI/180*35, 1), new Projectile(80, 80), new Explosion(100, 100), new MySpaceship(80, 80, Math.PI/180*35, 2), new Welcome()];

var arr1 = [new Asteroid(200, 136, 50), new Asteroid(400, 200, 30), new Spaceship(100, 350, 0, 1), new Spaceship(200, 350, 0, 2), new Spaceship(300, 350, 0, 3), new MySpaceship(400, 350, 0, 1), new Spaceship(500, 350, 0, 1), new Welcome()];

drawState(arr1);
