var SPACESHIP_RADIUS = 15;
var SPACESHIP_SIDES = 3;
var PROJECTILE_RADIUS = 3;
var STROKE_WIDTH = 2;
var STAR_RADIUS = 20;
var STAR_RAYS = 7;

function Asteroid(x, y, size) {
    this.x = x;
    this.y = y;
    this.size = size;
};

Asteroid.prototype.draw = function(ctx) {
    ctx.beginPath();
    ctx.arc(this.x, this.y, this.size, 0, 2 * Math.PI, false);
    ctx.closePath();
    ctx.lineWidth = STROKE_WIDTH;
    ctx.strokeStyle = 'black';
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
    ctx.rotate(this.rot);
    ctx.moveTo(-10, -10);
    ctx.lineTo(0, 20);
    ctx.lineTo(10, -10);
    ctx.closePath();
}

Spaceship.prototype.draw = function(ctx) {
    this.spaceship_contour(ctx);
    ctx.lineWidth = STROKE_WIDTH;
    ctx.strokeStyle = 'black';
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
    ctx.fillStyle = 'black';
    ctx.fill();
}

function Explosion(x, y) {
    this.x = x;
    this.y = y;
};

Explosion.prototype.draw = function(ctx) {
    ctx.translate(this.x, this.y);
    ctx.beginPath();
    ctx.moveTo(0, 2);
    ctx.lineTo(-2, 6);
    ctx.lineTo(-4, 2);
    ctx.lineTo(-8, 0);
    ctx.lineTo(-4, -2);
    ctx.lineTo(-6, -6);
    ctx.lineTo(0, -2);
    ctx.lineTo(2, -6);
    ctx.lineTo(4, -2);
    ctx.lineTo(6, -2);
    ctx.lineTo(4, 0);
    ctx.lineTo(6, 2);
    // ctx.arc(this.x, this.y, 20, 0, 2 * Math.PI, false);
    ctx.closePath();
    ctx.fillStyle = 'red';
    ctx.fill();
};

function drawState(arr) {
    var ctx = document.getElementById('mycanvas').getContext('2d');
    for (i = 0; i < arr.length; ++i) {
        ctx.save();
        arr[i].draw(ctx);
        ctx.restore();
    }
};

var arr1 = [new Asteroid(200, 136, 50), new Spaceship(180, 180, Math.PI/180*35, 1), new Projectile(80, 80), new Explosion(100, 100), new MySpaceship(80, 80, Math.PI/180*35, 2)];

var arr = [new Asteroid(100, 100, 50), new Asteroid(100, 100, 50)];
arr = [new MySpaceship(100, 100, Math.PI/180*35, 1), new MySpaceship(100, 100, Math.PI/180*35, 2)];

drawState(arr1);
