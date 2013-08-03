var SPACESHIP_RADIUS = 15;
var SPACESHIP_SIDES = 3;
var PROJECTILE_RADIUS = 2;
var STROKE_WIDTH = 2;
var STAR_RADIUS = 20;
var STAR_RAYS = 7;

function Asteroid(x, y, size) {
    this.x = x;
    this.y = y;
    this.size = size;
};

Asteroid.prototype.draw = function(ctx) {

};

function Spaceship(x, y, rot, id) {
    this.x = x;
    this.y = y;
    this.rot = rot;
    this.id = id;
};

Spaceship.prototype.draw = function(ctx) {
    ctx.beginPath();
    ctx.translate(this.x, this.y);
    ctx.moveTo(-10, -10);
    ctx.lineTo(0, 20);
    ctx.lineTo(10, -10);
    ctx.closePath();
    ctx.lineWidth = STROKE_WIDTH;
    ctx.strokeStyle = 'black';
    ctx.stroke();
    ctx.save();
};

function MySpaceship(x, y, rot, id) {
    Spaceship.call(this, x, y, rot, id);
}

MySpaceship.prototype = Object.create(Spaceship.prototype, {
    draw : {
        value : function(ctx) {
            ctx.beginPath();
            ctx.translate(this.x, this.y);
            ctx.moveTo(-10, -10);
            ctx.lineTo(0, 20);
            ctx.lineTo(10, -10);
            ctx.closePath();
            ctx.strokeStyle = 'black';
            ctx.stroke();
            ctx.save();
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

}

function Explosion(x, y) {
    this.x = x;
    this.y = y;
};

Explosion.prototype.draw = function(ctx) {

};

function drawState(arr) {
    var ctx = document.getElementById('mycanvas').getContext('2d');
    for (i = 0; i < arr.length; ++i) {
        arr[i].draw(ctx);
    }
};

var arr = [new Asteroid(200, 136, 50), new Spaceship(80, 80, Math.PI/180*35, 1), new Projectile(80, 80), new Explosion(100, 100), new MySpaceship(80, 80, Math.PI/180*35, 2)];
drawState(arr);
