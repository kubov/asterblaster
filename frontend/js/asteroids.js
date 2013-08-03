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

Asteroid.prototype.draw = function() {

};

function Spaceship(x, y, rot, id) {
    this.x = x;
    this.y = y;
    this.rot = rot;
    this.id = id;
};

Spaceship.prototype.draw = function() {
    var ctx = document.getElementById('mycanvas');

    ctx.beginPath();

    ctx.moveTo(-5, -5);
    ctx.lineTo(0, 10);
    ctx.lineTo(20, -5);
    ctx.lineTo(-20, -5);
    ctx.strokeStyle = 'black';
    ctx.closePath();
    ctx.stroke();
    ctx.save();
};

function MySpaceship(x, y, rot, id) {
    Spaceship.call(this, x, y, rot, id);
}

MySpaceship.prototype = Object.create(Spaceship.prototype, {
    draw : {
        value : function() {

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

Projectile.prototype.draw = function() {

}

function Explosion(x, y) {
    this.x = x;
    this.y = y;
};

Explosion.prototype.draw = function() {

};

function drawState(arr) {
    for (i = 0; i < arr.length; ++i) {
        arr[i].draw();
    }
};

var arr = [new Asteroid(200, 136, 50), new Spaceship(80, 80, Math.PI/180*35, 1), new Projectile(80, 80), new Explosion(100, 100), new MySpaceship(80, 80, Math.PI/180*35, 2)];
drawState(arr);
