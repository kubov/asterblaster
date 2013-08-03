var SPACESHIP_RADIUS = 15;
var SPACESHIP_SIDES = 3;
var PROJECTILE_RADIUS = 2;

function Asteroid(x, y, size) {
    this.x = x;
    this.y = y;
    this.size = size;
};

Asteroid.prototype.draw = function() {
    new Circle(this.x, this.y, this.size)
        .addTo(stage)
        .attr('fillColor', 'white')
};

function Spaceship(x, y) {
    this.x = x;
    this.y = y;
};

Spaceship.prototype.draw = function() {
    new Polygon(this.x, this.y, SPACESHIP_RADIUS, SPACESHIP_SIDES)
        .addTo(stage)
        .attr('fillColor', 'white');
};

function Projectile(x, y) {
    this.x = x;
    this.y = y;
};

Projectile.prototype.draw = function() {
    new Circle(this.x, this.y, PROJECTILE_RADIUS)
        .addTo(stage)
        .attr('fillColor', 'white');
}

function drawState(arr) {
    for (i = 0; i < 3; ++i) {
        arr[i].draw();
    }
};

bonsai.Path.rect(0, 0, 800, 1900).attr({fillColor:'black'}).addTo(stage);

var arr = [new Asteroid(200, 136, 50), new Spaceship(20, 20), new Projectile(80, 80)];
drawState(arr);
