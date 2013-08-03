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
    new Circle(this.x, this.y, this.size)
        .addTo(stage)
        .attr({strokeColor: 'white',
               strokeWidth: STROKE_WIDTH})
};

function Spaceship(x, y, rot, id) {
    this.x = x;
    this.y = y;
    this.rot = rot;
    this.id = id;
};

Spaceship.prototype.draw = function() {
    new Polygon(this.x, this.y, SPACESHIP_RADIUS, SPACESHIP_SIDES)
        .addTo(stage)
        .attr({strokeColor: 'white',
               strokeWidth: STROKE_WIDTH,
               rotation: this.rot});
    // new Path()
    //     .addTo(stage)
    //     .attr({strokeColor: 'white',
    //            strokeWidth: STROKE_WIDTH});
};

function MySpaceship(x, y, rot, id) {
    Spaceship.call(this, x, y, rot, id);
}

MySpaceship.prototype = Object.create(Spaceship.prototype, {
    draw : {
        value : function() {
        new Polygon(this.x, this.y, SPACESHIP_RADIUS, SPACESHIP_SIDES)
            .addTo(stage)
            .attr({strokeColor: 'red',
                   strokeWidth: STROKE_WIDTH,
                   rotation: this.rot});
        },
        enumerable: true, 
        configurable: true, 
        writable: true 
    }
})

function Projectile(x, y) {
    this.x = x;
    this.y = y;
};

Projectile.prototype.draw = function() {
    new Circle(this.x, this.y, PROJECTILE_RADIUS)
        .addTo(stage)
        .attr('fillColor', 'white');
}

function Explosion(x, y) {
    this.x = x;
    this.y = y;
};

Explosion.prototype.draw = function() {
    new Star(this.x, this.y, STAR_RADIUS, STAR_RAYS)
        .addTo(stage)
        .attr({strokeColor: 'white',
               strokeWidth: STROKE_WIDTH});
};

function drawState(arr) {
    for (i = 0; i < arr.length; ++i) {
        arr[i].draw();
    }
};

bonsai.Path.rect(0, 0, 700, 1900).attr({fillColor:'black'}).addTo(stage);

var arr = [new Asteroid(200, 136, 50), new Spaceship(20, 20, Math.PI/180*35, 1), new Projectile(80, 80), new Explosion(100, 100), new MySpaceship(80, 80, Math.PI/180*35, 2)];
drawState(arr);
