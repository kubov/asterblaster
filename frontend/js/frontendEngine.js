// JavaScript source code

var connection = {};
var mySpaceship;
var amIAlive;
var usedKeys = [MOVE_UP, MOVE_DOWN, MOVE_LEFT, MOVE_RIGHT, SHOT];
var pressedKeys = new Array();

function establishConnection() {
	if (window.MozWebSocket) {
		window.WebSocket = window.MozWebSocket;
	}
	if (window.WebSocket != undefined) {
		if (connection.readyState === undefined || connection.readyState > 1) {
			connection = new WebSocket(SERVER_URL);
			connection.onopen = onOpen;
			connection.onclose = onClose;
			connection.onmessage = onMessage;
		}
	}
	else {
		alert("Your browser is not supporting WebSockets");
	}
};

function onOpen(evt) {
	var userName = document.getElementById("userName").value;
	var jsonMessage = {
		"msgType": "hello",
		"data": { "name": userName },
	}
	connection.send(JSON.stringify(jsonMessage));
	activateGame();
};

function onClose(evt) {
};

function activateGame() {
	addEventListener('keydown', onKeyDown);
	addEventListener('keyup', onKeyUp);
}

function joinGame() {
	establishConnection();
	window.onunload = leaveGame;
}

function leaveGame() {
	connection.close();
	addEventListener("keydown", function (e) { return true; });
	addEventListener("keyup", function (e) { return true; });
}


function onMessage(evt) {
	var data = JSON.parse(evt.data);
	switch (data.msgType) {
		case "helloReply":
			mySpaceship = data.data.id;
			amIAlive = true;
			break;
		case "state":
			console.log(data.data);
			parseGameObjects(data.data);
			break;
	}

};

function parseGameObjects(data) {
	var objects = new Array();
	var results = new Array();
	if (data["asteroids"] != null) {
		for (var key in data["asteroids"]) {
			if (data["asteroids"][key]["alive?"] != null) {
				var temp = new Asteroid(data["asteroids"][key]["position"].x,
					data["asteroids"][key]["position"].y, data["asteroids"][key].radius)
				objects.push(temp);
			}
		}
	}
	if (data["players"] != null) {
		var multiply = 2 * Math.PI * (1 / ROTATION_ANGLE);
		for (var key in data["players"]) {
			var r = new Result(
				data["players"][key].name, data["players"][key].score);
			results.push(r);
			if (key == mySpaceship) {
				if (data["players"][key]["alive?"] != null) {
					var temp = new MySpaceship(data["players"][key]["position"].x,
						data["players"][key]["position"].y, data["players"][key].k * multiply,
						data["players"][key].id, data["players"][key].name);
					objects.push(temp);
				} else {
					amIAlive = false;
				}
			} else {
				if (data["players"][key]["alive?"] != null) {
					var temp = new Spaceship(data["players"][key]["position"].x,
						data["players"][key]["position"].y, data["players"][key].k * multiply,
						data["players"][key].id, data["players"][key].name);
				}
				objects.push(temp);
			}
		}
	}
	if (data["projectiles"] != null) {
		for (var key in data["projectiles"]) {
			if (data["projectiles"][key]["alive?"] != null) {
				var temp = new Projectile(data["projectiles"][key]["position"].x, data["projectiles"][key]["position"].y);
				objects.push(temp);
			}
		}
	}
	if (data["collisions"] != null) {
		for (var key in data["collisions"]) {
			var temp = new Projectile(data["collisions"][key]["position"].x, data["collisions"][key]["position"].y);
			objects.push(temp);
		}
	}
	drawState(objects);
	showResults(results);

}

function onKeyDown(evt) {

	if (usedKeys.indexOf(evt.keyCode) > -1) {
		evt.preventDefault();
	}
	if (evt.keyCode == SHOT) {
		if (amIAlive) {
			var snd = new Audio("media/bullet.mp3");  
			snd.play();
		}
	}
	if (pressedKeys.indexOf(evt.keyCode) == -1) {
		pressedKeys.push(evt.keyCode);
		onKeyPress(evt, "down");
	}
	return false;
}

function onKeyUp(evt) {
	if (amIAlive) {
		if (usedKeys.indexOf(evt.keyCode) > -1) {
			evt.preventDefault();
		}
		var i = pressedKeys.indexOf(evt.keyCode);
		if (i > -1) {
			pressedKeys.splice(i, 1);
			onKeyPress(evt, "up");
		}
	}
	return false;
}

function onKeyPress(evt, status) {

	var msgType;
	var data = {
		"status": status
	};
	switch (evt.keyCode) {
		case MOVE_LEFT:
			msgType = "rotate";
			data["direction"] = "left";
			break;
		case MOVE_RIGHT:
			msgType = "rotate";
			data["direction"] = "right";
			break;
		case MOVE_UP:
			msgType = "accelerate";
			break;
		case SHOT:
			msgType = "shot";
			break;
	}

	if (msgType != undefined) {
		var json = {
			"msgType": msgType,
			"data": data
		}
		connection.send(JSON.stringify(json));
		//console.log(JSON.stringify(json));
	}


};

