// JavaScript source code

var connection = {};
var mySpaceship;
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
	console.log("polaczenie otwarte");
	var userName = document.getElementById("userName").value;
	var jsonMessage = {
		"msgType": "hello",
		"data": { "name": userName },
	}
	connection.send(JSON.stringify(jsonMessage));
	activateGame();
};

function onClose(evt) {
	console.log("koneic polaczenia");
};

function activateGame() {
	addEventListener('keydown', onKeyDown);
	addEventListener('keyup', onKeyUp);
}

function joinGame() {
	establishConnection();
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
			break;
		case "state":
			//console.log(data.data);
			parseGameObjects(data.data);
			break;
	}

};

function parseGameObjects(data) {
	var objects = new Array();
	if (data["asteroids"] != null) {
		for (var key in data["asteroids"]) {
			var temp = new Asteroid(data["asteroids"][key]["position"].x,
				data["asteroids"][key]["position"].y, data["asteroids"][key].radius)
			objects.push(temp);
		}
	}
	if (data["players"] != null) {
		for (var key in data["players"]) {
			if (key == mySpaceship) {
				var temp = new MySpaceship(data["players"][key]["position"].x,
					data["players"][key]["position"].y, data["players"][key].radius, data["players"][key].id);
				//console.log(data["players"][key].speed);
			} else {
				var temp = new Spaceship(data["players"][key]["position"].x,
					data["players"][key]["position"].y, data["players"][key].radius, data["players"][key].id);

			}
			objects.push(temp);
		}
	}
	if (data["projectiles"] != null) {
		for (var key in data["projectiles"]) {
			var temp = new Projectile(data["projectiles"][key]["position"].x, data["projectiles"][key]["position"].y);
			objects.push(temp);
		}
	}
	if (data["collisions"] != null) {
		for (var key in data["collisions"]) {
			var temp = new Projectile(data["collisions"][key]["position"].x, data["collisions"][key]["position"].y);
			objects.push(temp);
		}
	}
	drawState(objects);
	
}

function onKeyDown(evt) {
	evt.preventDefault();
	if (pressedKeys.indexOf(evt.keyCode) == -1) {
		pressedKeys.push(evt.keyCode);
		onKeyPress(evt, "down");
	}
	return false;
}

function onKeyUp(evt) {
	evt.preventDefault();
	var i = pressedKeys.indexOf(evt.keyCode);
	if (i > -1) {
		pressedKeys.splice(i, 1);
		onKeyPress(evt, "up");
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
			var snd = new Audio("media/bullet.mp3");
			snd.play();
			break;
	}
	
	if (msgType != undefined)
	{
		var json = {
			"msgType": msgType,
			"data": data
		}
		connection.send(JSON.stringify(json));
		//console.log(JSON.stringify(json));
	}

	
};

