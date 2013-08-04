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
	var jsonMessage = {
		"msgType": "hello",
		"data": { "name": "Magda" },
	}
	connection.send(JSON.stringify(jsonMessage));
	activateGame();
};

function onClose(evt) {
	console.log("koneic polaczenia");
};

function activateGame() {
	document.getElementById("message").innerHTML = "Welcome to the game!";

	addEventListener('keydown', onKeyDown);

	/*
	document.onkeydown = onKeyDown;
	document.onkeyup = onKeyUp;

	
	window.addEventListener("keydown", function (e) {
		// space and arrow keys
		if (usedKeys.indexOf(e.keyCode) > -1) {
			e.preventDefault();
		}
	}, false);
	*/
}

function joinGame() {
	establishConnection();
}

function leaveGame() {
	connection.close();
	
	window.addEventListener("keydown", function (e) {
		// space and arrow keys
		if (usedKeys.indexOf(e.keyCode) > -1) {
			return false;
		}
	}, false);
}

function sendMessage() {
	establishConnection();
	var jsonMessage2 = {
		"msgType": "hello",
		"data": { "name": "Magda" },
	}
	console.log(connection);
	connection.send(jsonMessage2);
};

function onMessage(evt) {
	var data = JSON.parse(evt.data);
	switch (data.msgType) {
		case "helloReply":
			mySpaceship = data.data.id;	
			break;
		case "state":
			parseGameObjects(data.data);
			break;
	}

};

function parseGameObjects(data) {
	var objects = new Array();
	if (data["asteroids"] != null) {
		for (var key in data["asteroids"]) {
			var temp = new Asteroid(data["asteroids"][key].position.x,
				data["asteroids"][key].position.y, data["asteroids"][key].size)
			objects.push(temp);
		}
	}
	if (data["players"] != null) {
		for (var key in data["players"]) {
			if (key == mySpaceship) {
				var temp = new MySpaceship(data["players"][key].position.x,
					data["players"][key].position.y, data["players"][key].radius, data["players"][key].id);
			} else {
				var temp = new Spaceship(data["players"][key].position.x,
					data["players"][key].position.y, data["players"][key].radius, data["players"][key].id);

			}
			objects.push(temp);
		}
	}
	if (data["projectiles"] != null) {
		for (var key in data["projectiles"]) {
			var temp = new Projectile(data["projectiles"][key].position.x, data["projectiles"][key].position.y);
			objects.push(temp);
		}
	}
	if (data["collisions"] != null) {
		for (var key in data["collisions"]) {
			var temp = new Projectile(data["collisions"][key].position.x, data["collisions"][key].position.y);
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
	var i = pressedKeys.indexOf(evt.keyCode);
	if (i > -1) {
		pressedKeys.splice(i, 1);
		onKeyPress(evt, "up");
	}

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
			data["direction"] = "left";
			break;				
		case MOVE_UP:
			msgType = "accelerate";
			break;
	}
	
	if (msgType != undefined)
	{
		var json = {
			"msgType": msgType,
			"data": data
		}
		connection.send(JSON.stringify(json));
		console.log(JSON.stringify(json));
	}

	
};

function testMethod() {
	var json = {
		"msgType" : "gameState" ,
		"data" : {
			"asteroids" : [
				{ "x" : 10, "y" : 10 },
				{ "x" : 20, "y" : 20 }
			],
			"spaceships" : [
			],
			"projectiles" : [
			],
			"explosions" : [
			],

		}
	}
	var data = JSON.parse(JSON.stringify(json));

	parseGameObjects(data.data);
}