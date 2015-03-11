// peerChat.js
// A JavaScript file that exposes an interface between the peer.js library and
// the Chat.elm code that also embeds the elm output in the fullscreen of the
// page.
// This script should be included after the body of the html page.

// Authored by Mitch Spradlin
// 2015-02-22

//Have Elm take over the screen, exposing the fromJS port to be, when written
// a Signal that is updated in the elm code
//Note that this port is a String and nothing more
var peerData = Elm.fullscreen(Elm.Chat, {
    fromJS: ""
});

//Clobber our peer and set a new  from the Peer.js library
//This is 'us', and the ID we get is the one that others use to connect with us
// and is a unique identifier within our network
function makePeer(id) {
    //Clean up any previous instance
    if (typeof peer != "undefined") {
        peer.destroy();
    }
    //Set the peer to be new with the given id
    peer = new Peer(id, {
        debug: 1,
        logFunction: function(x) { console.log(x) },
        host: "aria.dnsalias.org",
        port: 9001,
        config: {'iceServers': [
            { url: 'stun:stun.l.google.com:19302' }
        ]}
    });
    //If that ID is already taken, report it
    if (typeof peer == "undefined") {
        toElm("err:" + id + ":taken");
    }
}

//A convenience function to send a string over to the Elm code
function toElm(info) {
    peerData.ports.fromJS.send(info);
}

//The global array of connections that we have established
//This is a dictionary indexed by connIDs
var connList = {};

//It seems like we need to kick an event to Elm in order for it to run at all,
// so we send this friendly note over
toElm("init");

//This function takes input from the fromElm port in the elm code and parses it
// to take the appropriate action
//Messages that can be passed follow the form Command:Args:Content and are
// are tokenized and the appropriate function is then called, the responses
// being passed through the fromJS port in a similar fashion
function parseInput(instr) {
    //For illustrative purposes during the demo
    toElm("From Elm -- " + instr);
    //Split into command parts
    var inArr = instr.split(':');
    //Decide what to do based on the first part
    //Note that we always get at least the empty string in the first value of
    // the return array
    switch (inArr[0]) {
        case 'makePeer':
            toElm("attemptingServerConn");
            if (inArr.length >= 2) {
                makePeer(inArr[1]);
                peer.on('open', function(id) {
                    toElm("myID:" + id);
                });
                peer.on('disconnected', function() {
                    toElm("lostConn");
                    peer.destroy();
                });
                //Setup the correct handlers for everything
                peer.on('connection', function(dataConnection) {
                    dataConnection.on('open', function() {
                        connect(dataConnection);
                    });
                });
            }
            break;
        case 'myID':
            if (typeof peer != "undefined") {
                toElm("myID:" + peer.id); 
            } else {
                report("", "Peer is not made yet");
            }
            break;
        case 'connect':
            if (inArr.length >= 2) {
                connectTo(inArr[1]);
            } else {
                report(peer.id, "No connectionID supplied");
            }
            break;
		case 'connectToExisting':
			connectToExisting();
			break;
        case 'send': //Send data to the given ID
            if (inArr.length >= 3) {
                if (inArr[1] === "all") {
                    for (var conn in connList) {
                        connList[conn].send(instr.substr(9));
                    }
                } else {
                    var conns = inArr[1].split(',');
                    for (var i = 0; i < conns.length; i++) {
                        if (typeof connList[conns[i]] != "undefined") {
                            connList[conns[i]].send(
                                instr.substr(5 + inArr[1].length + 1));
                        } else {
                            report(conns[i], "Not connected to requested ID");
                        }
                    }
                }
            } else {
                    report(peer.id, "USAGE: send:ID:Trace:Message");
            }
            toElm("clearQueue");
            break;
        case 'listConns': //List all of our current connections
            if (Object.keys(connList).length == 0) {
                toElm("No connections right now");
            } else {
                for (var id in connList) {
                    toElm("Connected to ID: " + id);
                }
            }   
            break;
        case 'getExisting': //Request and send back up the list of existing
                            // nodes
            getExisting();
            break;
        case 'setName': //See below
            if (typeof peer != "undefined" && inArr.length >= 2) {
                toElm("myName:" + inArr[1]);
            }
            break;
        case 'fwd': //Should rearchitect to avoid such things...
            toElm("clearQueue");
            parseInput(instr.slice(4));
            break;
        case 'destroySelf': //Destroy the peer
            toElm("lostConn");
            peer.destroy();
            break;
    }
}

//We subscribe to the fromElm port in the elm code, calling parseInput every
// time it is updated
peerData.ports.fromElm.subscribe(parseInput);

//Handles a connection object, registering the appropriate functions to call
// when an incoming connection is recieved
function connect(c) {
    //If we're already connected to them, bounce the connection
    if (typeof connList[c.peer] != "undefined") {
        return;
    }
    //Let elm know that we have a new person connected to us
    toElm("connectedTo:" + c.peer);
    //Add this connection to the rolling list of connections
    connList[c.peer] = c;
    //Register the appropriate actions to take upon various things that happen
    // with the data connection
    c.on('close', function() {
        terminate(c.peer);
    });
    c.on('data', function(data) {
        recieve(c.peer, data);
    });
    c.on('error', function() {
        c.close();
        report(c.peer, err);
    });
}

//Connects to a given id
function connectTo(id) {
    //Our new connection object
    var c = peer.connect(id, {
        reliable: true
    });
    //Let elm know what's up
    toElm("attempting:" + id);
    //If the connection goes through, add it to our list and let elm know
    c.on('open', function() {
        connect(c);
    });
}

//Requests and returns a list of IDs that exist
//This list can be considered current only immediately after retrieval
function getExisting() {
    //Make an xmlhttp object
    xmlhttp = new XMLHttpRequest();
    xmlhttp.open("GET", "http://aria.dnsalias.org:9001/", true);
    
    //Add an event listener to send the information to elm once it's recieved
    xmlhttp.addEventListener('load', function() {
        if (xmlhttp.status === 200) {
            //Just send the returned string to Elm - it will parse it
            toElm('existing:' + xmlhttp.responseText);
        }
    }, false);

    //Send the request
    xmlhttp.send();
}

//Connects to an ID that is deduced from a list of IDs presented at a URL
//Have tweaked the peerServer to just serve a JSON array of IDs
function connectToExisting() {
    //Make an xmlhttp object
    xmlhttp = new XMLHttpRequest();
    xmlhttp.open("GET", "http://aria.dnsalias.org:9001/", true);

    //Add an event listener to perform the parsing and connection when it gets in
    xmlhttp.addEventListener('load', function() {
        if (xmlhttp.status === 200){
            //Send it to a function that will perform the 
	    	connectToNew(JSON.parse(xmlhttp.responseText));
        }
    }, false);

    //Send the request
    xmlhttp.send();
}

//Given an array of IDs, connects to one that we aren't already connected to
function connectToNew(idArr) {
	//First, go through and eliminate any IDs that we are already connected to
	//This also includes ourselves
	connArray = Object.keys(connList);
	for (var i = 0; i < idArr.length; i++) {
		if (idArr[i] === peer.id) {
			idArr.splice(i,1);
			i--;
			continue;
		}
		for (var j = 0; j < connArray.length; j++) {
			if (idArr[i] === connArray[j]) {
				idArr.splice(i,1);
				i--;
			}
		}
	}

	//Now perform the connection, if there is someone to connect to
	if (idArr.length == 0) {
		toElm("Connected to everyone!");
	} else {
		//Pick a random one
        var choice = Math.round(Math.random() * (idArr.length - 1));
		connectTo(idArr[choice]);
	}
}
	

//Terminates a connection, cleaning up appropriately
function terminate(id) {
    delete connList[id];
    toElm("disconnectedFrom:" + id);
}

//Takes information recieved over a connection and sends it to Elm appropriately
function recieve(id, data) {
    toElm("data:" + id + ":" + data.toString());
}

//Reports an error to Elm
function report(id, err) {
    toElm("err:" + id + ":" + err);
}

//If there is a fatal error, reload the page
window.onerror = function reload() {
    location.reload();
}
