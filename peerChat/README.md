# peerChat
A decentralized chat client written in Elm and JavaScript. This project was done
as an exercise and because I thought it would be cool, and as such is a bit
rough around the edges.

It uses the wonderful [Peer.js library](peerjs.com) to handle the nitty-gritty
of creating and sending data over WebRTC connections, a simple bit of JavaScript
to expose these methods to Elm, and then Elm handles everything else. In order
to run, this currently requires a slightly modified 
[PeerServer](https://github.com/peers/peerjs-server), so it won't work out of
the box if you're looking to host an instance yourself. However, if that's what
you're looking to do, it wouldn't be difficult to set it up.

I'm hosting an implementation at [peerchat.us](http://peerchat.us) if you'd like
to see what it looks like. If you're the only one connected, I recommend opening
the page in at least one other tab so that it's not so lonely.
