$(document).ready(function() {
    var socket;

    if(!("WebSocket" in window)) {
        $('<p>Oh no, you need a browser that supports WebSockets.'+
          'How about <a href="http://www.getfirefox.com/">Mozilla Firefox</a>?'+
          '</p>').appendTo('#container');
    } else {
        //
        //The user has WebSockets
        //
        function connect(){
            var host = "ws://"+window.location.hostname+":"+
                window.location.port+"/epl_3d_EPL";
            try{
                socket = new WebSocket(host);
                message(socket.readyState + ' (WebSocekt new)');

	        socket.onopen = function(){
	            message(socket.readyState + ' (WebSocket open)');
	        }

	        socket.onmessage = function(msg){
                    d = JSON.parse(msg.data);

                    if(d.spawn  != undefined) {
                        d.spawn.forEach(function(item) {
                            VE.setNode(item.id, {size:3});
                        });

                        $('#spawn').text(d.spawn.length);
                    };

                    if(d.exit  != undefined) {
                        $('#exit').text(d.exit.length);
                    };

                    if(d.send  != undefined) {
                        d.send.forEach(function(item) {
                            VE.setNode(item.v1, {size:3});
                            VE.setNode(item.v2, {size:3});
                            VE.setEdge(item.v1, item.v2, { });
                        });
                        $('#send').text(d.send.length);
                    };

                    if(d.receive != undefined) {
                        d.receive.forEach(function(item) {
                            //VE.setNode(item.id, {size: Math.log(item.s) });
                        });
                        $('#receive').text(d.receive.length);
                    };

                    if(d.status != undefined) {
                        $('#process_info').html("<pre>"+
                                                JSON.stringify(d, undefined, 2)
                                                +"</pre>");
                    };
                }

	        socket.onclose = function(){
	            message(socket.readyState + ' (WebSocket closed)');
	        }

	    } catch(exception){
	        message(exception);
	    }

	    function message(msg){
	        $('#message').text(msg);
            }//End message()

        }//End connect()

    }//End else

    connect();

    VE.initialize('epl_3d', { transparency: true });
    VE.onNodeSelect = function onSelect(nodeId) {
        console.log(nodeId);
        socket.send(nodeId);
    }

});
