'use strict';

// module Network.WebSockets.Sync.Socket

// TODO: how to deal with timeouts??? timeout 3 sec -> try next server?
// TODO: try another node after n failed reconnects
// TODO: implement ConnectToAnother node
// TODO: drop connection after x secs of inactivity to relieve server
// TODO: resend unsent sync messages on connect
// TODO: subscribe to subscribed threads on connect
// TODO: reconnect to previous node if node doesn't accept connections after handoff

function timeout(id, ms, cb) {
  var self = timeout;
  self.timeouts = self.timeouts || {};

  if (self.timeouts[id])
    clearTimeout(self.timeouts[id]);

  self.timeouts[id] = setTimeout(function() {
    delete self.timeouts[id];
    cb();
  }, ms);
}

// http://stackoverflow.com/a/1349426/634020
function makeid()
{
  var text = "";
  var possible = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghipqrstuvwxyz0123456789";

  for( var i=0; i < 16; i++ )
    text += possible.charAt(Math.floor(Math.random() * possible.length));

  return text;
}

function _send(si, msg) {
  if (si.socket && si.socket.readyState === 1) {
    if (si.debug_log) console.log("Sending ++ " + msg);

    si.socket.send(msg);
  }
  else {
    if (si.debug_log) console.log("Backlogging ++ " + msg);

    si.requests = si.requests || [];
    si.requests.push(msg);

    if (!si.keep_alive) {
      establishConnection(si);
    }

    return false;
  }

  return true;
}

function establishConnection(si) {
  if (si.debug_log) console.log("Connecting to " + si.uri + "...");

  si.socket = new WebSocket(si.uri);

  /*
  window.addEventListener("beforeunload", function() {
    si.socket.close();
  });
  */

  si.socket.onopen = function() {
    if (si.handlers.connected != null) {
      var r = si.handlers.connected(si);

      // PureScript returns thunk
      if (typeof r === "function")
        r();
    }

    // send all outstanding requests
    var requests = si.requests || [];

    // send until all requests are sent or there's an error again
    while (requests.length > 0 && _send(si, requests.shift()))
      ;
  }

  si.socket.onmessage = function(msg) {
    var data = JSON.parse(msg.data);

    if (si.debug_log) console.log(data);

    // close connection after response
    if (!si.keep_alive) {
      si.socket.close();
      si.socket = null;
    }

    if (data && "sync-response" === data.cmd) {
      if (data.rid) {
        if (si.sync_requests[data.rid]) {
          var r = si.sync_requests[data.rid](JSON.stringify(data.response));

          // PureScript returns thunk
          if (typeof r === "function")
            r();

        }
        else if (data.error) {
          console.error(data.error);
        }

        delete si.sync_requests[data.rid];
      }
    }
    else if (data && "async-message" === data.cmd) {
      if (data.message != null)
        var r = si.handlers.message(JSON.stringify(data.message));

        // PureScript returns thunk
        if (typeof r === "function")
          r();
    }
  }

  si.socket.onerror = function(error) {
    console.error("Socket error (" + si.uri + "): " + error);

    if (si.handlers.disconnected != null) si.handlers.disconnected();

    if (si.keep_alive)
      timeout(si.uri, 3000, function() {exports.connectImpl(si.uri, si.keep_alive, si.handlers, si)();});
  }

  si.socket.onclose = function() {
    if (si.debug_log) console.log("Closing socket to " + si.uri + "...");

    if (si.handlers.disconnected != null) si.handlers.disconnected();

    if (si.keep_alive)
      timeout(si.uri, 3000, function() {exports.connectImpl(si.uri, si.keep_alive, si.handlers, si)();});
  }
}

exports.sendImpl = function(si, msg) {
  return function() {
    _send(si, JSON.stringify({
      cmd: "async-request",
      request: JSON.parse(msg)
    }));
    return {};
  };
};

exports.sendSyncImpl = function(si, msg, callback) {
  return function() {
    var rid = makeid();

    si.sync_requests[rid] = callback;

    _send(si, JSON.stringify({
      cmd: "sync-request",
      rid: rid,
      request: JSON.parse(msg)
    }));
    return {};
  };
};

exports.setHandlersImpl = function(si, handlers) {
  return function() {
    si.handlers = handlers;
  };
};

exports.killConnection = function(si_victim) {
  si_victim.socket.onclose = undefined;
  si_victim.socket.onerror = undefined;
  si_victim.socket.onmessage = undefined;
  si_victim.socket.onopen = undefined;

  si_victim.socket.close();
};

exports.connectImpl = function(uri, keep_alive, handlers, si_old, debug_log) {
  return function() {
    if (si_old && si_old.socket) {
      exports.killConnection(si_old);
    }

    var si = si_old || {
      uri: uri,
      sync_requests: {},
      handlers: handlers,
      keep_alive: keep_alive,
      debug_log: debug_log,
      requests: []
    };

    if (keep_alive) {
      establishConnection(si);
    }

    return si;
  };
};
