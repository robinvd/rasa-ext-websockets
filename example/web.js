var socket;

window.onload = function() {

  // functions:


  // Create a new WebSocket.
  socket = new WebSocket('ws://localhost:3000');

  // Show a connected message when the WebSocket is opened.
  socket.onopen = function(event) {
    console.log('connection open');
  };

  // Handle any errors that occur.
  socket.onerror = function(error) {
    console.log('WebSocket Error: ' + error);
  };

  // Send a message when the form is submitted.
  function send(message) {

    // Send the message through the WebSocket.
    socket.send(message);

    // Add the message to the messages list.
    console.log("send: " + message);

    return false;
  };

  // Handle messages sent by the server.
  socket.onmessage = function(event) {
    console.log(event);
    document.getElementById("text").innerHTML = event.data;

  };

  // Show a disconnected message when the WebSocket is closed.
  socket.onclose = function(event) {
    console.log('Disconnected from WebSocket.');
  };

  // Close the WebSocket connection when the close button is clicked.
  function close() {
    // Close the WebSocket.
    socket.close();

    return false;
  };


};
