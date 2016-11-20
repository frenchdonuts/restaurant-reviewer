// pull in desired CSS/SASS files
require( './styles/main.css' );

// Fullscreen our Elm App
var Elm = require( '../elm/Main' );
var app = Elm.Main.fullscreen();

app.ports.setTimezoneOffset.send(new Date().getTimezoneOffset())
