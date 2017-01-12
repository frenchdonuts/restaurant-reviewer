import runtime from 'serviceworker-webpack-plugin/lib/runtime' // Service Worker

// pull in desired CSS/SASS files
require('./styles/main.css');

// Fullscreen our Elm App
var Elm = require('../elm/Main');
var app = Elm.Main.fullscreen();

app.ports.setTimezoneOffset.send(new Date().getTimezoneOffset())

// Setup serviceWorker
if ('serviceWorker' in navigator) {
    var swConfig = {
        scope: '/'
    }

    runtime.register()
    console.log("Registration worked!")
}
