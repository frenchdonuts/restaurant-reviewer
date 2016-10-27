// pull in desired CSS/SASS files
require( './styles/main.scss' );

// Fullscreen our Elm App
var Elm = require( '../elm/Main' );
var restaurantReviewer = Elm.Main.fullscreen();
