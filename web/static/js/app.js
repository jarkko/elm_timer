// Brunch automatically concatenates all files in your
// watched paths. Those paths can be configured at
// config.paths.watched in "brunch-config.js".
//
// However, those files will only be executed if
// explicitly imported. The only exception are files
// in vendor, which are never wrapped in imports and
// therefore are always executed.

// Import dependencies
//
// If you no longer want to use a dependency, remember
// to also remove its path from "config.paths.watched".
import "phoenix_html"

// Import local files
//
// Local files can be imported directly using relative
// paths "./socket" or full ones "web/static/js/socket".

// import socket from "./socket"

const elmDiv = document.getElementById('elm-main');
const elmApp = Elm.embed(
  Elm.ElmTimer,
  elmDiv,
  {
    randomSeed: [
      Math.floor(Math.random()*0xFFFFFFFF),
      Math.floor(Math.random()*0xFFFFFFFF)
    ],
    resultFires: false,
  }
);

const blurrer = (id = "store-result-button") => {
  document.getElementById(id).blur();
}

elmApp.ports.blurStoreResultButton.subscribe(blurrer);

document.addEventListener("keydown", function(e) {
  if (e.keyCode == "13") { // enter hit
    elmApp.ports.resultFires.send(true)
  }
});
