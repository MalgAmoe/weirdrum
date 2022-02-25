import * as wasm from "wof";
import "./elm.js";

const sound = new wasm.Audio();
let interval = null;

var app = Elm.Main.init({ node: document.querySelector("main") });

app.ports.playKick.subscribe(function (params) {
  let { freq, pitch, wave, decay, attack, volume } = params;
  sound.play(freq, pitch, wave, decay, attack, volume);
});
app.ports.playSequence.subscribe(function () {
  sound.start();
  if (!interval) {
    interval = setInterval(() => {
      sound.schedule();
    }, 40);
  }
});
app.ports.stopSequence.subscribe(function () {
  if (interval) {
    clearInterval(interval);
    sound.stop();
    interval = null;
  }
});
app.ports.updateKick.subscribe(function (params) {
  let { freq, pitch, wave, decay, attack, volume } = params;
  sound.update(freq, pitch, wave, decay, attack, volume);
});

function getSteps() {
  const step = sound.get_steps()
  app.ports.receiveStepNumber.send(step);
  window.requestAnimationFrame(getSteps)
}
window.requestAnimationFrame(getSteps)

// if ("serviceWorker" in navigator) {
//   window.addEventListener("load", () => {
//     navigator.serviceWorker
//       .register("./sw.js")
//       .then((registration) => {
//         // console.log("SW registered: ", registration);
//       })
//       .catch((registrationError) => {
//         console.log("SW registration failed: ", registrationError);
//       });
//   });
// }
