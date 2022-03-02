import * as wasm from "wof";
import "./elm.js";

const sound = new wasm.Audio();
let interval = null;

var app = Elm.Main.init({ node: document.querySelector("main") });

app.ports.playKick.subscribe(function (params) {
  let { freq, pitch, wave, decay, punch, volume } = params;
  sound.play(freq, pitch, wave, decay, punch, volume);
});
app.ports.playSequence.subscribe(function () {
  sound.start();
  if (!interval) {
    interval = setInterval(() => {
      sound.schedule();
    }, 20);
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
  let { freq, pitch, wave, decay, punch, volume } = params;
  sound.update(freq, pitch, wave, decay, punch, volume);
});
app.ports.updateSequence.subscribe(function (sequence) {
  sound.update_steps(sequence)
});
app.ports.updateSequencerLength.subscribe(function (sequencerLength) {
  sound.update_sequencer_length(sequencerLength)
});
app.ports.updateOffset.subscribe(function (offset) {
  sound.update_offset(offset)
});
app.ports.updateTempo.subscribe(function (tempo) {
  sound.update_tempo(tempo)
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
