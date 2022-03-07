import * as wasm from "wof";
import "./elm.js";

const sound = new wasm.Audio();
let interval = null;

var app = Elm.Main.init({ node: document.querySelector("main") });

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
  sound.update_kick(freq, pitch, wave, decay, punch, volume);
});
app.ports.updateSnare.subscribe(function (params) {
  let { freq, blend, decay, punch, volume } = params;
  sound.update_snare(freq, blend, decay, punch, volume);
});
app.ports.updateKickSequence.subscribe(function (sequence) {
  sound.update_kick_steps(sequence)
});
app.ports.updateSnareSequence.subscribe(function (sequence) {
  sound.update_snare_steps(sequence)
});
app.ports.updateKickSequencerLength.subscribe(function (sequencerLength) {
  sound.update_sequencer_length("kick", sequencerLength)
});
app.ports.updateSnareSequencerLength.subscribe(function (sequencerLength) {
  sound.update_sequencer_length("snare", sequencerLength)
});
app.ports.updateKickOffset.subscribe(function (offset) {
  sound.update_offset("kick", offset)
});
app.ports.updateSnareOffset.subscribe(function (offset) {
  sound.update_offset("snare", offset)
});
app.ports.updateTempo.subscribe(function (tempo) {
  sound.update_tempo(tempo)
});

function getSteps() {
  const kickStep = sound.get_steps("kick")
  app.ports.receiveKickStepNumber.send(kickStep);
  const snareStep = sound.get_steps("snare")
  app.ports.receiveSnareStepNumber.send(snareStep);
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
