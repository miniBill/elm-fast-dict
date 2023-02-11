
importScripts("./backend-elm.js");

// @ts-ignore
const Backend = Elm.Backend;

const app = Backend.init({ flags: {} });

app.ports.toFrontend.subscribe(function (value) {
  postMessage(value);
});

onmessage = function ({ data }) {
  app.ports.fromFrontend.send(data);
};