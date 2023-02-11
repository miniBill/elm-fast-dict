
function getWorkersCount() {
  try {
    if (navigator.hardwareConcurrency)
      return navigator.hardwareConcurrency
  } catch { }
  return 2
}

const workersCount = getWorkersCount();

/** @type {Worker[]} */
var pool = Array();

/**
 * Get or spawn a worker for the pool
 * @param {number} index 
 */
function getWorker(index) {
  if (index in pool)
    return pool[index]

  const worker = new Worker("./backend-js.js", {
    name: `Worker ${index}`
  })
  worker.onmessage = function ({ data }) {
    app.ports.fromBackend.send({ index, data });
  }
  pool[index] = worker
  return worker
}

// @ts-ignore
const Frontend = Elm.Frontend;

const app = Frontend.init({
  node: document.getElementsByTagName("main")[0],
  flags: { workersCount: workersCount },
})

app.ports.toBackend.subscribe(function ({ index, data }) {
  /** @type {Worker} */
  var worker = getWorker(index);
  worker.postMessage(data);
})

app.ports.terminateAll.subscribe(function (index) {
  pool.forEach(worker => worker.terminate())
  pool = Array()
})
