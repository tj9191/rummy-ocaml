// ui_bonsai/src/listener.js

// Firestore REST URL for your shared game doc
const FIRESTORE_URL =
  "https://firestore.googleapis.com/v1/projects/rummy-ocaml/databases/(default)/documents/rummy/n9HZyYuqk9cUnEvEDcz5";

console.log("[listener.js] Loaded");

// Helper: fetch the state string from Firestore via REST
async function fetchStateString() {
  try {
    const resp = await fetch(FIRESTORE_URL);
    if (!resp.ok) {
      console.error("[listener.js] Fetch error:", resp.status);
      return null;
    }
    const data = await resp.json();
    const fields = data.fields || {};
    const stateField = fields.state || {};
    const stateString = stateField.stringValue || null;
    return stateString;
  } catch (e) {
    console.error("[listener.js] Exception while fetching state:", e);
    return null;
  }
}

/**
 * Global function called by OCaml:
 *   Firestore.start_realtime_listener (fun state -> ...)
 *
 * OCaml passes us a callback `cb` that expects the serialized state string.
 * We poll Firestore every 1.5s and call `cb` whenever the state changes.
 */
window.startRummyListener = function startRummyListener(cb) {
  console.log("[listener.js] startRummyListener called from OCaml");

  let lastState = null;
  let stopped = false;

  async function loop() {
    if (stopped) return;

    const stateString = await fetchStateString();
    if (stateString && stateString !== lastState) {
      console.log("[listener.js] New remote state, notifying OCaml");
      lastState = stateString;
      try {
        cb(stateString); // OCaml callback
      } catch (e) {
        console.error("[listener.js] Error calling OCaml callback:", e);
      }
    }

    // Poll again in 1500 ms
    setTimeout(loop, 1500);
  }

  // Start immediately
  loop();

  // Optional: return a stop function (not used by OCaml currently)
  return function stop() {
    stopped = true;
  };
};