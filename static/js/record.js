// Recorder page: choose the screen source, microphone and webcam, then
// record. Two MediaRecorders run side by side:
//   screen: the captured video plus one mixed audio track (microphone and,
//           in Chromium, tab/system audio), mixed with Web Audio;
//   cam:    the webcam video alone (composited into the export later).
// Each recorder hands over a chunk every 2 s; chunks are uploaded in order
// with a sequence number (PUT .../takes/:tid/:track?seq=N) and retried
// until the server has them, so a short network outage loses nothing.
import { bestRecorderType, isChromium } from "./support.js";

const root = document.querySelector(".js-recorder");
const api = root.dataset.api;
const projectUrl = root.dataset.projectUrl;
const $ = (sel) => root.querySelector(sel);

const ui = {
  screenPreview: $(".js-screen-preview"),
  pickScreen: $(".js-pick-screen"),
  displayAudio: $(".js-display-audio"),
  displayAudioRow: $(".js-display-audio-row"),
  quality: $(".js-quality"),
  allow: $(".js-allow-devices"),
  mic: $(".js-mic"),
  level: $(".js-level"),
  cam: $(".js-cam"),
  camPreview: $(".js-cam-preview"),
  start: $(".js-start"),
  pause: $(".js-pause"),
  stop: $(".js-stop"),
  timer: $(".js-timer"),
  status: $(".js-status"),
  unsupported: $(".js-unsupported"),
};

const videoOnlyTypes = ["video/webm;codecs=vp9", "video/webm;codecs=vp8", "video/webm", "video/mp4"];

let displayStream = null;
let micStream = null;
let camStream = null;
let audioCtx = null;
let meterSource = null;
let state = "idle"; // idle | countdown | recording | paused | finishing

function setStatus(text) {
  ui.status.textContent = text;
}

function stopStream(s) {
  if (s) for (const t of s.getTracks()) t.stop();
}

function ctx() {
  if (!audioCtx) audioCtx = new AudioContext();
  if (audioCtx.state === "suspended") audioCtx.resume();
  return audioCtx;
}

function updateStartButton() {
  ui.start.disabled = !(displayStream && state === "idle");
}

// --- capability check ---------------------------------------------------

const md = navigator.mediaDevices;
if (!window.isSecureContext || !md || !md.getDisplayMedia || typeof MediaRecorder === "undefined") {
  ui.unsupported.hidden = false;
  ui.unsupported.textContent = !window.isSecureContext
    ? "Aufnehmen ist nur über HTTPS oder http://localhost möglich."
    : "Dieser Browser kann den Bildschirm nicht aufnehmen.";
  ui.pickScreen.disabled = true;
}
if (!isChromium()) {
  // Only Chromium offers tab or system audio.
  ui.displayAudioRow.hidden = true;
  ui.displayAudio.checked = false;
}

// --- screen ------------------------------------------------------------------

ui.pickScreen.addEventListener("click", async () => {
  const max = ui.quality.value === "720" ? [1280, 720] : [1920, 1080];
  const wantAudio = ui.displayAudio.checked;
  try {
    const s = await md.getDisplayMedia({
      video: { frameRate: { ideal: 30, max: 30 }, width: { max: max[0] }, height: { max: max[1] } },
      audio: wantAudio ? { echoCancellation: false, noiseSuppression: false, autoGainControl: false } : false,
      systemAudio: wantAudio ? "include" : "exclude",
      selfBrowserSurface: "exclude",
      surfaceSwitching: "include",
    });
    stopStream(displayStream);
    displayStream = s;
    ui.screenPreview.srcObject = s;
    const [track] = s.getVideoTracks();
    track.addEventListener("ended", () => {
      if (state === "recording" || state === "paused") stopRecording();
      else {
        displayStream = null;
        ui.screenPreview.srcObject = null;
        updateStartButton();
        setStatus("Die Freigabe wurde beendet. Bitte erneut wählen.");
      }
    });
    const audio = s.getAudioTracks().length > 0;
    setStatus(
      "Bereit." + (wantAudio && !audio ? " (Für diese Quelle bietet der Browser keinen Ton an.)" : "")
    );
    updateStartButton();
  } catch (e) {
    setStatus(e.name === "NotAllowedError" ? "Freigabe abgelehnt." : "Fehler: " + e.message);
  }
});

// --- microphone and webcam ----------------------------------------------------

async function fillDevices() {
  const devices = await md.enumerateDevices();
  const fill = (select, kind, none) => {
    const current = select.value;
    select.textContent = "";
    select.append(new Option(none, ""));
    let n = 1;
    for (const d of devices.filter((d) => d.kind === kind)) {
      select.append(new Option(d.label || none.replace("Kein", "Gerät") + " " + n++, d.deviceId));
    }
    if ([...select.options].some((o) => o.value === current)) select.value = current;
  };
  fill(ui.mic, "audioinput", "Kein Mikrofon");
  fill(ui.cam, "videoinput", "Keine Webcam");
  return devices.some((d) => d.label);
}

ui.allow.addEventListener("click", async () => {
  let s = null;
  try {
    s = await md.getUserMedia({ audio: true, video: true });
  } catch (e) {
    try {
      s = await md.getUserMedia({ audio: true });
    } catch (e2) {
      setStatus("Kein Zugriff auf Mikrofon oder Kamera.");
    }
  }
  stopStream(s);
  await fillDevices();
  // Preselect the default microphone.
  if (!ui.mic.value && ui.mic.options.length > 1) {
    ui.mic.selectedIndex = 1;
    ui.mic.dispatchEvent(new Event("change"));
  }
  ui.allow.hidden = true;
});

if (md && md.enumerateDevices) {
  fillDevices().then((labelled) => {
    if (labelled) ui.allow.hidden = true;
  });
}

ui.mic.addEventListener("change", async () => {
  stopStream(micStream);
  micStream = null;
  if (meterSource) meterSource.disconnect();
  meterSource = null;
  ui.level.value = 0;
  if (!ui.mic.value) return;
  try {
    micStream = await md.getUserMedia({
      audio: { deviceId: { exact: ui.mic.value }, echoCancellation: true, noiseSuppression: true, autoGainControl: true },
    });
    const analyser = ctx().createAnalyser();
    analyser.fftSize = 1024;
    meterSource = ctx().createMediaStreamSource(micStream);
    meterSource.connect(analyser);
    const data = new Float32Array(analyser.fftSize);
    const tick = () => {
      if (!micStream) return;
      analyser.getFloatTimeDomainData(data);
      let peak = 0;
      for (const v of data) peak = Math.max(peak, Math.abs(v));
      ui.level.value = Math.min(1, peak);
      requestAnimationFrame(tick);
    };
    tick();
  } catch (e) {
    setStatus("Mikrofon nicht verfügbar: " + e.message);
  }
});

ui.cam.addEventListener("change", async () => {
  stopStream(camStream);
  camStream = null;
  ui.camPreview.hidden = true;
  if (!ui.cam.value) return;
  try {
    camStream = await md.getUserMedia({
      video: { deviceId: { exact: ui.cam.value }, width: { ideal: 1280 }, height: { ideal: 720 }, frameRate: { ideal: 30 } },
    });
    ui.camPreview.srcObject = camStream;
    ui.camPreview.hidden = false;
  } catch (e) {
    setStatus("Webcam nicht verfügbar: " + e.message);
  }
});

// --- uploading chunks ------------------------------------------------------------

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

class ChunkQueue {
  constructor(base, track) {
    this.url = base + "/" + track;
    this.queue = [];
    this.next = 0; // seq of the next chunk to hand in
    this.sent = 0;
    this.running = false;
    this.idleWaiters = [];
  }

  push(blob) {
    this.queue.push({ seq: this.next++, blob });
    this.pump();
  }

  get pending() {
    return this.queue.length;
  }

  async pump() {
    if (this.running) return;
    this.running = true;
    let failures = 0;
    while (this.queue.length > 0) {
      const { seq, blob } = this.queue[0];
      let r = null;
      try {
        r = await fetch(this.url + "?seq=" + seq, {
          method: "PUT",
          headers: { "Content-Type": "application/octet-stream" },
          body: blob,
        });
      } catch (e) {
        r = null;
      }
      if (r && (r.ok || r.status === 409)) {
        // 409: the server expects another chunk; drop the ones it has.
        const j = r.ok ? null : await r.json().catch(() => ({}));
        if (j && typeof j.expected === "number" && j.expected < seq) {
          setStatus("Fehler: Teile fehlen auf dem Server (" + j.expected + ").");
          break;
        }
        this.queue.shift();
        this.sent++;
        failures = 0;
      } else if (r && r.status >= 400 && r.status < 500) {
        setStatus("Fehler beim Übertragen (" + r.status + ").");
        break;
      } else {
        failures++;
        setStatus("Verbindung unterbrochen – die Aufnahme läuft weiter, neuer Versuch …");
        await sleep(Math.min(10000, 500 * 2 ** failures));
      }
    }
    this.running = false;
    if (this.queue.length === 0) for (const w of this.idleWaiters.splice(0)) w();
  }

  drained() {
    return this.queue.length === 0 && !this.running
      ? Promise.resolve()
      : new Promise((r) => this.idleWaiters.push(r));
  }
}

// --- recording ----------------------------------------------------------------------

let recorders = [];
let queues = {};
let starts = {};
let takeBase = null;
let elapsed = 0;
let resumedAt = 0;
let timerId = null;

function formatTime(ms) {
  const s = Math.floor(ms / 1000);
  const m = Math.floor(s / 60);
  const h = Math.floor(m / 60);
  const two = (n) => String(n).padStart(2, "0");
  return h > 0 ? h + ":" + two(m % 60) + ":" + two(s % 60) : m + ":" + two(s % 60);
}

function tickTimer() {
  const ms = elapsed + (state === "recording" ? performance.now() - resumedAt : 0);
  ui.timer.textContent = formatTime(ms);
  const pending = Object.values(queues).reduce((n, q) => n + q.pending, 0);
  if (state === "recording")
    setStatus("Aufnahme läuft" + (pending > 2 ? " – " + pending + " Teile warten auf Übertragung" : ""));
}

function setControls() {
  const active = state === "recording" || state === "paused";
  ui.start.hidden = active || state === "finishing";
  ui.pause.hidden = !active;
  ui.stop.hidden = !active;
  ui.pause.textContent = state === "paused" ? "Fortsetzen" : "Pause";
  for (const el of [ui.pickScreen, ui.mic, ui.cam, ui.quality, ui.displayAudio, ui.allow]) el.disabled = state !== "idle";
  root.classList.toggle("is-recording", state === "recording");
  updateStartButton();
}

function makeRecorder(stream, types, bitrate, name) {
  const type = types.find((t) => MediaRecorder.isTypeSupported(t));
  const rec = new MediaRecorder(stream, { mimeType: type, videoBitsPerSecond: bitrate, audioBitsPerSecond: 128000 });
  rec.addEventListener("dataavailable", (e) => {
    if (e.data && e.data.size > 0) queues[name].push(e.data);
  });
  rec.addEventListener("start", () => {
    starts[name] = performance.timeOrigin + performance.now();
  });
  rec.addEventListener("error", (e) => setStatus("Aufnahmefehler: " + (e.error?.message || "unbekannt")));
  return { rec, type, name };
}

ui.start.addEventListener("click", async () => {
  if (!displayStream) return;
  state = "countdown";
  setControls();
  for (let i = 3; i > 0; i--) {
    setStatus("Die Aufnahme beginnt in " + i + " …");
    await sleep(1000);
  }

  // One audio track: microphone + tab/system audio.
  const sources = [];
  if (micStream) sources.push(micStream);
  const displayAudio = displayStream.getAudioTracks();
  if (displayAudio.length) sources.push(new MediaStream(displayAudio));
  const tracks = [...displayStream.getVideoTracks()];
  if (sources.length) {
    const dest = ctx().createMediaStreamDestination();
    for (const s of sources) ctx().createMediaStreamSource(s).connect(dest);
    tracks.push(...dest.stream.getAudioTracks());
  }

  const bitrate = ui.quality.value === "720" ? 2500000 : 5000000;
  const screenTypes = [bestRecorderType()].filter(Boolean);
  const screen = makeRecorder(new MediaStream(tracks), screenTypes, bitrate, "screen");
  const cam = camStream ? makeRecorder(new MediaStream(camStream.getVideoTracks()), videoOnlyTypes, 1200000, "cam") : null;

  const r = await fetch(api + "/takes", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ screenMime: screen.type, camMime: cam ? cam.type : null }),
  }).catch(() => null);
  const j = r ? await r.json().catch(() => ({})) : {};
  if (!r || !r.ok) {
    setStatus("Die Aufnahme konnte nicht beginnen: " + (j.error || (r ? r.status : "keine Verbindung")));
    state = "idle";
    setControls();
    return;
  }
  takeBase = api + "/takes/" + j.id;
  queues = { screen: new ChunkQueue(takeBase, "screen") };
  if (cam) queues.cam = new ChunkQueue(takeBase, "cam");
  starts = {};
  recorders = [screen, cam].filter(Boolean);
  for (const { rec } of recorders) rec.start(2000);
  elapsed = 0;
  resumedAt = performance.now();
  state = "recording";
  setControls();
  timerId = setInterval(tickTimer, 500);
});

ui.pause.addEventListener("click", () => {
  if (state === "recording") {
    for (const { rec } of recorders) rec.pause();
    elapsed += performance.now() - resumedAt;
    state = "paused";
    setStatus("Pausiert");
  } else if (state === "paused") {
    for (const { rec } of recorders) rec.resume();
    resumedAt = performance.now();
    state = "recording";
  }
  setControls();
});

ui.stop.addEventListener("click", () => stopRecording());

async function stopRecording() {
  if (state !== "recording" && state !== "paused") return;
  if (state === "recording") elapsed += performance.now() - resumedAt;
  state = "finishing";
  setControls();
  clearInterval(timerId);
  tickTimer();
  setStatus("Die letzten Teile werden übertragen …");
  await Promise.all(
    recorders.map(
      ({ rec }) =>
        new Promise((resolve) => {
          if (rec.state === "inactive") return resolve();
          rec.addEventListener("stop", resolve, { once: true });
          rec.stop();
        })
    )
  );
  await Promise.all(Object.values(queues).map((q) => q.drained()));
  const r = await fetch(takeBase + "/finish", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ screenStart: starts.screen ?? null, camStart: starts.cam ?? null }),
  }).catch(() => null);
  stopStream(displayStream);
  stopStream(camStream);
  stopStream(micStream);
  if (r && r.ok) {
    setStatus("Fertig. Die Aufnahme wird verarbeitet …");
    location.href = projectUrl;
  } else {
    const j = r ? await r.json().catch(() => ({})) : {};
    setStatus("Abschliessen fehlgeschlagen: " + (j.error || "keine Verbindung") +
      ". Auf der Projektseite lässt sich die Aufnahme retten.");
  }
}

window.addEventListener("beforeunload", (e) => {
  if (state !== "idle") e.preventDefault();
});

// For tests and diagnostics.
window.aufnahme = { get state() { return state; }, stopRecording };
