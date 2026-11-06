// Editor: a cut list over the project's takes, with preview playback,
// a timeline (film strip + waveform per segment), split/remove/reorder
// with undo, the webcam layout and loudness option. Every change is saved
// to PUT <api>/edit (debounced); the export renders exactly that.
//
// Model (as in the server's project.scm):
//   segments: [{take, start, end}]  pieces of takes, in playing order
//   webcam:   {on, corner, size}    corner: top-left|top-right|bottom-left|bottom-right
//   loudnorm: bool
// Positions on the timeline ("T") run from 0 to the sum of segment lengths.

const root = document.querySelector(".js-editor");
const api = root.dataset.api + "/edit";
const $ = (sel) => root.querySelector(sel);

const ui = {
  stage: $(".js-stage"),
  main: $(".js-main"),
  cam: $(".js-camview"),
  play: $(".js-play"),
  time: $(".js-time"),
  split: $(".js-split"),
  remove: $(".js-remove"),
  left: $(".js-left"),
  right: $(".js-right"),
  undo: $(".js-undo"),
  reset: $(".js-reset"),
  timeline: $(".js-timeline"),
  segments: $(".js-segments"),
  playhead: $(".js-playhead"),
  camOn: $(".js-cam-on"),
  corner: $(".js-corner"),
  size: $(".js-size"),
  loudnorm: $(".js-loudnorm"),
  saveStatus: $(".js-save-status"),
  exportForm: root.querySelector(".js-export-form"),
};

let takes = new Map(); // id -> take info from the server
let takeOrder = []; // ids in recording order
let model = { segments: [], webcam: { on: true, corner: "bottom-right", size: 0.22 }, loudnorm: false };
let undoStack = [];
let selected = 0;
let pos = 0; // timeline position, seconds
let playing = false;
let current = -1; // index of the segment loaded in the player
let outputSize = [1920, 1080];

// --- helpers ------------------------------------------------------------------

const len = (s) => s.end - s.start;
const total = () => model.segments.reduce((n, s) => n + len(s), 0);

function offsets() {
  const out = [];
  let t = 0;
  for (const s of model.segments) {
    out.push(t);
    t += len(s);
  }
  return out;
}

// The segment index and take time at timeline position T.
function locate(T) {
  const offs = offsets();
  for (let k = 0; k < model.segments.length; k++) {
    const s = model.segments[k];
    if (T < offs[k] + len(s) || k === model.segments.length - 1) {
      return { k, t: Math.min(s.end, Math.max(s.start, s.start + (T - offs[k]))) };
    }
  }
  return { k: -1, t: 0 };
}

function fmt(sec) {
  const s = Math.max(0, Math.floor(sec + 0.0001));
  const m = Math.floor(s / 60);
  return m + ":" + String(s % 60).padStart(2, "0");
}

const snapshot = () => JSON.parse(JSON.stringify(model));

function change(mutate) {
  undoStack.push(snapshot());
  if (undoStack.length > 200) undoStack.shift();
  mutate();
  afterChange();
}

function afterChange() {
  selected = Math.max(0, Math.min(selected, model.segments.length - 1));
  pos = Math.min(pos, Math.max(0, total() - 0.001));
  current = -1; // segment boundaries may have moved
  render();
  applyWebcamLayout();
  seek(pos);
  scheduleSave();
}

// --- saving ---------------------------------------------------------------------

let saveTimer = null;
let saving = null;

function scheduleSave() {
  ui.saveStatus.textContent = "Ungespeicherte Änderungen …";
  clearTimeout(saveTimer);
  saveTimer = setTimeout(save, 600);
}

async function save() {
  clearTimeout(saveTimer);
  saveTimer = null;
  ui.saveStatus.textContent = "Wird gespeichert …";
  const body = JSON.stringify({ ...model, knownTakes: takeOrder });
  saving = fetch(api, { method: "PUT", headers: { "Content-Type": "application/json" }, body })
    .then(async (r) => {
      if (!r.ok) throw new Error(String(r.status));
      const j = await r.json();
      // A take that became ready meanwhile was appended by the server.
      if (j.segments.length !== model.segments.length) load(j, true);
      ui.saveStatus.textContent = "Gespeichert.";
    })
    .catch((e) => {
      ui.saveStatus.textContent = "Speichern fehlgeschlagen (" + e.message + ") – neuer Versuch …";
      saveTimer = setTimeout(save, 3000);
    })
    .finally(() => (saving = null));
  return saving;
}

async function flushSave() {
  if (saveTimer) await save();
  else if (saving) await saving;
}

// --- rendering ---------------------------------------------------------------------

function layer(cls, url, seg, take) {
  const el = document.createElement("div");
  el.className = cls;
  if (url) {
    el.style.backgroundImage = "url(" + JSON.stringify(url) + ")";
    // The image covers the whole take; show only this segment's part.
    el.style.backgroundSize = (100 * take.duration) / len(seg) + "% 100%";
    const rest = take.duration - len(seg);
    el.style.backgroundPositionX = rest > 0 ? (100 * seg.start) / rest + "%" : "0%";
  }
  return el;
}

function render() {
  const T = total();
  ui.segments.textContent = "";
  model.segments.forEach((seg, k) => {
    const take = takes.get(seg.take);
    const el = document.createElement("div");
    el.className = "segment" + (k === selected ? " selected" : "");
    el.style.width = (100 * len(seg)) / T + "%";
    el.dataset.index = k;
    el.append(layer("strip", take.strip, seg, take));
    el.append(layer("wave", take.wave, seg, take));
    const label = document.createElement("span");
    label.className = "label";
    label.textContent = "A" + (takeOrder.indexOf(seg.take) + 1) + " · " + fmt(len(seg));
    el.append(label);
    ui.segments.append(el);
  });
  ui.remove.disabled = model.segments.length <= 1;
  ui.left.disabled = selected <= 0;
  ui.right.disabled = selected >= model.segments.length - 1;
  ui.undo.disabled = undoStack.length === 0;
  updatePlayhead();
}

function updatePlayhead() {
  const T = total();
  ui.playhead.style.left = (T > 0 ? (100 * pos) / T : 0) + "%";
  ui.time.textContent = fmt(pos) + " / " + fmt(T);
}

function applyWebcamLayout() {
  const w = model.webcam;
  ui.camOn.checked = w.on;
  ui.corner.value = w.corner;
  ui.size.value = w.size;
  ui.stage.style.aspectRatio = outputSize[0] + " / " + outputSize[1];
  ui.cam.style.width = 100 * w.size + "%";
  const m = (100 * 24) / outputSize[0] + "%"; // the export's 24 px margin
  const mv = (100 * 24) / outputSize[1] + "%";
  ui.cam.style.left = w.corner.endsWith("left") ? m : "";
  ui.cam.style.right = w.corner.endsWith("right") ? m : "";
  ui.cam.style.top = w.corner.startsWith("top") ? mv : "";
  ui.cam.style.bottom = w.corner.startsWith("bottom") ? mv : "";
  ui.loudnorm.checked = model.loudnorm;
  syncCam();
}

// --- playback -----------------------------------------------------------------------

function loadSource(video, url) {
  if (video.dataset.src === url) return Promise.resolve();
  video.dataset.src = url;
  video.src = url;
  return new Promise((resolve) => {
    video.addEventListener("loadedmetadata", resolve, { once: true });
    video.addEventListener("error", resolve, { once: true });
  });
}

async function seek(T) {
  pos = Math.max(0, Math.min(T, total()));
  const { k, t } = locate(pos);
  if (k < 0) return;
  selected = playing ? selected : k;
  current = k;
  const take = takes.get(model.segments[k].take);
  await loadSource(ui.main, take.screen);
  ui.main.currentTime = t;
  syncCam(true);
  updatePlayhead();
  for (const el of ui.segments.children) el.classList.toggle("selected", Number(el.dataset.index) === selected);
}

// Shows the webcam of the current take at the matching time, if any.
async function syncCam(force) {
  const seg = model.segments[current];
  const take = seg && takes.get(seg.take);
  if (!model.webcam.on || !take || !take.cam) {
    ui.cam.hidden = true;
    ui.cam.pause();
    return;
  }
  const want = ui.main.currentTime - take.camOffset;
  await loadSource(ui.cam, take.cam);
  ui.cam.hidden = want < 0 || want > (ui.cam.duration || Infinity);
  if (force || Math.abs(ui.cam.currentTime - want) > 0.25) ui.cam.currentTime = Math.max(0, want);
  if (playing && ui.cam.paused && !ui.cam.hidden) ui.cam.play().catch(() => {});
  if (!playing) ui.cam.pause();
}

function tick() {
  if (!playing) return;
  const seg = model.segments[current];
  if (seg) {
    const t = ui.main.currentTime;
    const offs = offsets();
    if (t >= seg.end - 0.03 || ui.main.ended) {
      if (current + 1 < model.segments.length) {
        seek(offs[current + 1]).then(() => ui.main.play().catch(() => {}));
      } else {
        stop();
        pos = total();
        updatePlayhead();
        return;
      }
    } else {
      pos = offs[current] + (t - seg.start);
      updatePlayhead();
      syncCam(false);
    }
  }
  requestAnimationFrame(tick);
}

async function play() {
  if (pos >= total() - 0.05) await seek(0);
  else await seek(pos);
  playing = true;
  ui.play.textContent = "Pause";
  await ui.main.play().catch(() => {});
  syncCam(true);
  requestAnimationFrame(tick);
}

function stop() {
  playing = false;
  ui.play.textContent = "Abspielen";
  ui.main.pause();
  ui.cam.pause();
}

// --- operations ------------------------------------------------------------------------

function split() {
  const { k, t } = locate(pos);
  const s = model.segments[k];
  if (!s || t - s.start < 0.1 || s.end - t < 0.1) return;
  change(() => {
    model.segments.splice(k, 1, { ...s, end: t }, { ...s, start: t });
    selected = k + 1;
  });
}

function removeSelected() {
  if (model.segments.length <= 1) return;
  change(() => {
    const offs = offsets();
    pos = offs[selected];
    model.segments.splice(selected, 1);
  });
}

function move(delta) {
  const j = selected + delta;
  if (j < 0 || j >= model.segments.length) return;
  change(() => {
    const segs = model.segments;
    [segs[selected], segs[j]] = [segs[j], segs[selected]];
    selected = j;
    pos = offsets()[j];
  });
}

function undo() {
  const prev = undoStack.pop();
  if (!prev) return;
  model = prev;
  afterChange();
}

function resetAll() {
  change(() => {
    model.segments = takeOrder.map((id) => ({ take: id, start: 0, end: takes.get(id).duration }));
    selected = 0;
    pos = 0;
  });
}

// --- events -----------------------------------------------------------------------------

ui.play.addEventListener("click", () => (playing ? stop() : play()));
ui.split.addEventListener("click", split);
ui.remove.addEventListener("click", removeSelected);
ui.left.addEventListener("click", () => move(-1));
ui.right.addEventListener("click", () => move(1));
ui.undo.addEventListener("click", undo);
ui.reset.addEventListener("click", resetAll);

ui.timeline.addEventListener("click", (e) => {
  const rect = ui.segments.getBoundingClientRect();
  const x = Math.max(0, Math.min(1, (e.clientX - rect.left) / rect.width));
  const T = x * total();
  selected = locate(T).k;
  render();
  seek(T);
});

ui.camOn.addEventListener("change", () => change(() => (model.webcam.on = ui.camOn.checked)));
ui.corner.addEventListener("change", () => change(() => (model.webcam.corner = ui.corner.value)));
ui.size.addEventListener("input", () => {
  model.webcam.size = Number(ui.size.value);
  applyWebcamLayout();
});
ui.size.addEventListener("change", () => {
  // One undo step per drag, with the value from before the drag.
  const before = snapshot();
  before.webcam.size = Number(ui.size.dataset.before || before.webcam.size);
  undoStack.push(before);
  ui.size.dataset.before = ui.size.value;
  afterChange();
});
ui.loudnorm.addEventListener("change", () => change(() => (model.loudnorm = ui.loudnorm.checked)));

document.addEventListener("keydown", (e) => {
  if (e.target.closest("input, select, textarea")) return;
  if (e.key === " ") {
    e.preventDefault();
    playing ? stop() : play();
  } else if (e.key === "s" || e.key === "S") {
    split();
  } else if (e.key === "Delete" || e.key === "Backspace") {
    e.preventDefault();
    removeSelected();
  } else if (e.key === "ArrowLeft" || e.key === "ArrowRight") {
    e.preventDefault();
    const step = (e.shiftKey ? 5 : 1) * (e.key === "ArrowLeft" ? -1 : 1);
    seek(pos + step);
  } else if ((e.ctrlKey || e.metaKey) && e.key.toLowerCase() === "z") {
    e.preventDefault();
    undo();
  }
});

ui.exportForm.addEventListener("submit", async (e) => {
  if (!saveTimer && !saving) return;
  e.preventDefault();
  await flushSave();
  ui.exportForm.submit();
});

window.addEventListener("beforeunload", (e) => {
  if (saveTimer || saving) e.preventDefault();
});

// --- loading ---------------------------------------------------------------------------------

function load(j, keepPosition) {
  takes = new Map(j.takes.map((t) => [t.id, t]));
  takeOrder = j.takes.map((t) => t.id);
  model = { segments: j.segments, webcam: j.webcam, loudnorm: j.loudnorm };
  ui.size.dataset.before = model.webcam.size;
  const first = model.segments.length ? takes.get(model.segments[0].take) : j.takes[0];
  if (first && first.width > 0) {
    // As the export: the first segment's size, at most 1920 wide.
    const scale = Math.min(1, 1920 / first.width);
    outputSize = [Math.round(first.width * scale), Math.round(first.height * scale)];
  }
  if (!keepPosition) {
    pos = 0;
    selected = 0;
  }
  current = -1;
  render();
  applyWebcamLayout();
  seek(pos);
}

async function refreshPreviews() {
  // Film strips are made in the background; fetch them once they exist.
  if ([...takes.values()].every((t) => t.strip)) return;
  setTimeout(async () => {
    try {
      const j = await (await fetch(api)).json();
      for (const t of j.takes) if (takes.has(t.id)) Object.assign(takes.get(t.id), { strip: t.strip, wave: t.wave });
      render();
    } catch (e) {}
    refreshPreviews();
  }, 3000);
}

(async () => {
  const r = await fetch(api);
  if (!r.ok) {
    ui.saveStatus.textContent = "Laden fehlgeschlagen (" + r.status + ").";
    return;
  }
  load(await r.json(), false);
  ui.saveStatus.textContent = "";
  refreshPreviews();
})();

// For tests and diagnostics.
window.editor = {
  get model() { return model; },
  get pos() { return pos; },
  seek, split, removeSelected, move, undo, resetAll, flushSave,
  select(k) { selected = k; render(); },
};
