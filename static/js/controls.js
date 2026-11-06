// A small control window that stays on top while recording: timer,
// status, pause and stop, and the webcam image. Uses Document
// Picture-in-Picture where available (Chrome, Edge), else a popup window.
// The window holds no scripts of its own: its elements are wired from
// this page (same origin), and its styles are this page's stylesheets.
//
// Must be called from a click handler (both APIs need a user gesture).

export async function openControls({ onPause, onStop, camStream }) {
  let win = null;
  try {
    if ("documentPictureInPicture" in window) {
      win = await documentPictureInPicture.requestWindow({ width: 340, height: camStream ? 330 : 150 });
    } else {
      win = window.open("", "aufnahme-controls", "popup,width=360,height=" + (camStream ? 380 : 200));
    }
  } catch (e) {
    win = null;
  }
  if (!win) return null;

  const doc = win.document;
  doc.title = "Aufnahme";
  for (const link of document.querySelectorAll('link[rel="stylesheet"]')) {
    const copy = doc.createElement("link");
    copy.rel = "stylesheet";
    copy.href = link.href;
    doc.head.append(copy);
  }
  const theme = document.documentElement.getAttribute("data-theme");
  if (theme) doc.documentElement.setAttribute("data-theme", theme);
  doc.body.className = "pip-controls";
  doc.body.textContent = "";

  const el = (tag, cls, text) => {
    const e = doc.createElement(tag);
    if (cls) e.className = cls;
    if (text) e.textContent = text;
    return e;
  };
  const bar = el("div", "pip-bar");
  const timer = el("span", "timer", "0:00");
  const pause = el("button", "", "Pause");
  pause.type = "button";
  const stop = el("button", "danger", "Stopp");
  stop.type = "button";
  bar.append(timer, pause, stop);
  const status = el("p", "hint pip-status", "");
  doc.body.append(bar, status);
  if (camStream) {
    const video = el("video", "pip-cam");
    video.muted = true;
    video.autoplay = true;
    video.playsInline = true;
    video.srcObject = camStream;
    doc.body.append(video);
  }
  pause.addEventListener("click", onPause);
  stop.addEventListener("click", onStop);

  return {
    update({ time, state, text }) {
      if (win.closed) return;
      timer.textContent = time;
      pause.textContent = state === "paused" ? "Fortsetzen" : "Pause";
      doc.body.classList.toggle("is-recording", state === "recording");
      status.textContent = text || "";
    },
    close() {
      if (!win.closed) win.close();
    },
  };
}
