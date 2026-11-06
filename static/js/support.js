// Home page: checks what this browser can do for screen recording and
// fills the table rows (data-check) with "ja"/"nein" and a hint.

// The recording formats we would use, best first. The server converts
// whatever comes in to H.264/AAC MP4, so any of them is fine.
export const recorderTypes = [
  "video/webm;codecs=vp9,opus",
  "video/webm;codecs=vp8,opus",
  "video/webm",
  "video/mp4;codecs=avc1.42E01F,mp4a.40.2",
  "video/mp4",
];

export function bestRecorderType() {
  if (typeof MediaRecorder === "undefined") return null;
  return recorderTypes.find((t) => MediaRecorder.isTypeSupported(t)) || null;
}

// Chromium (Chrome, Edge, ...) is the only engine that offers tab and
// system audio in getDisplayMedia.
export function isChromium() {
  const brands = navigator.userAgentData?.brands || [];
  return brands.some((b) => /Chromium/.test(b.brand));
}

export function checks() {
  const md = navigator.mediaDevices;
  const type = bestRecorderType();
  return {
    secure: window.isSecureContext
      ? [true, ""]
      : [false, "Nur über HTTPS oder http://localhost möglich."],
    display: md && md.getDisplayMedia ? [true, ""] : [false, "Dieser Browser kann den Bildschirm nicht aufnehmen."],
    devices: md && md.getUserMedia ? [true, ""] : [false, "Kein Zugriff auf Mikrofon oder Webcam."],
    recorder: type ? [true, type] : [false, "MediaRecorder fehlt."],
    "display-audio": isChromium()
      ? [true, "Ton eines geteilten Tabs; Systemton je nach Betriebssystem."]
      : [false, "Nur das Mikrofon wird aufgenommen."],
    pip: "documentPictureInPicture" in window
      ? [true, ""]
      : [false, "Stattdessen ein kleines Popup-Fenster."],
  };
}

const table = document.querySelector(".js-support");
if (table) {
  const results = checks();
  for (const row of table.querySelectorAll("tr[data-check]")) {
    const [ok, detail] = results[row.dataset.check] || [false, ""];
    const cell = row.querySelector(".js-result");
    cell.textContent = ok ? "ja" : "nein";
    cell.className = "js-result " + (ok ? "yes" : "no");
    row.querySelector(".js-detail").textContent = detail;
  }
}
