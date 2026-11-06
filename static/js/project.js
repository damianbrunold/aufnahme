// Project page: while takes are being processed or an export runs, poll
// the project's status and update the progress; reload when done.
const root = document.querySelector(".js-project");
if (root) {
  const label = root.querySelector(".js-export-label");
  const bar = root.querySelector(".js-export-progress");
  const pubLabel = root.querySelector(".js-publish-label");
  const pubBar = root.querySelector(".js-publish-progress");
  const active = (s) => ["queued", "running"].includes(s);
  const busy = (j) =>
    active(j.export.status) || active(j.publish.status) || j.takes.some((t) => t.status === "processing");
  const initiallyBusy =
    active(root.dataset.export) ||
    active(root.dataset.publish) ||
    root.querySelector('[data-status="processing"]') !== null;

  async function poll() {
    let j = null;
    try {
      const r = await fetch(root.dataset.api);
      if (r.ok) j = await r.json();
    } catch (e) {}
    if (j) {
      if (j.export.status === "running" || j.export.status === "queued") {
        bar.hidden = false;
        bar.value = Math.round(100 * j.export.progress);
        label.textContent =
          j.export.status === "queued" ? "Export wartet …" : "Export läuft … " + bar.value + " %";
      }
      if (pubBar && active(j.publish.status)) {
        pubBar.value = Math.round(100 * j.publish.progress);
        pubLabel.textContent =
          j.publish.status === "queued" ? "Wartet …" : "Wird hochgeladen … " + pubBar.value + " %";
      }
      if (!busy(j)) {
        location.reload();
        return;
      }
    }
    setTimeout(poll, 1500);
  }
  if (initiallyBusy) setTimeout(poll, 1000);
}
