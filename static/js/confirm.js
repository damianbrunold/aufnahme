// Forms with class js-confirm ask before they submit (data-confirm).
for (const form of document.querySelectorAll("form.js-confirm")) {
  form.addEventListener("submit", (ev) => {
    if (!window.confirm(form.dataset.confirm || "Sicher?")) ev.preventDefault();
  });
}
