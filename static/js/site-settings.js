// Site settings: live preview of both colour schemes while picking.
const previews = {};
for (const p of document.querySelectorAll(".js-preview")) previews[p.dataset.scheme] = p;

function apply(input) {
  const p = previews[input.dataset.scheme];
  if (p) p.style.setProperty("--pv-" + input.dataset.key, input.value);
}

for (const input of document.querySelectorAll(".js-color")) {
  apply(input);
  input.addEventListener("input", () => apply(input));
}
