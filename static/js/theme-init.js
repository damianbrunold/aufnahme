// Loaded in <head> without defer: applies the theme before the first paint,
// so the page never flashes the wrong palette. Order: the visitor's own
// choice (toggle, localStorage), then the site's default scheme
// (data-theme-default on <html>), else the CSS follows the OS setting.
(function () {
  var root = document.documentElement;
  var t = null;
  try {
    t = localStorage.getItem("theme");
  } catch (e) {}
  if (t !== "dark" && t !== "light") t = root.getAttribute("data-theme-default");
  if (t === "dark" || t === "light") root.setAttribute("data-theme", t);
})();
