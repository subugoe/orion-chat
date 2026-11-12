// Right-align table columns that hold numbers, as in a spreadsheet.
// The model writes plain Markdown tables, which are all left-aligned; this
// marks every cell of a numeric column with .num (styled in gau.scss).
// Years stay left: they label rows rather than measure anything.
(() => {
  const number = /^[−–+-]?[$€£]?\s?\d[\d.,   ]*\s?(%|GB|TB|MB)?$/;
  const year = /^(1[89]|20)\d\d$/;
  const empty = new Set(["", "-", "–", "—", "NA", "n/a"]);

  const mark = (table) => {
    const rows = [...table.rows];
    if (rows.length < 2) return;
    const width = Math.max(...rows.map((r) => r.cells.length));
    for (let c = 0; c < width; c++) {
      const values = rows
        .slice(1)
        .map((r) => r.cells[c]?.textContent.trim() ?? "")
        .filter((t) => !empty.has(t));
      const numeric =
        values.length > 0 &&
        values.every((t) => number.test(t)) &&
        !values.every((t) => year.test(t));
      rows.forEach((r) => r.cells[c]?.classList.toggle("num", numeric));
    }
  };

  // Answers stream in and re-render, so check again after DOM changes,
  // at most once per frame.
  let queued = false;
  const run = () => {
    queued = false;
    document.querySelectorAll("shiny-chat-container table").forEach(mark);
  };
  new MutationObserver(() => {
    if (!queued) {
      queued = true;
      requestAnimationFrame(run);
    }
  }).observe(document.documentElement, { childList: true, subtree: true });
})();
