// Adjusts All-Star chip layouts after render so long honor rows stay balanced on player pages.
function layoutAllStarRows() {
  // Find every compact All-Star chip grid on the page.
  const grids = document.querySelectorAll(".all-star-chip-grid");

  grids.forEach((grid) => {
    // Read the current chip list before rebuilding the rows.
    const chips = Array.from(grid.querySelectorAll(".all-star-mini-chip"));
    if (!chips.length) {
      return;
    }

    // Unwrap any prior layout pass before measuring again.
    const existingRows = grid.querySelectorAll(".all-star-chip-row");
    if (existingRows.length) {
      const unwrapped = [];
      existingRows.forEach((row) => {
        unwrapped.push(...Array.from(row.children));
      });
      grid.replaceChildren(...unwrapped);
    }

    grid.style.justifyContent = "flex-start";

    // Group chips by the row they naturally wrapped onto.
    const rows = [];
    chips.forEach((chip) => {
      const top = chip.offsetTop;
      const lastRow = rows[rows.length - 1];

      if (!lastRow || Math.abs(lastRow.top - top) > 2) {
        rows.push({ top, chips: [chip] });
      } else {
        lastRow.chips.push(chip);
      }
    });

    if (rows.length <= 1) {
      return;
    }

    // Rebuild the grid into explicit row wrappers.
    const fragments = rows.map((row, index) => {
      const rowEl = document.createElement("div");
      rowEl.className = "all-star-chip-row";
      if (index === 0) {
        rowEl.classList.add("all-star-chip-row-first");
      } else {
        rowEl.classList.add("all-star-chip-row-centered");
      }
      rowEl.append(...row.chips);
      return rowEl;
    });

    grid.replaceChildren(...fragments);

    // Match centered-row widths to the first row so the grid looks even.
    const firstRow = grid.querySelector(".all-star-chip-row-first");
    const centeredRows = grid.querySelectorAll(".all-star-chip-row-centered");
    if (firstRow && centeredRows.length) {
      const firstRowWidth = Math.min(firstRow.scrollWidth, grid.clientWidth);
      centeredRows.forEach((rowEl) => {
        rowEl.style.width = `${firstRowWidth}px`;
      });
    }
  });
}

// Run once after load and again on resize.
window.addEventListener("load", layoutAllStarRows);
window.addEventListener("resize", () => {
  window.requestAnimationFrame(layoutAllStarRows);
});
