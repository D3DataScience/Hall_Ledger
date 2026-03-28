// Adds client-side sorting to directory and stat tables.
document.addEventListener("DOMContentLoaded", () => {
  // Find all sortable tables on the page.
  const tables = document.querySelectorAll(".sortable-table");

  const parseValue = (text, type) => {
    // Parse text values into sortable text or numeric values.
    const value = text.trim();

    if (type === "number") {
      if (value === "" || value === "Unknown") return Number.NEGATIVE_INFINITY;
      const normalized = value.startsWith(".") ? `0${value}` : value;
      const parsed = Number.parseFloat(normalized.replace(/,/g, ""));
      return Number.isNaN(parsed) ? Number.NEGATIVE_INFINITY : parsed;
    }

    return value.toLowerCase();
  };

  tables.forEach((table) => {
    // Table-specific sort controls and row store.
    const headers = table.querySelectorAll("thead th");
    const tbody = table.querySelector("tbody");
    if (!tbody) return;

    headers.forEach((header, index) => {
      header.addEventListener("click", () => {
        // Sort the table by the clicked header.
        const currentDirection = header.classList.contains("sort-asc")
          ? "asc"
          : header.classList.contains("sort-desc")
            ? "desc"
            : "";
        const nextDirection = currentDirection === "asc" ? "desc" : "asc";
        const type = header.dataset.sortType || "text";
        const rows = Array.from(tbody.querySelectorAll("tr"));

        headers.forEach((otherHeader) => {
          otherHeader.classList.remove("sort-asc", "sort-desc");
        });

        rows.sort((a, b) => {
          const aValue = parseValue(a.children[index]?.textContent || "", type);
          const bValue = parseValue(b.children[index]?.textContent || "", type);

          if (aValue < bValue) return nextDirection === "asc" ? -1 : 1;
          if (aValue > bValue) return nextDirection === "asc" ? 1 : -1;
          return 0;
        });

        header.classList.add(nextDirection === "asc" ? "sort-asc" : "sort-desc");
        tbody.append(...rows);
      });
    });
  });
});
