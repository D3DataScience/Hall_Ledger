// Balances long team and plaque layouts on player pages after the DOM loads.
function collapsePlaqueYears(yearText) {
  // Parse a year string into sortable ranges.
  const parts = yearText
    .split(",")
    .map((part) => part.trim())
    .filter(Boolean);

  const years = [];
  parts.forEach((part) => {
    const rangeMatch = part.match(/^(\d{4})-(\d{4})$/);
    const singleMatch = part.match(/^(\d{4})$/);

    if (rangeMatch) {
      const start = Number(rangeMatch[1]);
      const end = Number(rangeMatch[2]);
      if (!Number.isNaN(start) && !Number.isNaN(end)) {
        years.push({ start, end });
      }
      return;
    }

    if (singleMatch) {
      const year = Number(singleMatch[1]);
      if (!Number.isNaN(year)) {
        years.push({ start: year, end: year });
      }
    }
  });

  if (!years.length) {
    return yearText;
  }

  // Merge overlapping or adjacent year ranges.
  years.sort((a, b) => a.start - b.start || a.end - b.end);

  const collapsed = [];
  years.forEach((range) => {
    const last = collapsed[collapsed.length - 1];
    if (!last || range.start > last.end + 1) {
      collapsed.push({ ...range });
      return;
    }

    last.end = Math.max(last.end, range.end);
  });

  return collapsed
    .map((range) => (range.start === range.end ? String(range.start) : `${range.start}-${range.end}`))
    .join(", ");
}

function normalizePlaqueTeamLines() {
  // Normalize long team-history lists into collapsed year lines.
  const teamLists = document.querySelectorAll(".player-team-block .detail-list-compact");

  teamLists.forEach((list) => {
    const items = Array.from(list.querySelectorAll("li"));
    if (items.length < 2) {
      return;
    }

    const grouped = new Map();
    const order = [];

    // Group repeated team names so their years can be collapsed together.
    items.forEach((item) => {
      const text = item.textContent.trim();
      const match = text.match(/^(.*?),\s+(.+)$/);
      if (!match) {
        return;
      }

      const team = match[1].trim();
      const years = match[2].trim();

      if (!grouped.has(team)) {
        grouped.set(team, []);
        order.push(team);
      }

      grouped.get(team).push(years);
    });

    if (order.length < 2 && grouped.size === items.length) {
      return;
    }

    const replacementItems = order.map((team) => {
      // Write the collapsed team line back into the list.
      const li = document.createElement("li");
      li.textContent = `${team}, ${collapsePlaqueYears(grouped.get(team).join(", "))}`;
      return li;
    });

    list.replaceChildren(...replacementItems);
  });
}

// Run after page load so measured team text is stable.
window.addEventListener("load", normalizePlaqueTeamLines);
