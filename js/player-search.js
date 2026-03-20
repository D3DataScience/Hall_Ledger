// Powers the shared Hall Ledger player search bar and result navigation across the site.
document.addEventListener("DOMContentLoaded", () => {
  // Shared player index and current page path details.
  const players = Array.isArray(window.HALL_LEDGER_PLAYERS) ? window.HALL_LEDGER_PLAYERS : [];
  const pathName = window.location.pathname.replace(/\\/g, "/").toLowerCase();
  const isNestedPage =
    pathName.includes("/players/") ||
    pathName.includes("/milestone-players/") ||
    pathName.includes("/active-players/") ||
    pathName.includes("/retired-players/");
  const basePrefix = isNestedPage ? "../" : "./";

  // Search helpers for matching names and building links.
  const buildHref = (path) => `${basePrefix}${path}`;
  const normalize = (value) => value.trim().toLowerCase();

  const findMatches = (query, limit = 8) => {
    const normalizedQuery = normalize(query);

    if (!normalizedQuery) {
      return [];
    }

    return players
      .filter((player) => normalize(player.name).includes(normalizedQuery))
      .slice(0, limit);
  };

  const createResultMarkup = (player) => `
    <a class="topbar-search-result" href="${buildHref(player.path)}">
      <span class="topbar-search-name">${player.name}</span>
      <span class="topbar-search-meta">${player.status} - ${player.note}</span>
    </a>
  `;

  document.querySelectorAll("[data-global-player-search]").forEach((form) => {
    // Form-local search UI pieces.
    const input = form.querySelector(".topbar-search-input");
    const results = form.querySelector(".topbar-search-results");

    if (!input || !results) {
      return;
    }

    const renderMatches = (query) => {
      // Update the dropdown based on the current search text.
      const matches = findMatches(query);

      if (!query.trim()) {
        results.hidden = true;
        results.innerHTML = "";
        return matches;
      }

      if (matches.length === 0) {
        results.hidden = false;
        results.innerHTML = '<div class="topbar-search-empty">No player matches that search.</div>';
        return matches;
      }

      results.hidden = false;
      results.innerHTML = matches.map(createResultMarkup).join("");
      return matches;
    };

    input.addEventListener("input", () => {
      // Refresh matches while typing.
      renderMatches(input.value);
    });

    input.addEventListener("focus", () => {
      // Reopen the dropdown when a populated search box is focused.
      if (input.value.trim()) {
        renderMatches(input.value);
      }
    });

    form.addEventListener("submit", (event) => {
      // Jump to the top match on submit.
      const matches = renderMatches(input.value);

      if (matches.length === 0) {
        event.preventDefault();
        return;
      }

      event.preventDefault();
      window.location.href = buildHref(matches[0].path);
    });
  });

  document.addEventListener("click", (event) => {
    // Close open search dropdowns when clicking away.
    document.querySelectorAll("[data-global-player-search]").forEach((form) => {
      const results = form.querySelector(".topbar-search-results");
      if (!results) {
        return;
      }

      if (!form.contains(event.target)) {
        results.hidden = true;
      }
    });
  });
});
