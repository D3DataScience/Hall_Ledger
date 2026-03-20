// Builds the homepage featured-player grid from the site's induction pools.
(() => {
  // Homepage featured-player mount point.
  const grid = document.querySelector("[data-featured-player-grid]");
  if (!grid) return;

  // Curated featured-player pools for each induction lane.
  const featuredPools = {
    official: [
      {
        id: "ruthba01",
        label: "Official Induction",
        name: "Babe Ruth",
        href: "./players/ruthba01.html",
        image: "./images/players/ruthba01.jpg",
        stats: [
          ["fWAR", "179.4"],
          ["HR", "714"],
          ["RBI", "2,217"],
          ["Wins", "94"],
          ["IP", "1221.1"]
        ],
        chips: [["MVP", "MVP (1)"]]
      },
      {
        id: "aaronha01",
        label: "Official Induction",
        name: "Hank Aaron",
        href: "./players/aaronha01.html",
        image: "./images/players/aaronha01.jpg",
        stats: [
          ["fWAR", "136.5"],
          ["Hits", "3,771"],
          ["HR", "755"],
          ["RBI", "2,297"],
          ["BA", ".305"]
        ],
        chips: [["GG", "Gold Glove (3)"], ["MVP", "MVP (1)"]]
      },
      {
        id: "mayswi01",
        label: "Official Induction",
        name: "Willie Mays",
        href: "./players/mayswi01.html",
        image: "./images/players/mayswi01.jpg",
        stats: [
          ["fWAR", "149.8"],
          ["Hits", "3,283"],
          ["HR", "660"],
          ["RBI", "1,903"],
          ["BA", ".302"]
        ],
        chips: [["GG", "Gold Glove (12)"], ["MVP", "MVP (2)"]]
      },
      {
        id: "cobbty01",
        label: "Official Induction",
        name: "Ty Cobb",
        href: "./players/cobbty01.html",
        image: "./images/players/cobbty01.jpg",
        stats: [
          ["fWAR", "149.2"],
          ["Hits", "4,191"],
          ["HR", "117"],
          ["RBI", "1,937"],
          ["BA", ".366"]
        ],
        chips: [["MVP", "MVP (1)"]]
      },
      {
        id: "gehrilo01",
        label: "Official Induction",
        name: "Lou Gehrig",
        href: "./players/gehrilo01.html",
        image: "./images/players/gehrilo01.jpg",
        stats: [
          ["fWAR", "115.8"],
          ["Hits", "2,721"],
          ["HR", "493"],
          ["RBI", "1,995"],
          ["BA", ".340"]
        ],
        chips: [["MVP", "MVP (2)"]]
      },
      {
        id: "hornsro01",
        label: "Official Induction",
        name: "Rogers Hornsby",
        href: "./players/hornsro01.html",
        image: "./images/players/hornsro01.jpg",
        stats: [
          ["fWAR", "129.2"],
          ["Hits", "2,930"],
          ["HR", "301"],
          ["RBI", "1,584"],
          ["BA", ".358"]
        ],
        chips: [["MVP", "MVP (2)"]]
      }
    ],
    statistical: [
      {
        id: "bondsba01",
        label: "Statistical Induction",
        name: "Barry Bonds",
        href: "./milestone-players/bondsba01.html",
        image: "./images/players/bondsba01.jpg",
        stats: [
          ["fWAR", "164.5"],
          ["Hits", "2,935"],
          ["HR", "762"],
          ["RBI", "1,996"],
          ["BA", ".298"]
        ],
        chips: [["GG", "Gold Glove (8)"], ["MVP", "MVP (7)"], ["SS", "Silver Slugger (12)"]]
      },
      {
        id: "pujolal01",
        label: "Statistical Induction",
        name: "Albert Pujols",
        href: "./milestone-players/pujolal01.html",
        image: "./images/players/pujolal01.jpg",
        stats: [
          ["fWAR", "89.7"],
          ["Hits", "3,384"],
          ["HR", "703"],
          ["RBI", "2,218"],
          ["BA", ".296"]
        ],
        chips: [["GG", "Gold Glove (2)"], ["MVP", "MVP (3)"], ["SS", "Silver Slugger (6)"]]
      },
      {
        id: "clemero02",
        label: "Statistical Induction",
        name: "Roger Clemens",
        href: "./milestone-players/clemero02.html",
        image: "./images/players/clemero02.jpg",
        stats: [
          ["fWAR", "133.6"],
          ["Wins", "354"],
          ["SO", "4,672"],
          ["ERA", "3.12"],
          ["WHIP", "1.17"]
        ],
        chips: [["CY", "Cy Young (7)"], ["MVP", "MVP (1)"]]
      }
    ],
    active: [
      {
        id: "ohtansh01",
        label: "Active Induction",
        name: "Shohei Ohtani",
        href: "./active-players/ohtansh01.html",
        image: "./images/players/ohtansh01.jpg",
        stats: [
          ["fWAR", "49.7"],
          ["HR", "280"],
          ["RBI", "669"],
          ["Wins", "39"],
          ["IP", "528.2"]
        ],
        chips: [["MVP", "MVP (4)"], ["SS", "Silver Slugger (4)"]]
      },
      {
        id: "judgeaa01",
        label: "Active Induction",
        name: "Aaron Judge",
        href: "./active-players/judgeaa01.html",
        image: "./images/players/judgeaa01.jpg",
        stats: [
          ["fWAR", "61.6"],
          ["Hits", "1,205"],
          ["HR", "368"],
          ["RBI", "830"],
          ["BA", ".294"]
        ],
        chips: [["MVP", "MVP (3)"], ["SS", "Silver Slugger (5)"]]
      },
      {
        id: "troutmi01",
        label: "Active Induction",
        name: "Mike Trout",
        href: "./active-players/troutmi01.html",
        image: "./images/players/troutmi01.jpg",
        stats: [
          ["fWAR", "87.0"],
          ["Hits", "1,754"],
          ["HR", "404"],
          ["RBI", "1,018"],
          ["BA", ".294"]
        ],
        chips: [["MVP", "MVP (3)"], ["SS", "Silver Slugger (9)"]]
      },
      {
        id: "bettsmo01",
        label: "Active Induction",
        name: "Mookie Betts",
        href: "./active-players/bettsmo01.html",
        image: "./images/players/bettsmo01.jpg",
        stats: [
          ["fWAR", "62.5"],
          ["Hits", "1,767"],
          ["HR", "291"],
          ["RBI", "913"],
          ["BA", ".290"]
        ],
        chips: [["GG", "Gold Glove (6)"], ["MVP", "MVP (1)"], ["SS", "Silver Slugger (6)"]]
      }
    ],
    corrective: [
      {
        id: "loftoke01",
        label: "Corrective Induction",
        name: "Kenny Lofton",
        href: "./retired-players/loftoke01.html",
        image: "./images/players/loftoke01.jpg",
        stats: [
          ["fWAR", "62.6"],
          ["Hits", "2,428"],
          ["SB", "622"],
          ["R", "1,528"],
          ["BA", ".299"]
        ],
        chips: [["GG", "Gold Glove (4)"]]
      },
      {
        id: "grichbo01",
        label: "Corrective Induction",
        name: "Bobby Grich",
        href: "./retired-players/grichbo01.html",
        image: "./images/players/grichbo01.jpg",
        stats: [
          ["fWAR", "71.1"],
          ["Hits", "1,833"],
          ["HR", "224"],
          ["RBI", "864"],
          ["BA", ".266"]
        ],
        chips: [["GG", "Gold Glove (4)"], ["SS", "Silver Slugger (1)"]]
      },
      {
        id: "whitalo01",
        label: "Corrective Induction",
        name: "Lou Whitaker",
        href: "./retired-players/whitalo01.html",
        image: "./images/players/whitalo01.jpg",
        stats: [
          ["fWAR", "74.9"],
          ["Hits", "2,369"],
          ["HR", "244"],
          ["RBI", "1,084"],
          ["BA", ".276"]
        ],
        chips: [["GG", "Gold Glove (3)"], ["SS", "Silver Slugger (4)"]]
      }
    ]
  };

  let previousSet = {
    official: [],
    statistical: [],
    active: null,
    corrective: null
  };

  // Small helpers for featured-player selection.
  const randomIndex = length => Math.floor(Math.random() * length);

  function pickOne(pool, previousId = null) {
    if (!pool.length) return null;
    const filtered = pool.filter(player => player.id !== previousId);
    const source = filtered.length ? filtered : pool;
    return source[randomIndex(source.length)];
  }

  function pickMany(pool, count, previousIds = []) {
    const available = [...pool];
    const picks = [];

    while (picks.length < count && available.length > 0) {
      const preferred = available.filter(player => !previousIds.includes(player.id));
      const source = preferred.length ? preferred : available;
      const chosen = source[randomIndex(source.length)];
      picks.push(chosen);
      const removeIndex = available.findIndex(player => player.id === chosen.id);
      if (removeIndex >= 0) available.splice(removeIndex, 1);
    }

    return picks;
  }

  function renderChips(chips) {
    // Render the compact honor-chip row.
    return chips.map(([icon, text]) => (
      `<span class="honor-chip"><span class="honor-chip-icon">${icon}</span><span class="honor-chip-text">${text}</span></span>`
    )).join("");
  }

  function renderStats(stats) {
    // Render the small stat grid below the lead stat.
    return stats.map(([label, value]) => (
      `<div><dt>${label}</dt><dd>${value}</dd></div>`
    )).join("");
  }

  function splitFeaturedWar(stats) {
    // Split the lead fWAR stat from the supporting stat list.
    const warStat = stats.find(([label]) => label === "fWAR") || null;
    const remainingStats = stats.filter(([label]) => label !== "fWAR");
    return { warStat, remainingStats };
  }

  function renderCard(player, slotClass) {
    // Build one featured-player card for the homepage grid.
    const { warStat, remainingStats } = splitFeaturedWar(player.stats);
    const compactStats = remainingStats.slice(0, 3);
    const compactChips = player.chips;

    return `
      <article class="future-card featured-player-card ${slotClass}">
        <a class="featured-player-link" href="${player.href}">
          <img class="featured-player-image" src="${player.image}" alt="${player.name}">
        </a>
        <div class="featured-player-body">
          <p class="criteria-label">${player.label}</p>
          <h3><a href="${player.href}">${player.name}</a></h3>
          ${warStat ? `<p class="featured-player-war"><span class="featured-player-war-label">${warStat[0]}</span><span class="featured-player-war-value">${warStat[1]}</span></p>` : ""}
          <dl class="featured-player-stats">
            ${renderStats(compactStats)}
          </dl>
          ${compactChips.length ? `<div class="featured-player-chip-row">${renderChips(compactChips)}</div>` : ""}
        </div>
      </article>
    `;
  }

  function renderFeaturedPlayers() {
    // Pick one refresh set for each homepage slot.
    const officialPicks = pickMany(featuredPools.official, 2, previousSet.official);
    const statisticalPicks = pickMany(featuredPools.statistical, 2, previousSet.statistical);
    const activePick = pickOne(featuredPools.active, previousSet.active);
    const correctivePick = pickOne(featuredPools.corrective, previousSet.corrective);

    previousSet = {
      official: officialPicks.map(player => player.id),
      statistical: statisticalPicks.map(player => player.id),
      active: activePick ? activePick.id : null,
      corrective: correctivePick ? correctivePick.id : null
    };

    grid.innerHTML = [
      officialPicks[0] ? renderCard(officialPicks[0], "featured-slot-official-a") : "",
      officialPicks[1] ? renderCard(officialPicks[1], "featured-slot-official-b") : "",
      statisticalPicks[0] ? renderCard(statisticalPicks[0], "featured-slot-statistical-a") : "",
      statisticalPicks[1] ? renderCard(statisticalPicks[1], "featured-slot-statistical-b") : "",
      activePick ? renderCard(activePick, "featured-slot-active") : "",
      correctivePick ? renderCard(correctivePick, "featured-slot-corrective") : ""
    ].join("");
  }

  // Render the static-on-refresh homepage mix.
  renderFeaturedPlayers();
})();
