# Changelog

## Unreleased

### Navigation

- The sidebar shows Home, Advisor, Money, Plan and Household instead of every page. Money groups
  Transactions, Accounts and Cash flow; Plan groups Budgets, Forecast and Insights. Every page keeps
  its address, and a strip above the page switches between a section's pages. The phone tab bar is
  Home, Money, Add, Plan, Settings.

### Forecast

- A Forecast page (`/forecast`, `g f`): cash day by day over the next 1, 3 or 6 months, with
  the actual line up to the last import and the projection after it. Above it are the lowest
  point, what's left before the next salary, what's due before then and where the line ends.
  Below it are accounts that would go below zero, the upcoming payments with where each comes
  from, what-ifs (leave a payment out, add a one-off amount, keep to the budgets) drawn beside
  the baseline, and the assumptions in words.
- An 80% range around the forecast, measured by backtesting the same projection weekly over the
  last year (8 in 10 past forecasts landed inside it), with the chance of staying above zero.
  With more than a year of history, usual spending follows the season (a pricier December, a
  cheaper holiday month).
- Recurring income and transfers are now found as well as bills: salary on payday, savings
  sweeps, the card bill, the household transfer (`Cashflow_core.Series`). A bill is projected at
  its new price once two payments agree.
- A `forecast` query in the API and as an MCP tool, loaded through a declared reporting effect
  like `cashflow_summary`.
- The Overview's "may run short before payday" card links to the forecast, and a property test
  checks that the two agree.

### Advisor

- An Advisor page (`/advisor`): a chat about your own money that streams its answer as it is
  written. It reads your budgets, spending, recurring payments, goals and forecast through
  read-only tools; it cannot change anything. Answers cite the figures they used, and a
  finding on Home can be continued as a conversation. It replies in the language set in your
  settings and speaks in a plain, practical voice.
- Conversations are private, pinned to personal or household books and to one currency, and
  kept in a history menu under the title bar (titled by your first message, grouped by date,
  with delete). Data is shared with the model only after you turn the Advisor on, over a
  zero-data-retention provider route.
- The model sees income and expense category names, never bank account names or numbers, and
  every period comes with its first and last day.

### Deployment

- `web/build-bundle.sh <out-dir>` builds the web UI and writes exactly its deployable files,
  flat, into an empty directory, to be installed as `static_dir`. CI's web job uses it, so a
  deploy builds the bundle the same way. The bundle now includes the PNG icons that
  `index.html` and the manifest reference.
- `cashflow.opam` lists every library the server uses directly (`ppx_sexp_conv`, `sexplib`,
  `sexplib0`, `httpun`, `gluten`, `bigstringaf`), so `opam install . --deps-only` is enough
  on a fresh FreeBSD build jail.

### Fixes

- Visiting without a session shows the sign-in page again instead of "Something went wrong",
  and a failed command shows the server's message instead of the raw HTTP failure. The UI's
  requests now return the status and body of every response; only a request that got no
  response is an error. `web/e2e/signed_out.js` loads the page without a session.
