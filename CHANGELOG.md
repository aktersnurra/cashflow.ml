# Changelog

## Unreleased

### Command bar

- Typing a question in the command palette (Ctrl+K or /) now offers "Ask the advisor: ..." after any page or action
  that matches. Choosing it opens a new personal conversation with your words as a draft; nothing is sent until you
  press Send.
- Not built yet: the advisor proposing changes you confirm (budgets, goals, categories, entries).

### Advisor memory

- Settings has a Memory section: short notes (up to 280 characters, 20 per person) that the advisor reads
  at the start of every personal conversation, such as "answer in whole kronor". Add, edit and forget; forgetting
  deletes the note at once. Notes are labelled data below the advisor's rules and never change a figure. Household
  conversations never see them. Line breaks and control characters can't be used to start a new prompt line.
- The advisor suggesting notes itself is not built yet.

### Navigation

- The sidebar shows Home, Advisor, Money, Plan and Household instead of every page. Money groups
  Transactions, Accounts and Cash flow; Plan groups Budgets, Forecast and Insights. Every page keeps
  its address, and a strip above the page switches between a section's pages. The phone tab bar is
  Home, Money, Import, Plan, Settings.

### Import

- An Import page (`/import`), the middle tab on phones in place of Add: one file box, which on a
  touch screen reads as a "Choose a bank file" button. Adding a transaction by hand is a link
  from there.
- The file says which account it is for. `identify_statement` reads its format, bank, account
  name and the last four digits of its number; the rows go into the account with those digits,
  or else into a new account named from the file, which `import_statement` (with `new_account`
  instead of `account_id`) opens with the bank's opening balance only when the import is
  confirmed. The name is asked for only when the file doesn't give one, and the account can be
  changed in the preview. An account imported into learns its number and bank from the file.
- A statement in a layout no format knows is read by the assistant, when one is configured: it
  sees the first rows with every digit masked and answers as structured output (OpenRouter
  `json_schema`, strict, only from endpoints that honour it) which column holds what and how
  dates are written. The rows are then read from the file, so no figure comes from the model.
  The answer is checked against the file and cached.
- A statement can be uploaded in the zip the bank downloads it in (Handelsbanken's download is an
  .xlsx named .zip); a spreadsheet without its workbook relationships still reads.

### Interface

- Starter categories follow the user's language: saving a language in Settings renames those
  that still have a starter name (personal and household ones), and so does signing in in the
  browser (personal ones). Categories someone renamed keep their names.
- Drop-downs draw the app's chevron and, where the browser supports customizable select, open as
  a themed list in light and dark.

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
