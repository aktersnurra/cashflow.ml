# Changelog

## Unreleased

### Only what you need to see

- Insights names a price change from the charge before it, as Overview does: it compared the latest charge with the
  typical one, so it could say "went from SEK 849 to SEK 849". `recurring_payments` now includes `previous_amount`.
- "Your balance is growing" counts from the first month with money in an account, not from the empty months before
  it, which counted the whole balance as growth.
- Forecast shows the lowest point once when it falls on the day before salary, instead of the same number twice.
- Overview puts what needs doing first when something does (transactions to categorize, a statement due, a
  mismatch with the bank, an invitation, money running short before payday), above the month's figures. The chart of
  where the money went waits until most of the month's spending is categorized, instead of showing one Uncategorized
  band.
- Page subtitles that only restated the page's name are gone (Transactions, Accounts, Cash flow, Budgets, Forecast,
  Insights, Settings). Import, Add and Household keep theirs, which say something the title doesn't.
- Insights says "Not yet reconciled with the bank" once above its findings instead of on each, and no longer offers
  "Uncategorized accounts for 100% of your spending" as a finding.
- Accounts lists the categories money has gone through; the ones not used yet fold into one line you can open ("15
  categories not used yet"), instead of a row of dashes each.
- "Add transaction" and "Open account" are plain buttons rather than the page's main action: statements bring most
  rows in, and opening an account is rare.

### The advisor can propose a manual entry

- Ask the advisor to add a purchase, some income, or a move between your own accounts. It appears as a card with the
  accounts by the names you know, the exact amount, the date and whether it will post at once or wait for the bank
  statement to confirm it. Nothing is entered until you click Apply, which enters it exactly as adding it by hand does.
  Undo voids an entry still waiting for its statement or reverses a posted one, and leaves it alone once a statement has
  confirmed it or you have reversed or corrected it. Dates must be within the last year or the next month.
- To pick the right account the advisor is shown a name hint for each bank account: its name with every digit and
  asterisk removed ("Lönekonto ****1234" becomes "Lönekonto"), never a number; the same goes for the institution. Account names are otherwise not shown to it.

### Closing the inbox's category picker

- A tap or click outside the inbox's category, sharing and pairing pickers closes them without choosing anything. On a
  phone, which has no Escape key, there was no way out except picking a category.

### Logos from more websites

- A website link now finds the logo on more sites: icons a page declares without a size (IKEA, Circle K) are tried, and
  SVG logos are accepted (Lidl, Willys). An SVG is cleaned before it is stored, keeping only the drawing (no scripts,
  event handlers or links to anything outside the file), and served so that it can load and run nothing.

### The advisor can propose rules, goals and categories

- Besides budgets and filing transactions, the advisor can now propose a rule (a merchant text that suggests a category on
  import, never one that files by itself), a new savings goal, or a new expense or income category. Each appears as a card
  with exactly what would be created, and nothing exists until you click Apply. Each can be undone while it is still as it was
  made: a rule is removed, a goal is archived if you have not changed it, and a category is closed if nothing has used it yet.
  A goal or category that already exists by that name is not created twice.
- Not built yet: proposing a manual entry or editing an existing goal.

### Undo for advisor proposals

- A proposal you applied now has an Undo button. Undoing a budget puts back the budget that was there before (or
  removes it if there was none). Undoing a categorization puts the rows back in the inbox as they were. Undo only
  works while the change is still as the proposal left it: a budget you edited since, or a row you categorized
  again, is yours and is left alone, and the card says it can't be undone here. Undoing twice does nothing more.
- The advisor is told, on later turns, when a proposal it made was applied and then undone.

### Starter categories in each reader's language

- The starter categories (Groceries, Housing, Uncategorized...) are shown in each person's own language, and changing
  language no longer renames anything. Before, it renamed the household's shared categories too, so two members who
  read different languages kept switching them for each other. A category someone renamed shows what they typed, to
  everyone.
- The assistant's suggestions, the month review, the unusual-spending note and the advisor name categories in the
  reader's language as well. MCP sees them in English.
- Existing books need no migration: a starter category still named in Swedish counts as untouched.
- The inbox's category picker says what kind each choice is in your language ("Utgiftskategori", not "expense").

### Advisor can propose categories

- Ask the advisor to categorize your uncategorized transactions. It reads the waiting rows and your categories, then
  proposes where each of up to 20 rows should go, as a card listing every transaction, its amount and the category,
  with Apply and Dismiss. Nothing is filed until you click Apply, which files them exactly as the Inbox does. If any row
  changed since the proposal (you categorized it yourself, say), none is filed and the card says so; they are filed
  together or not at all. Money out can only go to an expense category and money in to an income one.
- Not built yet: proposing a new category or rule, goals and entries, and undoing an applied proposal.

### Command bar

- Typing a question in the command palette (Ctrl+K or /) now offers "Ask the advisor: ..." after any page or action
  that matches. Choosing it opens a new personal conversation with your words as a draft; nothing is sent until you
  press Send.
- Not built yet: the advisor proposing changes you confirm (budgets, goals, categories, entries).

### Automatic categories

- Settings has an Automatic categories section: for each bank account with statements, choose how sure the app must
  be (90%, 80% or 70%) before it files an imported row without asking. It covers categories learned from your history
  and the assistant's suggestions; rules you marked automatic work as before. It is off by default, per account, and
  rows below the bar wait in the inbox. Undoing the import takes automatically filed rows back.
- A suggestion is only filed automatically when its category has the bank account's owner and currency; otherwise it
  waits in the inbox. History suggestions reach at most 90% (and 70% needs two earlier choices); the assistant's reach
  at most 80%. Only an admin can change this for a household bank account.
- Not built yet: a "done, undo" list for individual automatically filed rows.

### Advisor proposals

- The advisor can now propose a monthly budget for a category it has shown you. It appears as a card with the
  category, the current and proposed amounts and the pay cycle it starts from, with Apply and Dismiss. Nothing
  changes until you click Apply, which sets the budget exactly as the Budgets page does. If the budget changed since
  the proposal, or it is older than 24 hours, it is not applied and the card says so. Clicking twice writes once.
- The advisor is told only that a proposal awaits your confirmation, never that it was done.
- Not built yet: proposing goals, categories or entries, and undoing an applied proposal.

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
