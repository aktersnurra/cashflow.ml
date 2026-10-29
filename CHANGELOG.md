# Changelog

## Unreleased

### Your accountant's briefing

- **One desk.** The Overview opens with one box: *Ask, or drop statements*. A question goes to the advisor as a new
  conversation. Statements dropped anywhere on the page (or on the advisor page, or picked with the paperclip, several
  at once) are read, matched to their account and filed without a button when nothing needs you: the account is
  known, no row is ambiguous and the balance agrees. A new account or a doubtful row brings up its questions right
  there. The Import tab is gone; the advisor has its place in the phone's tab bar, and adding a transaction by hand
  is on the transactions page.
- The box stays out of the way until wanted: a grab bar above the tab bar to swipe up (down to put it away), a
  chevron at the foot of the page on a computer; dragging files over the page opens it. Both it and the advisor's
  composer are one line, like a chat app: "+" to add statements on the left, then the text, then the return key to
  send or a small square to stop on the right.
- **No inbox to work through.** Filing without asking is on for every bank account (at 80% sure; change or turn it
  off under Settings). After each import, and whenever rows are waiting, cashflow sweeps the rest in the
  background: what it is sure of is filed; a small row (under 300) it isn't sure of is filed with its best guess
  and marked as a guess, because its category changes nothing you'd act on. What is left and matters (big, or a
  merchant that recurs) becomes at most three questions on the Overview, one per merchant: one tap files every row
  from it and teaches the next import. A line says what the last sweep did.
- **Four places instead of thirteen pages:** Overview, Advisor, Money and Settings. The Overview takes what is
  ahead (the forecast chart and its key numbers, with payments, what-ifs and assumptions folded under it; goals and
  budgets). Money is Transactions and Accounts, with the inbox, an account and adding by hand inside them. The
  household is a section of Settings. Insights, Cash flow and Import are gone: the briefing, the Overview's money
  flow and the desk do their work. Old addresses lead to where things went.
- A question typed on the desk while the advisor wasn't on screen could land in the last conversation instead of
  a new one; it now waits until the advisor is showing, then starts its own.
- How far your statements reach now counts only the accounts you spend from: a savings account whose statement ends
  at its last monthly transfer no longer makes the whole month look unread.

- **Worth knowing** on the Overview: the questions about your money an accountant would answer before you ask,
  already answered with exact figures, best first, at most five. Why last month cost more (a yearly bill, a
  one-off, a habit); where the money goes (bills, what is yours to decide, one-offs, and the biggest categories);
  a category that has crept up and stayed; small purchases at one place that add up; what you pay regularly, with
  new bills and price rises; fees and interest; what a normal month costs and how long your cash covers it; what you
  really save without one-offs; a month coming up that cost more last year; spending in the week after payday.
- Each is ranked by whether it is worth your attention: the money at stake against your income, how far it is from
  your usual swing, how much history backs it, and whether you could do anything about it. Only complete months
  are read. **Got it** puts an answer away (a materially different one comes back); **Not useful** stops that kind
  of question. The advisor can read the same briefing (`get_briefing`).
- **The model asks too.** It reads a digest of your books (cycles, categories, the merchants that cost the most) and
  proposes up to four more questions, each tied to a check the app runs: what a merchant cost, how a category changed,
  a merchant that is new. Only those whose figures hold up, and that the briefing doesn't already answer, are shown,
  ranked with the rest; the question is the model's words, every figure the app's. It is asked once per set of
  figures, after the page has loaded.
- **Ask advisor** on any card opens the advisor on that question, under the card, with the question and its figures
  as the server states them.
- Spending that started from nothing no longer reads as "crept up".
- It replaces the weekly brief and its written summary, and the month review, large-purchase and price-change cards:
  Needs attention now holds only things to do. Design: `design/superpowers/specs/2026-10-29-accountant-briefing-design.md`.

### Things that need you, and what you can spend

- The Overview of a cycle in progress leads with **safe to spend until payday**: what day-to-day spending can still
  take with the bills still due and your goals' monthly amounts covered, counting usual spending for the days since
  your last upload. Per day, beside what a usual day costs; with no goal, a note says nothing is set aside.
- New cards for things in your statements that need a person, each saying what, how much and when, with what to
  do: a regular income or bill that should have arrived by now and hasn't (only judged as far as the statements
  reach), the same charge taken twice, the biggest purchase in half a year, and a yearly bill (insurance, car tax)
  due within a month, known from its last two payments. They stay quiet otherwise: a salary not imported yet isn't
  missing, and the same lunch three days running is a habit.
- Cards of the same urgency are ordered by the money they are about; the Overview shows the first three.
- The month review says why a finished month came out as it did, against the months before it: an ordinary month
  says so, otherwise the few things that made the difference, each with its amount and what kind of thing it was:
  a yearly bill (expected), a one-off purchase, a habit (and how many months running), less than usual somewhere,
  or a change in income. The written review is built on the same list, and the advisor can explain any of the last
  five finished months (`explain_month`).
- Cards learn from what you do with them: a missing payment you resolved stays quiet for that series, a double
  charge you said was right mutes that merchant, and after two large purchases you resolved those cards stop.
- The Overview reads at a glance: safe to spend, its per-day figure and days left, then where the cycle is heading
  (with how far off forecasts this far ahead have been, as ±) and the bills to come. The sum behind the number
  folds out under **Breakdown**; the range and the forecast's track record are on the ±. The advisor's period
  outlook carries the track record too.
- Cards say what, how much and when in a title and one line, in whole kronor. The month review is the month's
  spending beside a usual one, then what made the difference as rows tagged Yearly, One-off, Habit, Less or
  Income. The weekly brief is one row of figures and where the money went.

### Fixed

- The month review explained a month only for purchases posted straight to a category: rows filed from the
  inbox (how imports reach a category) all read as "more than usual", never as a yearly bill or a one-off.
  The review now shows on the Overview when it has something to explain, without repeating the figures.
- A yearly bill (or a purchase with nothing else bought in the half year before it) no longer shows as "your
  biggest purchase in half a year".
- A stray brace in the stylesheet dropped the desktop chat composer's own style.

### A month in progress, honestly

- Statements arrive when you upload them, so a pay cycle's figures run only as far as the imports reach. Each
  cycle now says how far that is (`covered_through`, the earliest last-imported day of your bank accounts and
  cards) and whether it is `complete`.
- The Overview no longer calls the cycle so far a result. Right after payday it leads with where the cycle is
  heading, "Expected by 24 Nov", from what you have so far, the income, bills and household contributions still
  due (listed under it) and your usual day-to-day spending for the days left, with the forecast's backtested range
  once there is a year of history. Below it: so far, through which day, and spending so far beside the same day of
  earlier cycles. Savings rate and "vs. 6-month average" wait until the cycle is complete.
- A `period_outlook` query (API and MCP) and advisor tool give the same. The advisor is told never to compare a
  cycle in progress with whole cycles.
- The month review waits until the statements cover the cycle to its end, and the weekly brief covers the last
  week the statements cover in full.

### Categorising by itself

- Filed once by you, a merchant is filed by itself after that: the other rows waiting from it at once, and later
  imports' without asking. Rows cashflow files by itself never teach it; a merchant you filed under different
  categories stays a suggestion.
- Categories are chosen by TypeSafe's Jev 1.14 (`openrouter.classifier_model`, `none` turns it off) through
  OpenRouter's decisions API, whose calibrated probability is the confidence. It is served by TypeSafe on an
  endpoint OpenRouter lists as zero-data-retention; requests ask for ZDR and no data collection, and an account-wide
  ZDR guardrail on the OpenRouter key enforces it. If Jev can't answer, the chat model does, capped at 75.
- Ask the advisor to categorise for you and it does: every waiting row cashflow is at least 80% sure of is filed at
  once, and the rest come back as a list to review with its best guess. Say you want it to continue and later
  imports are filed the same way.

### Phone

- While you type, the tab bar steps aside and the advisor's message box sits on the keyboard.
- No zooming: pinch and double-tap zoom are off, and form fields are large enough that iOS doesn't zoom into them.

### Danger zone: start over with your imports

- Settings has a Danger zone. "Reset my imported data" undoes every statement imported into your accounts, newest first,
  then deletes what they made: the transactions, their reversals (so also what earlier undos left behind), the inbox,
  the import list and the stored balances they changed, so statements can be uploaded again from scratch. You type
  `reset` to confirm. Your accounts, categories, rules, budgets, goals, opening balances and anything you entered by
  hand stay. A half of a move that lives in the household goes back to its inbox.
- The household's owner has the same for the household's accounts. A member cannot. It is not offered over MCP.
- Not built yet: removing a member, handing the household to someone else, and deleting an account.

### A move filed as spending is still a move

- A move between your own accounts only paired while both rows waited in the inbox. A row filed in a category first
  (by you, before the other account's statement was in or before pairing existed, or on import by a confident
  suggestion on an account that applies them) stayed spending, and its other half stayed income: the cash flow counted
  every such move twice. Pairing now also finds rows filed in an income or expense category, on import and with "Find
  transfers between my accounts", and moves them out of that category. A row shared with the household is left alone.
- A row filed on import without asking is kept as a filed inbox row, so it can pair, but doesn't teach later
  suggestions: only a category you chose does.
- Undoing the import of one half of a move files the other half where you had put it, instead of back in the inbox.

### A household belongs to its creator; no more global admin

- Anyone can create a household, and whoever does owns it. The owner invites members, renames the household, changes
  its pay cycle, adds or closes its shared bank accounts and turns on its Advisor. A member uses it (categories,
  entries, uploads) but doesn't run it. Before, a deployment-wide admin flag from the identity provider's
  `cashflow-admin` group decided all of that, so nobody else could even start a household.
- The `is_admin` column is dropped, and the app no longer asks the identity provider for the `groups` claim. An
  `admin_group` still in a config file is ignored. `create-session --admin` is gone.
- Everyone can now close their own accounts; that used to need the admin scope.

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

### An undone import leaves no trace

- Undoing an import reverses its rows in the ledger, and the account's list showed the original row, the reversal and,
  after uploading the file again, the new row: every transaction three times, though the balance was right. The list
  now shows an imported row once: a row that was undone, and the entry that undid it, are left out.
- The reports that look at single transactions saw them too. After an undo and a new upload, the forecast lost its bills
  and salary, recurring payments found none, and the largest transactions listed rows twice. A transfer reversed the same
  day, and its reversal, now cancel out before any report reads them. Totals (cash flow, spending by category, net
  worth) were right and are unchanged.

### The weekly brief covers households and what is coming

- Home's weekly brief follows the personal, household and family switch: the household's brief is about the household's
  own accounts, and a member's personal spending is not in it. It also says what recurring payments are expected in the
  next seven days. The sentences are written a few seconds after the figures stop changing, once per set of figures.

### The advisor can propose changing a savings goal

- Ask the advisor to raise a goal's target, move its date, change the monthly saving or rename it. The card names the goal
  and shows each new value beside the old one; nothing changes until you click Apply, and Undo puts the goal back unless
  you have changed it since. A change is refused when the goal is no longer as it was proposed against.
- To say which goal, the advisor is now shown your goals' names (one line each) with a reference. Until now it saw only
  their figures.
  
### A faster Accounts page

- The Accounts page no longer reads every transaction you have ever had to draw the net-worth figure: the database adds
  up each account's movements per day and only those sums are loaded. The account's own page shows its latest 50
  transactions, with a button for older ones, instead of 200 at once.
- Answers from the server are sent gzipped to browsers that accept it.

### The advisor no longer proposes a category you already have under another name

- A proposed category counts as a duplicate if it matches an existing one in any language: asking for "Mat & hushåll"
  when the starter Groceries is already there is refused, as "Groceries" is.

### A weekly brief on Home

- Home opens with last week (Monday to Sunday) in figures: income, spending, the week before and the three categories
  that took the most, with two or three sentences that a model writes from those figures alone. Whether there is a brief,
  and every number in it, is computed by code; a week with no income or spending has none. The sentences are written once
  for a given set of figures (an import that changes them writes them again) and never per transaction, and nothing is
  sent outside the app. Without a configured model the figures show on their own.

### OpenRouter

- Requests to OpenRouter now carry the app's public URL as `HTTP-Referer` next to the `cashflow` title, so usage shows
  up under the app in the OpenRouter dashboard.
- The advisor now runs on `anthropic/claude-sonnet-5.5` via Amazon Bedrock. `claude-sonnet-4.6` on the same route is
  still accepted.
- New `openrouter.advisor_model` setting: keep `model` on Haiku for categorising, pairing and summaries, and run only
  the advisor on Sonnet. Without it the advisor follows `model`, as before.

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

### Transfers between your accounts

- Importing a statement now pairs a move between your own accounts (a salary transfer to the household account, checking
  to savings) with its other half automatically, so it no longer counts as income in one account and spending in the
  other. Only clear matches pair by themselves: the same amount the other way, within three days, on another account,
  with no rival. When an assistant is configured it judges the unsure ones, and anything it isn't sure of stays in the
  inbox.
- New button in the inbox, "Find transfers between my accounts", does the same for everything already waiting there.
- Not built yet: rows you already filed under a category are not re-paired.

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
