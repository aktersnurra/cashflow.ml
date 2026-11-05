# The accountant's review

Status: direction agreed with the user on 2026-11-03/04. All four steps are built. This extends
`2026-10-29-accountant-briefing-design.md`, whose first principles still hold.

## Why

The app must not be a generic chat app with some transaction logs. The relationship is with an accountant, who never
opens with "what would you like to know?". They open with the facts and what can be explored together.

## Structure

| Part | What it is |
| --- | --- |
| The review | What the accountant prepared for this pay cycle: the facts, what they found, what they need from you, and what you could explore together |
| Matters | One thing being worked through, with a status: open, decided or parked. All talking happens inside one |
| What we agreed | The record of decisions and their effect |
| The books | The evidence behind the figures: transactions, accounts, statements |

## Rules

1. The accountant always speaks first. No page opens on an empty prompt.
2. Topics come from the person's books. Each comes from a real finding and carries its figure; no canned questions.
3. Every figure opens its evidence. That is how transactions are reached, not through a log to browse.
4. Every matter ends somewhere: decided, parked, or with a next step.
5. The engine does the arithmetic, the model does the judgement, the person decides.

Typing freely is always possible, and what is typed opens a new matter. The words are "Review" / "Genomgång" and
"Matters" / "Ärenden". The voice is blunt: the verdict first, bad news once, no character.

## Steps

1. **The accountant speaks first** (no database change). A new conversation opens on the facts, what needs a decision
   and what can be looked at, all from the engine; the landing and canned starters go. The sidebar says Review and
   Matters.
2. **Matters.** Statuses on a conversation and the list in the sidebar; the vocabulary inside a conversation follows.
3. **What we agreed.** The record of decisions, tied to propose-then-confirm changes.
4. **Past reviews.** A dated review for each pay cycle.

## Step 1 as built

`/advisor` with no conversation shows the opening rather than redirecting to the Overview. That page also holds
consent and the composer, and it keeps its place when moving between pages. The opening is rendered from data already
reachable by the Overview's page component, so it resets with it at a session or membership boundary.

## Step 2 as built

A conversation has a status in `advisor_conversations.matter_state`: open, decided or parked. Existing conversations
start open.

- Saying where a matter stands changes neither its version nor its place in the list, as no turn was added. A new turn
  or a retry sets it back to open, so talking about a matter takes it up again.
- Once an answer is in, a row above the composer asks where it stands, or says it is decided or parked with a way to
  reopen it.
- The open list comes from its own request (`state=open`, five rows), shown in the sidebar and under "Still open" on the
  opening. It is separate from the conversation menu's list because opening a conversation empties that one. Its titles
  are private: they show only once access is known, and clear when it is lost.
- The conversation menu holds every matter, grouped Open, Parked, Decided, replacing the date groups.
- Arriving at Matters no longer resumes the latest conversation. It shows the opening, which names what is still open.
- Inside a conversation the words follow: "New matter", "Raise this". An answer no longer has an "Advisor" label, and
  the disclosure speaks in the first person. Settings still calls the feature Advisor.

## Step 3 as built

`/agreed`, "What we agreed" / "Det vi enats om", sits under Matters in the sidebar. It lists every matter that stands
decided or had a change applied, newest first, with what each change did, taken from the card that was applied.

- Applying a change settles its matter once no other proposal awaits an answer and no answer is still being written.
  Dismissing the last open proposal does the same if a change stands applied. A matter with nothing applied is only ever
  decided by the person saying so.
- Undoing, from the record or the conversation, sets a decided matter back to open. A parked one stays parked. The record
  keeps the change, marked undone with its date: it happened.
- A matter decided by word alone is on the record and says nothing changed in the books. Reopening it takes it off the
  record, as there is then nothing agreed.
- `advisor_conversations.decided_at` holds the date while a matter stands decided. Saying "decided" again keeps the first
  date, and a new turn clears it.
- Deleting a matter deletes its record, as a proposal has always been deleted with its conversation. Withdrawing consent
  hides the record without deleting it.
- "Their effect" means what changed and whether it still stands. The figures since, such as what has been spent against
  a budget that was agreed, are not here yet.

## Step 4 as built

A past review is an earlier cycle on the Review, chosen from the month picker, which now reaches every cycle loaded
(thirteen) rather than seven.

- It opens on the cycle's dates and a verdict from the engine's figures: what was saved or overspent, what came in and
  what went out. A cycle the statements don't reach the end of gets no verdict; it keeps its note under the figures.
- Under that: how many budgets held and which went over, then what was agreed in the cycle, each leading to its matter.
- Today's to-dos and what is ahead are left out, as they aren't about that month.
- It is worked out from the books as they stand now, not stored when the cycle ended. A statement imported late corrects
  it, and nothing has to run at the end of a cycle. So it doesn't preserve what the accountant said at the time.
- What was agreed goes by the UTC day it was last settled, against the cycle's dates, so a decision just after midnight
  on the first or last day of a cycle can fall in the neighbouring one.
- A past review has no address of its own: reloading returns to the current cycle.
