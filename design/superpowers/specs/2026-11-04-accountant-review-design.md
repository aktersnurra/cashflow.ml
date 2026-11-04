# The accountant's review

Status: direction agreed with the user on 2026-11-03/04. Step 1 is being built; steps 2 to 4 follow. This extends
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
