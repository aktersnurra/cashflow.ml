# The accountant's briefing

Status: direction agreed with the user on 2026-10-29 ("This is the correct course of the app"). Slices 1–3 are
built (slice 3 without relevance judging); slice 4 follows.

## 1. First principles

A person does not want budgeting, forecasting or budget-versus-actual. Those are an accountant's instruments. They
want what the accountant's judgement gives them:

1. **Am I OK?** Can I pay what is coming, and am I on track.
2. **Can I do X?** A decision about the future.
3. **Get to what I care about.** A buffer, a flat, a trip.
4. **Not get screwed.** A missing salary, a double charge, a price rise, a forgotten subscription.
5. **Spend as little time on it as possible.** Bookkeeping is the cost of 1–4, not the product.

The relationship the user wants is the one with a good accountant: *dump the statements and receipts on the desk,
and have them come back with the insights, the questions I would have asked already answered, and answer whatever
else I ask.*

Old personal-finance software (and much of this app until now) hands the person the accountant's tools — a
ledger, categories, budgets, rules, charts — and leaves the judgement to them. Most people give up, because the
upkeep is the product. An LLM makes the judgement cheap. The app is built around that, not around the tools with a
model bolted on to narrate them.

Rule: **the LLM does the judgement, a deterministic engine does the arithmetic, the person makes the decisions.**
What the person sees is answers and questions with numbers in them, not prose about a dashboard.

## 2. Which questions are worth answering (signal to noise)

A question about someone's money is worth answering unprompted when its answer is all of:

| Factor | Meaning | Measured as |
| --- | --- | --- |
| Material | Enough money, against *their* income | yearly stake ÷ yearly income, full marks at 5% |
| Surprising | Not what they would assume; beyond their own normal swing | per family: deviation over the usual spread, or new |
| Reliable | Enough history, a pattern rather than a point | per family: occurrences, months of history |
| Actionable | They could change it, or it changes a plan | per family: controllable spending high, rent low |
| New | Not shown and acknowledged, not muted | the person's answers (section 4) |

The score is the product, so a near-zero factor kills the question. That is what removes "you saved 4% more than
last month": immaterial, inside the normal swing, and nothing to do.

## 3. The questions (families)

Each family is computed by the engine with exact figures; the stable key names its subject, and includes rounded
figures where a material change should make it new again.

1. **Where does my money actually go?** Fixed (regular bills), flexible, one-off, over the last complete cycles;
   the top flexible categories per year.
2. **What am I paying for regularly?** Every regular bill as a yearly total; new ones; price rises.
3. **What is drifting?** A category above its earlier level three or more cycles running.
4. **Where is the leak?** Small frequent purchases at one merchant that add up over half a year.
5. **What do fees and interest cost me?** The year's total paid to banks and lenders.
6. **What does a normal month cost?** The baseline without one-offs, and how many months the cash covers it.
7. **What do I really save?** Savings per cycle without one-offs, against what the totals suggest.
8. **What is coming that I've forgotten?** A month that ran well above usual last year, coming up.
9. **When do I spend?** The week after payday against the rest, when the difference is strong.

Things that need doing (a missing salary, a double charge, a reconciliation, the inbox) stay in the attention
feed: they are to-dos, not insights.

## 4. Who does what

- **Engine (core, deterministic):** `Questions` turns each family into candidates with figures and the factor
  scores. Every number shown or said comes from here.
- **Briefing (io):** reads the books, runs the engine, applies what the person said (novelty), ranks, keeps the
  top few above a floor. A query, so the advisor can use it too.
- **Person:** each answer has one-tap follow-ups. *Got it* acknowledges that answer (a materially different one
  comes back). *Not useful* mutes the family. Anything else is typed in the chat.
- **LLM (slice 3):** proposes questions beyond the families from the person's merchants, aggregates and notes,
  and tests them with the engine's tools before they count (a hypothesis that does not hold is dropped silently);
  judges relevance from what it knows about the person; words each answer as a question title and one line around
  the number; answers follow-ups in chat with the same tools.

## 5. Surface

The Overview: safe to spend (am I OK), then the briefing (three to five answered questions), then the to-dos.
Each briefing card: the question as its title, the answer as one number and one line, follow-ups. It replaces the
month review card and the weekly brief's written summary.

## 6. Slices

1. Question engine with the families above, tested on scenarios including irregular uploads.
2. Briefing query and the Overview section, with *Got it* / *Not useful* feeding novelty.
3. LLM: hypothesis → verify with tools, relevance, wording; chat follow-ups from a card.
4. Learning: what is opened, acted on or muted adjusts the factors per family.

## 6a. Slice 3 as built

- `Questions.digest`: the last six complete cycles (income, spending), each of the twelve biggest categories per
  cycle, and the thirty merchants that cost the most (purchases, total, first and last date, whether a bill). With
  the keys of the questions already answered, this is all the model sees: merchants, categories and amounts.
- The model (strict JSON schema) proposes at most four questions, each with one check: `merchant_spend` (3, 6 or 12
  months), `category_change` (the last three cycles against the three before) or `new_merchant`. Its title must
  carry no digits.
- `Questions.verify` runs the check. A proposal is dropped when its figures don't hold up (fewer than two
  purchases; a change under 15% or twice the floor; a merchant not new) or when a family already covers the subject
  (the leak for that merchant, the drift for that category). What survives is scored like any other question.
- The reply is cached by a digest of what the model was shown; a failure is remembered for the same digest. The
  Overview loads the figures first and asks for proposals in a second request (`propose`), so the model never
  delays the page.
- Ask on a card opens the advisor with the question as its source: `resolve_finding` takes `question:` keys and the
  server states the question and its figures (`Briefing.describe`); the browser's copy is not trusted.

Not yet: the model judging the relevance of the families' answers (the score decides), and learning (slice 4).

## 7. Non-goals

No prose the person did not ask for. No number from the model that the engine did not compute. No question shown
because it can be computed rather than because it is worth knowing.
