# Changelog

## Unreleased

### Forecast

- A Forecast page (`/forecast`, `g f`): cash day by day over the next 1, 3 or 6 months, with
  the actual line up to the last import and the projection after it. Above it are the lowest
  point, what's left before the next salary, what's due before then and where the line ends.
  Below it are accounts that would go below zero, the upcoming payments with where each comes
  from, what-ifs (leave a payment out, add a one-off amount, keep to the budgets) drawn beside
  the baseline, and the assumptions in words. It is deterministic and shows no range.
- Recurring income and transfers are now found as well as bills: salary on payday, savings
  sweeps, the card bill, the household transfer (`Cashflow_core.Series`). A bill is projected at
  its new price once two payments agree.
- A `forecast` query in the API and as an MCP tool, loaded through a declared reporting effect
  like `cashflow_summary`.
- The Overview's "may run short before payday" card links to the forecast, and a property test
  checks that the two agree.
