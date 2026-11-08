# hledger-cal

`hledger cal` is a calendar-shaped view of an hledger journal for someone whose
money lives in several currencies. It answers "where did the money go this
year/quarter/month/week, valued in the currency I think in, and how much did
exchange-rate movement cost me?" Plain hledger can't show this at a glance:
`hledger bal -M -X NGN` gives the numbers, but not the calendar layout, the
per-day drill-down, the FX effect, or which rates are stale. Open `hledger cal`
and you see the current month laid out as a calendar, valued in your base
currency. You move between periods and views with the keyboard, and every
number on screen agrees with what `hledger bal` / `hledger reg` report for the
same query and valuation.

It is an hledger add-on (an executable named `hledger-cal` on your `PATH`),
built on hledger-lib 1.52 and brick.

## Install

```sh
cabal install --installdir ~/.local/bin --install-method=copy
```

Make sure `~/.local/bin` is on your `PATH`, then check that hledger finds the
add-on:

```sh
hledger cal --help
```

## Usage

```sh
hledger cal                          # current month, valued in the base currency
hledger cal -Y                       # this year
hledger cal -Q -p 2025               # quarters of 2025
hledger cal -M -p 2026-10 -X USD     # October 2026, valued in USD
hledger cal --commodities            # commodities view
hledger cal --print | less -R        # static output instead of the TUI
```

Options are parsed with hledger's own machinery, so `-f`, `LEDGER_FILE`,
`-p` period expressions and QUERY arguments (`acct:expenses`, `cur:USD`,
`not:desc:transfer`) behave as in hledger.

```
hledger cal [-Y|-Q|-M|-W] [--commodities]
            [-p PERIODEXPR | -b DATE -e DATE]
            [-f FILE]... [-X COMM | -V | --value=TYPE[,COMM] | --native]
            [--infer-market-prices | --no-infer-market-prices]
            [--forecast] [--stale-days N]
            [--print] [--width N] [--color=auto|always|never] [--ascii]
            [QUERY...]
```

When stdout is not a terminal (or with `--print`) the current view is printed
as static text. It needs at least 80 columns. `NO_COLOR` and `--color=never`
disable color; `--ascii` forces the ASCII glyph set.

## Keys

| Key | Action |
|---|---|
| `Y` `Q` `M` `W` `C` | Year, Quarter, Month, Week, Commodities view (they mirror `-Y -Q -M -W --commodities`) |
| `Tab` / `Shift-Tab` | Cycle views forward / back |
| `[` `]` | Previous / next period |
| `c` | Cycle the valued-in currency (base, ..., native) |
| `v` | Cycle rates at: period end, txn date, today |
| `/` | Edit the account/query filter (`Esc` cancels, empty clears) |
| `Enter` | Year/Quarter: drill down. Month/Week: register pane for the cursor day. Commodities: value in the selected commodity |
| `Esc` | Close the open pane (register, filter prompt, help); otherwise return from a drill-down |
| Arrow keys | Move the cursor (Month/Week day, Year month, Quarter quarter, Commodities row) |
| `g` | Reload the journal from disk, keeping view, focus, currency and filter |
| `?` | Help overlay listing every key for the current view |
| `q` | Quit |

Register pane: `Up`/`Down`/`PgUp`/`PgDn`/`Home`/`End` scroll; `Esc` or `Enter`
closes it.

## Valuation defaults

hledger-cal values every figure in a base commodity, and differs from plain
hledger in two ways:

- **Valuation is on by default.** Without `-X`/`-V`/`--value`, figures are
  valued in the base commodity: `-X COMM` if given, else the commodity of the
  `D` directive, else the commodity of the first posting. Each figure is valued
  at the end of the period it summarizes ("period end"). `v` switches to the
  posting date ("txn date") or today, and `--native` (or the `native` chip)
  keeps money in its own commodities, in which case ratios, mix bars, FX
  effect, rates and similar figures show `—`.
- **Price inference is on by default.** Prices implied by `@` / `@@` costs are
  used alongside `P` directives (hledger needs `--infer-market-prices` for
  that). Turn it off with `--no-infer-market-prices`. Rates older than
  `--stale-days` (default 30) at the valuation date are marked stale with `•`.

Amounts with no available price keep their native commodity and are shown after
the base figure; they are never dropped or treated as zero.

## Account types

Only postings to accounts of type Revenue (income) or Expense (expenses) feed
the totals, cells and registers; transfers between asset accounts are never
listed. Account types come from hledger itself: a `type:` tag on an `account`
directive, else inference from the account name.

```
account income         ; type: R
account expenses       ; type: X
account assets:bank    ; type: C
```

Income is shown positive, and net = income - expenses.

## Startup benchmark

```sh
cabal bench startup --benchmark-options='+RTS -N -RTS'
```

Generates a 10,000-transaction journal and times load + settings + a Month
render at 140 columns; it exits non-zero if that takes 1.0 s or more.
