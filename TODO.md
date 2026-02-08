# scholr TODO

## P-value formatting: current state (2026-02-07)

### What's in scholr now

| Function | Input | Output | File |
|----------|-------|--------|------|
| `p(model, var)` | model object | Plain text `= .032` or `< .001` | inline-helpers.R |
| `pval(x)` | raw numeric | LaTeX `$p < 0.05$` (categorical) | utils.R |
| `pv(x)` or `pv(model, var)` | **either** | LaTeX `$p$ < 0.05` (categorical) | inline-helpers.R |
| `bpv(model, var)` | model object | LaTeX `$b$ = 0.42, $p$ < 0.01` | inline-helpers.R |

Convention: plain text versions (`p`, `bp`) for Rmd; LaTeX versions (`pv`, `bpv`) for Rnw.

### Remaining migration in good_book

The book still uses homegrown `pval`, `pval2`, `pval3` defined inline in
`protests_book.Rnw` and `03_media_coverage.Rnw`. To migrate:

1. Add `library(scholr)` or `source()` the relevant scholr functions
2. Replace `pval3(x)` calls with `pv(x)` — same signature, handles tibble input
3. Replace `pval(model, num)` calls — these use positional index not var name,
   so they need minor refactoring to use `pv(model, "var_name")` instead
4. `pval2(x)` uses `car::recode()` — replace with `pv(x)`
5. Grep all `.Rnw` files for `pval`, `pval2`, `pval3` to find every call site

### Note on pval() vs pv()

`scholr::pval(x)` and `scholr::pv(x)` both exist. `pval` is the older categorical
formatter (raw numeric only). `pv` is the newer unified version that also accepts
models. Consider deprecating `pval()` in a future version once the book is migrated.
