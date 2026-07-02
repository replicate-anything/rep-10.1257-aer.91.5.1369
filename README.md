# Colonial Origins (Acemoglu, Johnson & Robinson 2001)

Folder-backed replication study for [The Colonial Origins of Comparative Development](https://doi.org/10.1257/aer.91.5.1369).

## Source materials

Original Stata code and data are from the MIT replication page:

https://econ-www.mit.edu/faculty/acemoglu/data/ajr2001

MIT analysis scripts live in `code/maketableN.do`. Thin runners in `code/tab_N.do` set paths and call them. R translations are `code/tab_N.R`.

## Layout

```
rep-10.1257-aer.91.5.1369/
  replication.yml       # 8 tables × 2 engines (R + Stata)
  data/                 # maketable1.dta … maketable8.dta
  code/
    tab_N.R             # R: make_tab_N() + format_tab_N()
    tab_N.do            # Stata runner (paths + log)
    maketableN.do       # original MIT analysis (path-patched)
    format_stata.R      # shared Stata log → HTML formatter
  artifacts/            # precomputed display HTML
```

Each table has two replication ids in `replication.yml`:

| Table | R id | Stata id | `group` |
|-------|------|----------|---------|
| 1–8 | `tab_N` | `tab_N_stata` | `tab_N` |

The `group` field ties the two engines together in Shiny (R / St buttons per row).

**Code tab:** Stata entries merge the runner (`code/tab_N.do`) with the substantive MIT script (`code/maketableN.do`) into a single copy-paste script with `SETUP` and `ANALYSIS` banners. R entries show only `code/tab_N.R`.

## Run locally

```r
options(
  replicateEverything.registry_root = "path/to/registry",
  replicateEverything.use_sibling_packages = TRUE,
  replicateEverything.study_folders_root = "path/to/monorepo"
)

# R engine
replicateEverything::run_replication("10.1257/aer.91.5.1369", "tab_2", format = TRUE)

# Stata engine (requires Stata)
replicateEverything::run_replication("10.1257/aer.91.5.1369", "tab_2_stata", format = TRUE)
```

## Notes

- Stata output logs go under `artifacts/staging/` (gitignored).
- Table 1 quartile bins may differ slightly from the paper (noted in the original MIT code).
- R IV specifications use `AER::ivreg`; Hausman tests in Table 8 use `lmtest::waldtest`.
