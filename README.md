# Colonial Origins (Acemoglu, Johnson & Robinson 2001)

Folder-backed replication study for [The Colonial Origins of Comparative Development](https://doi.org/10.1257/aer.91.5.1369).

## Source materials

Original Stata code and data are from the MIT replication page:

https://econ-www.mit.edu/faculty/acemoglu/data/ajr2001

We preserve the MIT `.do` files under `stata/` and run them through thin wrappers in `code/tab_N.do`. R translations live alongside them as `code/tab_N.R`.

## Layout

```
rep-10.1257-aer.91.5.1369/
  replication.yml       # 8 tables × 2 engines (R + Stata)
  data/                 # maketable1.dta … maketable8.dta
  stata/                # original MIT scripts (path-patched)
  code/
    tab_N.R             # R: make_tab_N() + format_tab_N()
    tab_N.do            # Stata wrapper → stata/maketableN.do
    format_stata.R      # shared Stata log → HTML formatter
  artifacts/            # precomputed display HTML
```

Each table has two replication ids in `replication.yml`:

| Table | R id | Stata id | `group` |
|-------|------|----------|---------|
| 1–8 | `tab_N` | `tab_N_stata` | `tab_N` |

The `group` field ties the two engines together in Shiny (R / St buttons per row).

**Code tab:** Shiny shows one language at a time. Stata entries merge the runner (`code/tab_N.do`) with the substantive MIT script (`stata/maketableN.do`) into a single copy-paste script with `SETUP` and `ANALYSIS` banners. R entries show only `code/tab_N.R` (no Stata mixed in). R display helpers (`format_stata.R`) are infrastructure and are not shown in the Code tab.

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

- MIT scripts log to `maketableN.log`; wrappers write logs under `artifacts/staging/` (or writable staging on the server).
- Table 1 quartile bins may differ slightly from the paper (noted in the original MIT code).
- R IV specifications use `AER::ivreg`; Hausman tests in Table 8 are not yet ported to R.
