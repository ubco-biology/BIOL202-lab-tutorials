# R Tutorials for Biostatistics — BIOL 202 Lab Companion

The lab companion for BIOL 202 (Introduction to Biostatistics), UBC Okanagan.
A Quarto book, built to mirror the course's OER (*Biostatistics for Biologists*).

## Building

Requires [Quarto](https://quarto.org) and the `biol202` package:

```r
remotes::install_github("ubco-biology/biol202_ubco")
```

Then:

```bash
quarto render
```

## Structure

| Path | What it is |
|---|---|
| `_quarto.yml` | Book config; chapter order mirrors the lecture sequence (redesign plan §9) |
| `index.qmd` | Welcome + one-time setup (installing `biol202`) |
| `01-`…`17-*.qmd` | The 17 tutorials |
| `90-`…`99-*.qmd` | Reference chapters |
| `csv_data/` | CSV copies of the cleaned datasets, for the URL-import lesson in Tutorial 4 only |
| `images/` | Figures |
| `convert/convert_tutorials.R` | Reproducible migration from the old bookdown repo |

## Where the data live

Datasets ship with the `biol202` package: `data(birds)`, and `?birds` for the
data dictionary. The CSVs in `data/` exist **only** so Tutorial 4 can teach
`read_csv()` against a real URL — every other tutorial uses `data()`.

## Licence

Content CC BY-NC-SA 4.0. See the package's `LICENSE-CONTENT.md` for third-party
data provenance (notably the Whitlock & Schluter datasets).
