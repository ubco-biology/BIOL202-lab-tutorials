# ---------------------------------------------------------------------------
# smoke_test.R -- check that each chapter's R code actually runs
# ---------------------------------------------------------------------------
#
# `quarto render` is the real test, but it needs the Quarto CLI. This gives a
# fast check that does not: it pulls the R code out of each chapter with
# knitr::purl() and runs it in a fresh environment, reporting the first error
# per chapter.
#
# Chunks marked eval = FALSE (installation instructions, deliberate error
# demonstrations) are skipped by purl, which is what we want.
#
# Run:  Rscript convert/smoke_test.R
# ---------------------------------------------------------------------------

suppressMessages({
  library(knitr)
})

chapters <- sort(list.files(".", pattern = "^[0-9].*[.]qmd$"))
results <- data.frame(chapter = chapters, status = NA_character_,
                      detail = NA_character_, stringsAsFactors = FALSE)

tmpdir <- tempfile("purl")
dir.create(tmpdir)

for (i in seq_along(chapters)) {
  ch <- chapters[i]
  rfile <- file.path(tmpdir, sub("[.]qmd$", ".R", ch))

  ok <- tryCatch({
    suppressMessages(
      knitr::purl(ch, output = rfile, quiet = TRUE, documentation = 0)
    )
    TRUE
  }, error = function(e) {
    results$status[i] <<- "PURL FAIL"
    results$detail[i] <<- conditionMessage(e)
    FALSE
  })
  if (!ok) next

  # Run in a fresh environment so chapters cannot lean on each other's state --
  # every tutorial must stand on its own.
  env <- new.env(parent = globalenv())
  res <- tryCatch({
    suppressWarnings(suppressMessages(
      source(rfile, local = env, echo = FALSE)
    ))
    "OK"
  }, error = function(e) paste("ERROR:", conditionMessage(e)))

  results$status[i] <- if (identical(res, "OK")) "OK" else "FAIL"
  results$detail[i] <- if (identical(res, "OK")) "" else res
}

cat("\n=================== CHAPTER SMOKE TEST ===================\n")
for (i in seq_len(nrow(results))) {
  cat(sprintf("%-38s %s\n", results$chapter[i], results$status[i]))
  if (nzchar(results$detail[i]) && !is.na(results$detail[i])) {
    cat("    ", substr(results$detail[i], 1, 160), "\n")
  }
}
cat("==========================================================\n")
cat("OK:", sum(results$status == "OK", na.rm = TRUE),
    "/", nrow(results), "\n")
