# ---------------------------------------------------------------------------
# check_links.R -- verify every internal link in the rendered book resolves
# ---------------------------------------------------------------------------
#
# WHY THIS EXISTS
#
# The bookdown source relied on all chapters being merged into a single HTML
# page, so a bare link like [see here](#some_anchor) resolved no matter which
# chapter defined the anchor. A Quarto book renders one page per chapter, so
# those links must become file.qmd#some_anchor. The migration rewrote 197 of
# them automatically.
#
# A broken cross-reference does not fail the render -- it renders as a link that
# silently goes nowhere. So the only way to know the migration worked is to
# check every link against the anchors that actually exist in the output.
#
# Run after `quarto render`:  Rscript convert/check_links.R
# ---------------------------------------------------------------------------

book_dir <- "_book"
if (!dir.exists(book_dir)) stop("No _book/ directory -- run `quarto render` first.")

html_files <- list.files(book_dir, pattern = "[.]html$", full.names = TRUE)

# --- 1. collect every anchor id that exists in the rendered book -------------
anchors <- list()
for (f in html_files) {
  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
  ids <- unlist(regmatches(txt, gregexpr('id="[^"]+"', txt)))
  ids <- gsub('^id="|"$', "", ids)
  anchors[[basename(f)]] <- ids
}

# --- 2. collect every internal link and check it ----------------------------
broken <- list()
checked <- 0

for (f in html_files) {
  from <- basename(f)
  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
  links <- unlist(regmatches(txt, gregexpr('href="[^"]+"', txt)))
  links <- gsub('^href="|"$', "", links)

  for (lnk in links) {
    # Skip external links, mailto, and pure "#" placeholders.
    if (grepl("^(https?:|mailto:|#$)", lnk)) next
    if (!grepl("[.]html", lnk) && !grepl("^#", lnk)) next

    checked <- checked + 1

    if (grepl("^#", lnk)) {
      # same-page anchor
      target_file <- from
      anchor <- sub("^#", "", lnk)
    } else {
      parts <- strsplit(lnk, "#", fixed = TRUE)[[1]]
      target_file <- basename(parts[1])
      anchor <- if (length(parts) > 1) parts[2] else NA_character_
    }

    if (!target_file %in% names(anchors)) {
      broken[[length(broken) + 1]] <- sprintf("%s -> %s (FILE MISSING)", from, lnk)
      next
    }
    if (!is.na(anchor) && nzchar(anchor) && !anchor %in% anchors[[target_file]]) {
      broken[[length(broken) + 1]] <- sprintf("%s -> %s (ANCHOR MISSING)", from, lnk)
    }
  }
}

cat("\n==================== INTERNAL LINK CHECK ====================\n")
cat("internal links checked:", checked, "\n")
cat("broken:", length(broken), "\n\n")
if (length(broken)) {
  cat(paste(unlist(broken), collapse = "\n"), "\n")
} else {
  cat("  All internal links resolve.\n")
}
cat("============================================================\n")
