# ---------------------------------------------------------------------------
# convert_tutorials.R -- migrate the bookdown tutorials to this Quarto book
# ---------------------------------------------------------------------------
#
# WHAT THIS IS FOR
#
# The 17 tutorials were written for bookdown and are ~7,500 lines of tested
# teaching prose. Rather than rewrite them (and lose pedagogy that works), this
# script does the mechanical part of the migration reproducibly, so it can be
# re-run if the source is revised. Judgement calls are then made by hand in the
# resulting .qmd files.
#
# It reads from ../rawdata_lab_repo (a read-only clone of the old repo) and
# writes .qmd files here. It never writes to the source.
#
# WHAT IT CHANGES, AND WHY
#
#  1. Bookdown callouts -> Quarto callouts. <div class="note"> becomes
#     ::: {.callout-note}, and so on.
#
#  2. Cross-references. In bookdown every chapter is merged into one HTML
#     document, so a bare [link](#anchor) resolves across chapters. In a Quarto
#     book each chapter is its own page, so those links break. The script builds
#     a map of every {#anchor} to the file that defines it, then rewrites
#     [link](#anchor) to [link](file.qmd#anchor).
#
#  3. Data loading. Students used to download each CSV from a GitHub URL. Now
#     the data ship with the biol202 package, so read_csv("<url>/x.csv") becomes
#     data(x). This removes the single largest source of week-1 failure (a typo
#     in a URL, or no internet), and it is why the package exists.
#
#  4. Package loading. library(tidyverse) becomes the individual packages
#     actually used, per the convention agreed for the course.
#
#  5. naniar. n_miss(x) becomes sum(is.na(x)) and n_complete(x) becomes
#     sum(!is.na(x)). naniar was loaded in 9 tutorials for these two functions
#     alone, and both are one-line base R.
#
#  6. Variable names. The bundled data use consistent snake_case, so references
#     like aspirinTreatment become aspirin_treatment.
#
#  7. Retired datasets and broken references get flagged for hand review.
#
# HOW TO RUN
#
#   Rscript convert/convert_tutorials.R
#
# It prints a report of anything needing hand attention.
# ---------------------------------------------------------------------------

src_dir <- normalizePath("../rawdata_lab_repo", mustWork = TRUE)
out_dir <- normalizePath(".", mustWork = TRUE)

# --- which source file becomes which chapter ------------------------------
file_map <- c(
  "01_Getting_started.Rmd"                = "01-getting-started.qmd",
  "02_ReproducibleR.Rmd"                  = "02-reproducible-r.qmd",
  "03_Preparing_formatting_assignments.Rmd" = "03-formatting-assignments.qmd",
  "04_Importing_Tidy_Data.Rmd"            = "04-importing-data.qmd",
  "05_visualize_single_variable.Rmd"      = "05-visualize-one-variable.qmd",
  "06-Describe_single_variable.Rmd"       = "06-describe-one-variable.qmd",
  "07_Visualize_two_variables.Rmd"        = "07-visualize-two-variables.qmd",
  "08_Sampling_Estimation_Uncertainty.Rmd" = "08-sampling-estimation.qmd",
  "09_Hypothesis_testing.Rmd"             = "09-hypothesis-testing.qmd",
  "10_single_categorical_variable.Rmd"    = "10-one-categorical.qmd",
  "11_two_categorical_variables.Rmd"      = "11-two-categorical.qmd",
  "12_single_numeric_variable.Rmd"        = "12-one-mean.qmd",
  "13_comparing_two_means.Rmd"            = "13-two-means.qmd",
  "14_assumptions_transformations.Rmd"    = "14-assumptions-transformations.qmd",
  "15_Comparing_more_than_2_means.Rmd"    = "15-anova.qmd",
  "16_Correlation.Rmd"                    = "16-correlation.qmd",
  "17_Least_squares_regression.Rmd"       = "17-regression.qmd",
  "96_Tables_markdown.Rmd"                = "90-tables.qmd",
  "95_gtsummary.Rmd"                      = "91-gtsummary.qmd",
  "98_visual_md_editor.Rmd"               = "92-visual-editor.qmd",
  "99_Common_issues.Rmd"                  = "99-common-issues.qmd"
)
# 94_load_all_packages.Rmd is deliberately NOT migrated: installing the biol202
# package now installs everything, so the chapter has no job left to do.

# --- variable renames (raw CSV header -> snake_case in the package) --------
var_renames <- c(
  "aspirinTreatment"     = "aspirin_treatment",
  "propFertile"          = "prop_fertile",
  "biomassRatio"         = "biomass_ratio",
  "nSpecies"             = "n_species",
  "biomassStability"     = "biomass_stability",
  "serotoninLevel"       = "serotonin_level",
  "treatmentTime"        = "treatment_time",
  "impressivenessScore"  = "impressiveness_score",
  "inbreedCoef"          = "inbreed_coef",
  "nPups"                = "n_pups",
  "BumpusNumber"         = "bumpus_number",
  "Number_of_siblings"   = "number_of_siblings",
  "Dominant_hand"        = "dominant_hand",
  "Dominant_foot"        = "dominant_foot",
  "Dominant_eye"         = "dominant_eye",
  "Antibody"             = "antibody",
  "Trout_Caught"         = "trout_caught"
)

# Object names students create. bird.malaria used a dot, which reads as an S3
# method to R and is inconsistent with every other name in the course.
obj_renames <- c("bird.malaria" = "bird_malaria")

retired <- c("caffeine", "chap11q01", "chap11q06", "chap12q01DeathAndTaxes",
             "chap12q17StalkieEyespan", "eelgrass2", "endangered_species",
             "perfume", "stalkflies")

issues <- list()
note_issue <- function(file, msg) {
  issues[[length(issues) + 1]] <<- paste0("  [", file, "] ", msg)
}

# --- pass 1: build the anchor -> chapter map ------------------------------
# Needed before any rewriting, because a link in chapter 3 may point at an
# anchor defined in chapter 11.
anchor_map <- list()
for (src in names(file_map)) {
  path <- file.path(src_dir, src)
  if (!file.exists(path)) next
  txt <- readLines(path, warn = FALSE)
  # Anchors appear on heading lines: ## Some heading {#the-anchor}
  hits <- regmatches(txt, gregexpr("[{]#[A-Za-z0-9_-]+[}]", txt))
  for (h in unlist(hits)) {
    id <- gsub("^[{]#|[}]$", "", h)
    anchor_map[[id]] <- file_map[[src]]
  }
}
cat("Anchors found:", length(anchor_map), "across", length(file_map), "chapters\n")

# --- helpers ---------------------------------------------------------------

convert_callouts <- function(txt) {
  # <div class="note">  -> ::: {.callout-note}
  # <div class="flag">  -> ::: {.callout-warning}  (things that trip students up)
  # <div class="advanced"> -> ::: {.callout-tip collapse="true"} (optional depth)
  txt <- gsub('<div class="note">', '::: {.callout-note}', txt, fixed = TRUE)
  txt <- gsub('<div class="flag">', '::: {.callout-warning}', txt, fixed = TRUE)
  txt <- gsub('<div class="advanced">',
              '::: {.callout-tip collapse="true"}', txt, fixed = TRUE)
  txt <- gsub("^</div>\\s*$", ":::", txt)
  txt
}

fix_crossrefs <- function(txt, this_file) {
  # Rewrite [text](#anchor) to [text](chapter.qmd#anchor) when the anchor lives
  # in another chapter. Same-chapter links are left alone.
  m <- gregexpr("\\]\\(#[A-Za-z0-9_-]+\\)", txt)
  regmatches(txt, m) <- lapply(regmatches(txt, m), function(links) {
    vapply(links, function(lnk) {
      id <- gsub("^\\]\\(#|\\)$", "", lnk)
      target <- anchor_map[[id]]
      if (is.null(target)) {
        note_issue(this_file, paste0("link to unknown anchor #", id))
        return(lnk)
      }
      if (identical(target, this_file)) return(lnk)
      paste0("](", target, "#", id, ")")
    }, character(1))
  })
  txt
}

fix_data_loading <- function(txt, this_file) {
  # read_csv("https://raw.githubusercontent.com/.../data/foo.csv") -> data(foo)
  pat <- '[a-zA-Z._0-9]+\\s*<-\\s*read_csv\\("https://raw\\.githubusercontent\\.com[^"]*/([a-zA-Z._0-9]+)\\.csv"\\)'
  for (i in seq_along(txt)) {
    m <- regexec(pat, txt[i])
    g <- regmatches(txt[i], m)[[1]]
    if (length(g) == 2) {
      ds <- g[2]
      ds_clean <- gsub("\\.", "_", ds)
      if (ds_clean %in% retired) {
        note_issue(this_file, paste0("uses RETIRED dataset '", ds, "' - needs replacing"))
      }
      txt[i] <- sub(pat, paste0("data(", ds_clean, ")"), txt[i])
    }
  }
  # Any remaining raw-GitHub URL is a case the pattern missed.
  bad <- grep("raw.githubusercontent", txt)
  for (b in bad) note_issue(this_file, paste0("line ", b, ": leftover GitHub URL"))
  txt
}

fix_packages <- function(txt, this_file) {
  # The old package-loading convention, replaced course-wide.
  txt <- gsub("^library\\(tidyverse\\)\\s*$",
              "library(dplyr)\nlibrary(ggplot2)\nlibrary(readr)", txt)
  # naniar is gone; its two used functions are base R one-liners.
  #
  # CAREFUL: this cannot be a naive swap of the function name. n_miss(x) has one
  # closing paren but sum(is.na(x)) needs two, so replacing only the opening
  # produces unbalanced, unparseable code. Each call site is rewritten whole.
  #
  # The idiom `n() - n_miss(x)` means "how many values are not missing", which
  # is more directly written sum(!is.na(x)) -- shorter, and it says what it
  # means, which matters more for novices than saving a character.
  txt <- gsub("n\\(\\)\\s*-\\s*(naniar::)?n_miss\\(([^()]*)\\)",
              "sum(!is.na(\\2))", txt)
  txt <- gsub("(naniar::)?n_miss\\(([^()]*)\\)", "sum(is.na(\\2))", txt)
  txt <- gsub("(naniar::)?n_complete\\(([^()]*)\\)", "sum(!is.na(\\2))", txt)
  txt <- txt[!grepl("^library\\(naniar\\)\\s*$", txt)]

  # A call with empty parens was the last step of a pipe, e.g.
  #   birds %>% select(type) %>% n_complete()
  # There is no safe textual rewrite for that -- it has to be restructured into
  # sum(!is.na(birds$type)) by hand -- so flag it rather than emit sum(!is.na()).
  if (any(grepl("sum\\(!?is\\.na\\(\\)", txt))) {
    note_issue(this_file,
               "naniar call at the end of a pipe - restructure BY HAND")
  }
  # ggmosaic was archived from CRAN; flag its uses for hand conversion.
  if (any(grepl("geom_mosaic|library\\(ggmosaic\\)", txt))) {
    note_issue(this_file, "uses ggmosaic - convert to geom_bar(position = 'fill') BY HAND")
  }
  txt
}

fix_names <- function(txt) {
  for (old in names(var_renames)) {
    txt <- gsub(paste0("\\b", old, "\\b"), var_renames[[old]], txt)
  }
  for (old in names(obj_renames)) {
    txt <- gsub(old, obj_renames[[old]], txt, fixed = TRUE)
  }
  txt
}

fix_images <- function(txt) {
  # Images moved from more/ to images/.
  txt <- gsub("\\./more/", "images/", txt)
  txt <- gsub("(?<![a-zA-Z])more/", "images/", txt, perl = TRUE)
  txt
}

strip_yaml <- function(txt) {
  # Source chapters have no YAML of their own (bookdown put it in index.Rmd),
  # but strip one if present so Quarto chapter headers stay clean.
  if (length(txt) && grepl("^---\\s*$", txt[1])) {
    close_at <- which(grepl("^---\\s*$", txt))[2]
    if (!is.na(close_at)) txt <- txt[(close_at + 1):length(txt)]
  }
  txt
}

check_broken_data <- function(txt, this_file) {
  # Two datasets are referenced by the tutorials but do not exist in the repo.
  for (bad in c("students2", "locusts")) {
    if (any(grepl(paste0("\\b", bad, "\\b"), txt))) {
      note_issue(this_file,
                 paste0("references '", bad, "', which does not exist - FIX BY HAND"))
    }
  }
  txt
}

# --- pass 2: convert -------------------------------------------------------
for (src in names(file_map)) {
  path <- file.path(src_dir, src)
  if (!file.exists(path)) {
    warning("missing source: ", src)
    next
  }
  dest <- file_map[[src]]

  txt <- readLines(path, warn = FALSE)
  txt <- strip_yaml(txt)
  txt <- convert_callouts(txt)
  txt <- fix_crossrefs(txt, dest)
  txt <- fix_data_loading(txt, dest)
  txt <- fix_packages(txt, dest)
  txt <- fix_names(txt)
  txt <- fix_images(txt)
  txt <- check_broken_data(txt, dest)

  writeLines(txt, file.path(out_dir, dest))
  cat("wrote", dest, "\n")
}

# --- report ----------------------------------------------------------------
cat("\n===================== NEEDS HAND ATTENTION =====================\n")
if (length(issues) == 0) {
  cat("  (none)\n")
} else {
  cat(paste(unlist(issues), collapse = "\n"), "\n")
}
cat("================================================================\n")
