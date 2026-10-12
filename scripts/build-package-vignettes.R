# Run from the repository root. Append to a staged site or create a standalone copy.
# The two package vignettes (vignettes/PhiSpace.Rmd and vignettes/PhiSpaceST.Rmd)
# download their data from Bioconductor (celldex, scRNAseq, STexampleData), so
# they need no input cache. R CMD check also builds them.
site_dir <- Sys.getenv("PHISPACE_SITE_DIR", "Test/site-package-vignettes")
if (!dir.exists(site_dir)) {
  pkgdown::init_site(pkg = ".", override = list(destination = site_dir))
} else if (!file.exists(file.path(site_dir, "pkgdown.yml"))) {
  stop("Existing destination is not a staged pkgdown website: ", site_dir)
}
site_dir <- normalizePath(site_dir, winslash = "/")

pkgload::load_all(".", quiet = TRUE)
for (name in c("PhiSpace", "PhiSpaceST")) {
  pkgdown::build_article(
    name, pkg = ".", lazy = FALSE, new_process = FALSE,
    override = list(destination = site_dir), quiet = FALSE
  )
  article <- file.path(site_dir, "articles", paste0(name, ".html"))
  if (!file.exists(article)) stop("Article not built: ", article)
}
file.create(file.path(site_dir, ".nojekyll"))
message("Built the package vignettes: ", file.path(site_dir, "articles"))
