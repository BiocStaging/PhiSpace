# Run from the repository root. Rebuild the function reference from man/ into
# a staged site, or create a new one.
site_dir <- Sys.getenv("PHISPACE_SITE_DIR", "Test/site-reference")
if (!dir.exists(site_dir)) {
  pkgdown::init_site(pkg = ".", override = list(destination = site_dir))
} else if (!file.exists(file.path(site_dir, "pkgdown.yml"))) {
  stop("Existing destination is not a staged pkgdown website: ", site_dir)
}
site_dir <- normalizePath(site_dir, winslash = "/")

# Remove the staged pages so that topics deleted from man/ do not survive.
unlink(file.path(site_dir, "reference"), recursive = TRUE)
pkgdown::build_reference(
  pkg = ".", lazy = FALSE, examples = FALSE,
  override = list(destination = site_dir)
)

# Every Rd topic must have a page, and the index must link to it.
rd <- tools::file_path_sans_ext(list.files("man", pattern = "\\.Rd$"))
pages <- file.path(site_dir, "reference", paste0(rd, ".html"))
if (!all(file.exists(pages))) {
  stop("Missing reference pages: ", paste(rd[!file.exists(pages)], collapse = ", "))
}
index <- readLines(file.path(site_dir, "reference", "index.html"), warn = FALSE)
if (length(grep("href=\"[^\"]+\\.html\"", index)) == 0) stop("Reference index has no topic links.")
invisible(file.create(file.path(site_dir, ".nojekyll")))
message("Built ", length(rd), " reference pages: ", file.path(site_dir, "reference"))
