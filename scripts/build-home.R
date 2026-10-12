# Run from the repository root. Rebuild the home page (index.html), the authors
# and citation page (authors.html) and 404.html from README.md,
# DESCRIPTION and inst/CITATION, and the articles index
# (articles/index.html) from _pkgdown.yml, and the sitemap of all staged
# pages, into a staged site. Run it last.
site_dir <- Sys.getenv("PHISPACE_SITE_DIR", "Test/site-home")
if (!dir.exists(site_dir)) {
  pkgdown::init_site(pkg = ".", override = list(destination = site_dir))
} else if (!file.exists(file.path(site_dir, "pkgdown.yml"))) {
  stop("Existing destination is not a staged pkgdown website: ", site_dir)
}
site_dir <- normalizePath(site_dir, winslash = "/")

pkgdown::build_home(pkg = ".", override = list(destination = site_dir), preview = FALSE)
# pkgdown renders every Markdown file at the root; the repository notes
# (AGENTS.md, CLAUDE.md, WEBSITE.md) are not website pages.
notes <- setdiff(list.files(".", pattern = "\\.md$"), c("README.md", "NEWS.md", "LICENSE.md"))
unlink(file.path(site_dir, sub("\\.md$", ".html", notes)))

# The citation page must list every DOI in inst/CITATION, and the home page
# must link to each of them.
cit <- utils::readCitationFile("inst/CITATION", meta = list(Encoding = "UTF-8"))
dois <- unlist(lapply(cit, function(x) x$doi))
if (length(dois) == 0) stop("inst/CITATION has no DOIs.")
for (page in c("authors.html", "index.html")) {
  html <- paste(readLines(file.path(site_dir, page), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  missing <- dois[!vapply(dois, grepl, logical(1), x = html, fixed = TRUE)]
  if (length(missing)) stop(page, " does not cite: ", paste(missing, collapse = ", "))
}

# Every article of _pkgdown.yml must have been built and listed in the index.
pkgdown::build_articles_index(pkg = ".", override = list(destination = site_dir))
cfg <- yaml::read_yaml("_pkgdown.yml")
articles <- unlist(lapply(cfg$articles, `[[`, "contents"))
# vignettes/articles/<name>.Rmd is written to articles/<name>.html
pages <- paste0(sub("^articles/", "", articles), ".html")
built <- file.exists(file.path(site_dir, "articles", pages))
if (!all(built)) stop("Articles not built: ", paste(articles[!built], collapse = ", "))
index <- paste(readLines(file.path(site_dir, "articles", "index.html"), warn = FALSE), collapse = "\n")
unlinked <- pages[!vapply(paste0("href=\"", pages, "\""), grepl, logical(1), x = index, fixed = TRUE)]
if (length(unlinked)) stop("Articles index does not link to: ", paste(unlinked, collapse = ", "))

# The sitemap lists every staged page.
pkgdown:::build_sitemap(pkgdown::as_pkgdown(".", override = list(destination = site_dir)))
message("Built home and citation pages citing ", length(dois), " DOIs, and the index of ",
        length(articles), " articles: ", site_dir)
