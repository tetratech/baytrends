


# ===== Load devtools =====
setwd("C:/Users/jharc/Repositories/baytrends")
library(devtools)

# ===== Cleanly unload and remove old version if present =====
if ("baytrends" %in% .packages()) detach("package:baytrends", unload = TRUE)
if ("baytrends" %in% rownames(installed.packages())) remove.packages("baytrends")

# ===== Rebuild package documentation and content =====
# roxygen2::roxygenise(clean = TRUE)
devtools::document()            # Regenerate NAMESPACE and .Rd files
devtools::build_vignettes()     # Rebuild vignettes
devtools::test()                # Run unit tests

# ===== Run full R CMD check =====
devtools::check()               # Recommended before building binary (optional, but strongly advised)

# ===== Build binary and copy to Dropbox folder =====
install_file <- devtools::build(path = "C:/Users/jharc/", binary = TRUE)
file.copy(
  install_file,
  file.path("C:/Users/jharc/Dropbox/CBP/$ binary", basename(install_file)),
  overwrite = TRUE
)

# ===== Install binary locally =====
detach("package:baytrends", unload = TRUE)
install.packages(install_file, repos = NULL, type = "win.binary")

