pkgs <- c("shiny", "bslib", "DT", "DBI", "RSQLite", "openxlsx")
new  <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new)) install.packages(new)
cat("All packages ready.\n")
