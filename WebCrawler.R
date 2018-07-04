library(Rcrawler)

selector <- ".left.fullWidth:not(.left.fullWidth.upper.fontColorOne)"

Rcrawler(Website = "https://www.finextra.com/", ExtractCSSPat = selector, DIR = "/pages", no_cores = 4, no_conn = 4)