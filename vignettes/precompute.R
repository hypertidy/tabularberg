# Rebuild the precomputed vignette (needs the network, and pizzarr with the
# numcodecs shuffle/zlib codecs: the mdsumner/pizzarr icechunk-handoff branch).
#
#   cd vignettes && Rscript precompute.R
knitr::knit("tabularberg.Rmd.orig", "tabularberg.Rmd")
