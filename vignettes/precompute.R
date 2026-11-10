# Rebuild the precomputed vignette (needs the network, and pizzarr with the
# numcodecs shuffle/zlib codecs: the mdsumner/pizzarr icechunk-handoff branch).
#
#   cd icechunkr/vignettes && Rscript precompute.R
knitr::knit("icechunkr.Rmd.orig", "icechunkr.Rmd")
