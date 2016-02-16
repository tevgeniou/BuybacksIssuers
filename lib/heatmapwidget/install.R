#https://cran.r-project.org/web/packages/htmlwidgets/vignettes/develop_intro.html
#installing

if(0) {
  devtools::create("heatmapwidget")               # create package using devtools
  setwd("heatmapwidget")                          # navigate to package dir
  htmlwidgets::scaffoldWidget("heatmapwidget")    # create widget scaffolding
}
library(heatmapwidget)
devtools::install()                        # install the package so we can try it
