#'  Heatmap widget based on original code with the following copyright:
#'  Copyright 2013, Satrapade
#'  by V. Kapartzianis
#'  Dual licensed under the MIT or GPL Version 2 licenses.
#'
#'  01/2016 Patched to handle NA's, widget E. Junqué de Fortuny
#'
#' @import htmlwidgets
#' @import htmltools
#' @import shiny
#'
#' @export
heatmapwidget <- function(data,  width = NULL, height = NULL) {
  # forward options using x
  params = list(
    data = data#,
    #options = list(..., height = height, width = width),
    #width = width , height = height
  )
  params = Filter(Negate(is.null), params)
  # create widget
  #htmlwidgets::createWidget(
  #  name = 'heatmapwidgetGenerate',
  #  params,
  #  width = width,
  #  height = height,
  #  package = 'heatmapwidget'
  #)
  heatmapwidgetGenerate(data)
}

#' Widget output function for use in Shiny
#'
#' @export
heatmapwidgetOutput <- function(outputId, width = '100%', height = '400px'){
  #shinyWidgetOutput(outputId, 'heatmapwidget', width, height, package = 'heatmapwidget')
  tableOutput(outputId, 'heatmapwidget', width, height, package = 'heatmapwidget')
}

#' Widget render function for use in Shiny
#'
#' @export
renderHeatmapwidget <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, heatmapwidgetOutput, env, quoted = TRUE)
}
heatmapwidgetGenerate <- function(data,style="norm", include.rownames = TRUE, include.colnames=TRUE, nsmall=2, border=0) {#these last ones are not used
  if(is.null(data)) {
    cat("NO data ...")
    return
  }else{
    cat(paste(data))
    cat(data[is.numeric(data)])
  }
  #vmax <- max(Filter(is.numeric, data))
  vmax = max(data[is.numeric(data) & !is.na(data)])
  sigmoid <- function(v) { return(1 / (1 + exp(-v))) }
  toRGB <- function(intensity, sign) {
    rgb <- "rgb("
    darkness <- floor((1 - intensity) * 256)
    if(is.na(sign))
      rgb <- paste(rgb,"255,255,255",sep="")
    else if(sign)
      rgb <- paste(rgb, darkness, ",255,", darkness, sep="")
    else
      rgb <- paste(rgb, "255,", darkness, ",", darkness, sep="")
    rgb <- paste(rgb, ")", sep="")
    return (rgb)
  }
  heatmap <- function(x, style="norm") {
    cat(paste("Heatmap",x,"\n"))
    if ("squashed" == style) {
      squashed <- sigmoid(x) - 0.5
      intensity <- abs(squashed) / 0.5
      rgb <- toRGB(intensity, squashed >= 0)
    } else { # ("norm" == style)
      intensity <- abs(x) / vmax
      rgb <- toRGB(intensity, x > 0)
    }
    return (paste("background-color:", rgb))
  }
  if (is.null(colnames(data))) colnames(data) <- 1:ncol(data)
  if (is.null(rownames(data))) rownames(data) <- 1:nrow(data)
  as.character(
    tags$table(
      border = border,
      class = 'data table table-bordered table-condensed',
      tagList({
        if (include.colnames)
          tags$thead(
            class = 'thead',
            tags$tr(
              tagList({
                if (include.rownames)
                  tags$th()
                else
                  list()
              }),
              lapply(colnames(data), function(name) {
                tags$th(name)
              })
            )
          )
        else
          list()
      }),
      tags$tbody(
        lapply(1:nrow(data), function(i) {
          tags$tr(
            tagList({
              if (include.rownames)
                tags$td(
                  align="right",
                  rownames(data)[i]
                )
              else
                list()
            }),
            lapply(1:ncol(data), function(j) {
              if (is.numeric(data[i,j]) & !is.na(data[i,j]))
                tags$td(
                  align="right",
                  style=heatmap(data[i,j], style),
                  format(data[i,j], nsmall=nsmall)
                )
              else if(!is.na(data[i,j]))
                tags$td(
                  as.character(data[i,j])
                )
              else
                tags$td(

                )
            })
          )
        })
      )
    )
  )
}

