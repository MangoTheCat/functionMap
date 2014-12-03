##
## Copyright (c) 2010 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

horizonplot <- function(x, data, ...)
    UseMethod("horizonplot")

horizonplot.default <-
    function(x, data = NULL, ...,
             horizonscale = NA,
             origin = function(y) na.omit(y)[1],
             colorkey = FALSE, legend = NULL,
             panel = panel.horizonplot,
             prepanel = prepanel.horizonplot,
             col.regions = c("#B41414","#E03231","#F7A99C","#9FC8DC","#468CC8","#0165B3"),
             strip = FALSE, strip.left = TRUE,
             par.strip.text = list(cex = 0.6),
             colorkey.digits = 3,
             #layout = c(1, NA), ## TODO pending new lattice release
             groups = NULL,
             default.scales =
               list(y = list(relation = "free", axs = "i",
                             draw = FALSE, tick.number = 2)))
{
    if (!is.null(groups))
        stop("'groups' does not work in this plot")
    ans <- xyplot(x, data = data, ...,
                  origin = origin, horizonscale = horizonscale,
                  panel = panel, prepanel = prepanel,
                  col.regions = col.regions,
                  strip = strip, strip.left = strip.left,
                  par.strip.text = par.strip.text,
                  #layout = layout,
                  default.scales = default.scales)
    ans$call <- match.call()
    ## add colorkey
    if (isTRUE(colorkey)) {
        colorkey <- list()
    }
    if (is.list(colorkey)) {
        if (ans$y.scales$relation == "same") {
            origin <- ans$y.limits[1]
            horizonscale <- diff(ans$y.limits)
        }
        if (is.na(horizonscale)) {
            labels <- expression(
                - 3 * Delta[i], - 2 * Delta[i], - 1 * Delta[i], 0,
                + 1 * Delta[i], + 2 * Delta[i], + 3 * Delta[i], 0)
            if (is.numeric(origin)) {
                labels[4] <- origin
            } else {
                labels[4] <- "origin"
            }
        } else {
            if (is.numeric(origin)) {
                labels <- round(origin + (-3:3) * horizonscale, colorkey.digits)
            } else {
                labels <- paste(ifelse(-3:3>=0,"+","-"),
                                round(abs(-3:3) * horizonscale, colorkey.digits))
                labels[4] <- "origin"
            }
        }
        ii <- round((0:5 / 5) * (length(col.regions)-1)) + 1
        colorkey <-
            modifyList(list(col = col.regions[ii], at = -3:3,
                            labels = list(labels = labels, at = -3:3)),
                       colorkey)
        space <- colorkey$space
        if (is.null(space)) space <- "right"
        if (is.null(legend)) legend <- list()
        legend[[space]] <- list(fun = "draw.colorkey",
                                args = list(colorkey))
        ans <- update(ans, legend = legend)
    }
    ans
}


panel.horizonplot <-
    function(x, y, ..., border = NA,
             col.regions = c("#B41414","#E03231","#F7A99C","#9FC8DC","#468CC8","#0165B3"),
             origin) ## catch origin, don't pass to panel.xyarea!
{
    regions <- trellis.par.get("regions")
    origin <- current.panel.limits()$y[1]
    scale <- diff(current.panel.limits()$y)
    ## ordered for drawing, from least extreme to most extreme
    sections <- c(0, -1, 1, -2, 2, -3) ## these are the lower bounds
    ii <- round(((sections + 3) / 5) * (length(col.regions)-1)) + 1
    col <- col.regions[ii]
    for (i in seq_along(sections)) {
        section <- sections[i]
        yi <- y
        if (section < 0) {
            yi <- origin + origin - y
            section <- abs(section) - 1
        }
        baseline <- origin + section * scale
        if (all(yi <= baseline, na.rm = TRUE))
            next
        yi <- yi - baseline
        yi <- origin + pmax(pmin(yi, scale), 0)
        panel.xyarea(x, yi, border = border, col = col[i], col.line = col[i], ...)
    }
}

prepanel.horizonplot <-
    function(x, y, ..., horizonscale = NA,
             origin = function(y) na.omit(y)[1])
{
    if (is.function(origin))
        origin <- origin(y)
    ans <- prepanel.default.xyplot(x, y, ...)
    if (is.na(horizonscale))
        horizonscale <- max(abs(ans$ylim - origin)) / 3
    ans$ylim <- origin + c(0, horizonscale)
    ans
}
