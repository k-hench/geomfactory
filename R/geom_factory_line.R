.onAttach <- function(libname, pkgname) {
  cli::rule( center = str_c("Welcome to", crayon::blue("geomfactory")))
}

# Helper to parse the aesthetics
#
# @keyword internal
prep_aes <- function(...){aes(...)}

#' Create an alternative color aesthetic
#'
#' \code{factory_geom_line} is the factory function for the geom_line.
#' Given the name of the new the new color aesthetic (eg. by \code{aes_name = test}),
#' it creates the new geom \code{geom_line_test(), a manual and a contiuous scale for the
#' new aesthetic (\code{scale_test_manual()} and \code{scale_test_continuous()}) as well as
#' a guide that can chandle the new aesthetic (\code{guide_colourbar_test()}).
#'
#'
#' @param aes_name string skalar, the name of the new color aesthetic.
#'
#' @examples
#' factory_geom_line('var2')
#'
#' @export
factory_geom_line <- function(aes_name){

geom_line_alt <<- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = get(str_c("GeomLine_",aes_name)),
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}


GeomLine_alt <<- ggproto(str_c("GeomLine_",aes_name), Geom,
                       #default_aes = aes(alt = 'black', size = 0.5, linetype = 1, alpha = NA),
                       default_aes = aes(size = 0.5, linetype = 1, alpha = NA),
                       required_aes = c("x","y",aes_name),
                       setup_data = function(data, params) {
                         data[order(data$PANEL, data$group, data$x), ]
                       },
                       draw_panel = function(data, panel_params, coord, arrow = NULL,
                                             lineend = "butt", linejoin = "round", linemitre = 10,
                                             na.rm = FALSE) {
                         if (!anyDuplicated(data$group)) {
                           message_wrap("geom_path: Each group consists of only one observation. ",
                                        "Do you need to adjust the group aesthetic?")
                         }

                         # must be sorted on group
                         data <- data[order(data$group), , drop = FALSE]
                         munched <- coord_munch(coord, data, panel_params)

                         # Silently drop lines with less than two points, preserving order
                         rows <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
                         munched <- munched[rows >= 2, ]
                         if (nrow(munched) < 2) return(zeroGrob())

                         # Work out whether we should use lines or segments
                         attr <- ggplot2:::dapply(munched, "group", function(df) {
                           linetype <- unique(df$linetype)
                           ggplot2:::new_data_frame(list(
                             solid = identical(linetype, 1) || identical(linetype, "solid"),
                             constant = nrow(unique(df[, c("alpha", aes_name,"size", "linetype")])) == 1
                           ), n = 1)
                         })
                         solid_lines <- all(attr$solid)
                         constant <- all(attr$constant)
                         if (!solid_lines && !constant) {
                           stop("geom_path: If you are using dotted or dashed lines",
                                ", colour, size and linetype must be constant over the line",
                                call. = FALSE)
                         }

                         # Work out grouping variables for grobs
                         n <- nrow(munched)
                         group_diff <- munched$group[-1] != munched$group[-n]
                         start <- c(TRUE, group_diff)
                         end <-   c(group_diff, TRUE)

                         if (!constant) {
                           grid::segmentsGrob(
                             munched$x[!end], munched$y[!end], munched$x[!start], munched$y[!start],
                             default.units = "native", arrow = arrow,
                             gp = grid::gpar(
                               col = alpha(munched[aes_name] %>% unlist() %>% unname(), munched$alpha)[!end],
                               fill = alpha(munched[aes_name] %>% unlist() %>% unname(), munched$alpha)[!end],
                               lwd = munched$size[!end] * .pt,
                               lty = munched$linetype[!end],
                               lineend = lineend,
                               linejoin = linejoin,
                               linemitre = linemitre
                             )
                           )
                         } else {
                           id <- match(munched$group, unique(munched$group))
                           grid::polylineGrob(
                             munched$x, munched$y, id = id,
                             default.units = "native", arrow = arrow,
                             gp = grid::gpar(
                               col = alpha(munched[aes_name] %>% unlist() %>% unname(), munched$alpha)[start],
                               fill = alpha(munched[aes_name] %>% unlist() %>% unname(), munched$alpha)[start],
                               lwd = munched$size[start] * .pt,
                               lty = munched$linetype[start],
                               lineend = lineend,
                               linejoin = linejoin,
                               linemitre = linemitre
                             )
                           )
                         }
                       },

                       draw_key = draw_key_path
)




scale_alt_manual <<- function (..., values, aesthetics = aes_name){
  manual_scale_alt(aes_name, values, ...)
}

scale_alt_continuous <<- function(..., low = "#084082ff", high = "#f0a830ff", space = "Lab",
                                 na.value = "grey50", guide = str_c("colourbar_",aes_name), aesthetics = aes_name) {
  ggplot2:::continuous_scale(aesthetics, "gradient", scales::seq_gradient_pal(low, high, space),
                   na.value = na.value, guide = guide, ...)
}


manual_scale_alt <- function(aesthetic, values = NULL, ...) {
  # check for missing `values` parameter, in lieu of providing
  # a default to all the different scale_*_manual() functions
  if (rlang::is_missing(values)) {
    values <- NULL
  } else {
    force(values)
  }

  pal <- function(n) {
    if (n > length(values)) {
      stop("Insufficient values in manual scale. ", n, " needed but only ",
           length(values), " provided.", call. = FALSE)
    }
    values
  }
  discrete_scale(aesthetic, "manual", pal,
                 guide =  guide_legend(override.aes = list(colour = values)), ...)

}

guide_colourbar_alt <<- function(

  # title
  title = waiver(),
  title.position = NULL,
  title.theme = NULL,
  title.hjust = NULL,
  title.vjust = NULL,

  # label
  label = TRUE,
  label.position = NULL,
  label.theme = NULL,
  label.hjust = NULL,
  label.vjust = NULL,

  # bar
  barwidth = NULL,
  barheight = NULL,
  nbin = 20,
  raster = TRUE,

  # frame
  frame.colour = NULL,
  frame.linewidth = 0.5,
  frame.linetype = 1,

  # ticks
  ticks = TRUE,
  ticks.colour = "white",
  ticks.linewidth = 0.5,
  draw.ulim= TRUE,
  draw.llim = TRUE,

  # general
  direction = NULL,
  default.unit = "line",
  reverse = FALSE,
  order = 0,
  available_aes = c("colour", "color", "fill", aes_name),

  ...) {

  if (!is.null(barwidth) && !grid::is.unit(barwidth)) barwidth <- unit(barwidth, default.unit)
  if (!is.null(barheight) && !grid::is.unit(barheight)) barheight <- unit(barheight, default.unit)

  structure(list(
    # title
    title = title,
    title.position = title.position,
    title.theme = title.theme,
    title.hjust = title.hjust,
    title.vjust = title.vjust,

    # label
    label = label,
    label.position = label.position,
    label.theme = label.theme,
    label.hjust = label.hjust,
    label.vjust = label.vjust,

    # bar
    barwidth = barwidth,
    barheight = barheight,
    nbin = nbin,
    raster = raster,

    # frame
    frame.colour = frame.colour,
    frame.linewidth = frame.linewidth,
    frame.linetype = frame.linetype,

    # ticks
    ticks = ticks,
    ticks.colour = ticks.colour,
    ticks.linewidth = ticks.linewidth,
    draw.ulim = draw.ulim,
    draw.llim = draw.llim,

    # general
    direction = direction,
    default.unit = default.unit,
    reverse = reverse,
    order = order,

    # parameter
    available_aes = available_aes,
    ...,
    name = str_c("colorbar_",aes_name)),
    class = c("guide", "colorbar")
  )
}

mv(from = "geom_line_alt", to = str_c('geom_line_',aes_name),envir = parent.frame())
mv(from = "GeomLine_alt", to = str_c("GeomLine_",aes_name),envir = parent.frame())
mv(from = "scale_alt_manual", to = str_c('scale_',aes_name,'_manual'),envir = parent.frame())
mv(from = "scale_alt_continuous", to = str_c('scale_',aes_name,'_continuous'),envir = parent.frame())
mv(from = "guide_colourbar_alt", to = str_c("guide_colourbar_",aes_name),envir = parent.frame())
}
