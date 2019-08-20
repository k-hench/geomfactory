#' Create an alternative color aesthetic for ggforce::geom_shape
#'
#' \code{factory_geom_shape} is the factory function for the geom_shape form the ggforce package.
#' Given the name of the new the new color aesthetic (eg. by \code{aes_name = test}),
#' it creates the new geom \code{geom_shape_test(), a manual and a contiuous scale for the
#' new aesthetic for both color and fill  (\code{scale_test_c_manual()/
#' \code{scale_test_f_manual()} and \code{scale_test_c_continuous()}/ scale_test_f_continuous()})
#' as well as a guide that can chandle the new aesthetic (\code{guide_colourbar_test()}).
#'
#' The newly created aesthetics are then \code{test_c} (color) and \code{test_f} (fill).
#'
#' @param aes_name string skalar, the name of the new geom
#'
#' @examples
#' factory_geom_shape('var3')
#'
#' @export
factory_geom_shape <- function(aes_name){

  aes_c <- str_c(aes_name,'_c')
  aes_f <- str_c(aes_name,'_f')

  geom_shape_alt <<- function(mapping = NULL, data = NULL, stat = 'identity',
                         position = 'identity', expand = 0, radius = 0, ...,
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
    layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = get(str_c("GeomShape_",aes_name)),
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        expand = expand,
        radius = radius,
        ...
      )
    )
  }

  aes_defaults <- tibble( size = .5,
                          aes_c = rgb(0,0,0),
                          aes_f = '#D3D3D3',
                          alpha = NA) %>%
    set_names(.,nm = c('size', aes_c, aes_f, 'alpha'))

  GeomShape_alt <<- ggproto(str_c("GeomShape_",aes_name), GeomPolygon,
                            required_aes = c("x", "y"),
                            non_missing_aes = c(aes_c, aes_f),
                            default_aes = aes_defaults %>% purrr::pmap(prep_aes) %>% .[[1]],
                     draw_panel = function(data, panel_params, coord, expand = 0, radius = 0) {
                       n <- nrow(data)
                       if (n == 1 && expand == 0) {
                         return(zeroGrob())
                       }

                       munched <- coord_munch(coord, data, panel_params)
                       munched <- munched[order(munched$group), ]
                       if (!is.integer(munched$group)) {
                         munched$group <- match(munched$group, unique(munched$group))
                       }

                       # For gpar(), there is one entry per polygon (not one entry per point).
                       # We'll pull the first value from each group, and assume all these values
                       # are the same within each group.


                       first_idx <- !duplicated(munched$group)
                       first_rows <- munched[first_idx, ]
                       ggforce:::shapeGrob(munched$x, munched$y,
                                 default.units = 'native',
                                 id = munched$group, expand = expand, radius = radius,
                                 gp = grid::gpar(
                                   col = alpha(first_rows[aes_c] %>% unlist() %>% unname(), first_rows$alpha),
                                   fill = alpha(first_rows[aes_f] %>% unlist() %>% unname(), first_rows$alpha),
                                   lwd = first_rows$size * .pt,
                                   lty = first_rows$linetype
                                 )
                       )
                     },
                     extra_params = c('expand', 'radius')

  )

  scale_alt_c_manual <<- function (..., values, aesthetics = aes_c){
    manual_scale_alt(aesthetics, values, ...)
  }

  scale_alt_f_manual <<- function (..., values, aesthetics = aes_f){
    manual_scale_alt(aesthetics, values, ...)
  }

  scale_alt_c_continuous <<- function(..., low = "#084082ff", high = "#f0a830ff", space = "Lab",
                                      na.value = "grey50", guide = str_c("colourbar_",aes_name), aesthetics = aes_c) {
    ggplot2:::continuous_scale(aesthetics, "gradient", scales::seq_gradient_pal(low, high, space),
                               na.value = na.value, guide = guide, ...)
  }

  scale_alt_f_continuous <<- function(..., low = "#084082ff", high = "#f0a830ff", space = "Lab",
                                      na.value = "grey50", guide = str_c("colourbar_",aes_name), aesthetics = aes_f) {
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
    available_aes = c("colour", "color", "fill", str_c(aes_name,c('','_c','_f'))),

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

  mv(from = "geom_shape_alt", to = str_c('geom_shape_',aes_name),envir = parent.frame())
  mv(from = "GeomShape_alt", to = str_c("GeomShape_",aes_name),envir = parent.frame())
  mv(from = "scale_alt_c_manual", to = str_c('scale_',aes_name,'_c_manual'),envir = parent.frame())
  mv(from = "scale_alt_f_manual", to = str_c('scale_',aes_name,'_f_manual'),envir = parent.frame())
  mv(from = "scale_alt_c_continuous", to = str_c('scale_',aes_name,'_c_continuous'),envir = parent.frame())
  mv(from = "scale_alt_f_continuous", to = str_c('scale_',aes_name,'_f_continuous'),envir = parent.frame())
  mv(from = "guide_colourbar_alt", to = str_c("guide_colourbar_",aes_name),envir = parent.frame())
}

