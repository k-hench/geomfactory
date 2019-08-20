#' Create an alternative color aesthetic for geom_rect
#'
#' \code{factory_geom_rect} is the factory function for geom_rect.
#' Given the name of the new the new color aesthetic (eg. by \code{aes_name = test}),
#' it creates the new geom \code{geom_rect_test(), a manual and a contiuous scale for the
#' new aesthetic for both color and fill  (\code{scale_test_c_manual()/
#' \code{scale_test_f_manual()} and \code{scale_test_c_continuous()}/ scale_test_f_continuous()})
#' as well as a guide that can chandle the new aesthetic (\code{guide_colourbar_test()}).
#'
#' The newly created aesthetics are then \code{test_c} (color) and \code{test_f} (fill).
#'
#' @param aes_name string skalar, the name of the new geom
#'
#' @examples
#' factory_geom_rect('var3')
#'
#' @importFrom grid grobTree
#' @export
factory_geom_rect <- function(aes_name){

  aes_c <- str_c(aes_name,'_c')
  aes_f <- str_c(aes_name,'_f')

  geom_rect_alt <<- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        linejoin = "mitre",
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
    layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = get(str_c("GeomRect_",aes_name)),
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        linejoin = linejoin,
        na.rm = na.rm,
        ...
      )
    )
  }

  aes_defaults <- tibble( size = .5,
                          aes_c = rgb(0,0,0),
                          aes_f = '#D3D3D3',
                          linetype = 1,
                          alpha = NA) %>%
    set_names(.,nm = c('size', aes_c, aes_f,'linetype', 'alpha'))

  GeomRect_alt <<- ggproto(str_c("GeomRect_",aes_name), Geom,
                      default_aes = aes(colour = NA, fill = "grey35", size = 0.5,
                                        alpha = NA),

                      required_aes = c("xmin", "xmax", "ymin", "ymax"),
                      non_missing_aes = c(aes_c, aes_f),
                      default_aes = aes_defaults %>% purrr::pmap(prep_aes) %>% .[[1]],

                      draw_panel = function(self, data, panel_params, coord, linejoin = "mitre") {
                        if (!coord$is_linear()) {
                          aesthetics <- setdiff(
                            names(data), c("x", "y", "xmin", "xmax", "ymin", "ymax")
                          )
                          print(names(data))
                          print(aesthetics)

                          polys <- lapply(split(data, seq_len(nrow(data))), function(row) {
                            poly <- ggplot2:::rect_to_poly(row$xmin, row$xmax, row$ymin, row$ymax)
                            aes <- ggplot2:::new_data_frame(row[aesthetics])[rep(1,5), ]

                            draw_panel_non_linear(cbind(poly, aes),aes_name, panel_params, coord)
                          })

                          ggplot2:::ggname("bar", do.call("grobTree", polys))
                        } else {
                          coords <- coord$transform(data, panel_params)
                          ggplot2:::ggname("geom_rect", grid::rectGrob(
                            coords$xmin, coords$ymax,
                            width = coords$xmax - coords$xmin,
                            height = coords$ymax - coords$ymin,
                            default.units = "native",
                            just = c("left", "top"),
                            gp = grid::gpar(
                              col = alpha(coords[aes_c] %>% unlist() %>% unname(), coords$alpha),
                              fill = alpha(coords[aes_f] %>% unlist() %>% unname(), coords$alpha),
                              lwd = coords$size * .pt,
                              lty = coords$linetype,
                              linejoin = linejoin,
                              # `lineend` is a workaround for Windows and intentionally kept unexposed
                              # as an argument. (c.f. https://github.com/tidyverse/ggplot2/issues/3037#issuecomment-457504667)
                              lineend = if (identical(linejoin, "round")) "round" else "square"
                            )
                          ))
                        }
                      },

                      draw_key = draw_key_polygon
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

  mv(from = "geom_rect_alt", to = str_c('geom_rect_',aes_name),envir = parent.frame())
  mv(from = "GeomRect_alt", to = str_c("GeomRect_",aes_name),envir = parent.frame())
  mv(from = "scale_alt_c_manual", to = str_c('scale_',aes_name,'_c_manual'),envir = parent.frame())
  mv(from = "scale_alt_f_manual", to = str_c('scale_',aes_name,'_f_manual'),envir = parent.frame())
  mv(from = "scale_alt_c_continuous", to = str_c('scale_',aes_name,'_c_continuous'),envir = parent.frame())
  mv(from = "scale_alt_f_continuous", to = str_c('scale_',aes_name,'_f_continuous'),envir = parent.frame())
  mv(from = "guide_colourbar_alt", to = str_c("guide_colourbar_",aes_name),envir = parent.frame())
}

draw_panel_non_linear <- function(data,aes_name, panel_params, coord, rule = "evenodd") {
  n <- nrow(data)
  aes_c <- str_c(aes_name,'_c')
  aes_f <- str_c(aes_name,'_f')

  if (n == 1) return(zeroGrob())

  munched <- coord_munch(coord, data, panel_params)

  if (is.null(munched$subgroup)) {
    # Sort by group to make sure that colors, fill, etc. come in same order
    munched <- munched[order(munched$group), ]

    # For gpar(), there is one entry per polygon (not one entry per point).
    # We'll pull the first value from each group, and assume all these values
    # are the same within each group.
    first_idx <- !duplicated(munched$group)
    first_rows <- munched[first_idx, ]

    ggplot2:::ggname(
      "geom_polygon",
      grid::polygonGrob(
        munched$x, munched$y, default.units = "native",
        id = munched$group,
        gp = grid::gpar(
          col = alpha(first_rows[aes_c] %>% unlist() %>% unname(), first_rows$alpha),
          fill = alpha(first_rows[aes_f] %>% unlist() %>% unname(), first_rows$alpha),
          lwd = first_rows$size * .pt,
          lty = first_rows$linetype
        )
      )
    )
  } else {
    if (utils::packageVersion('grid') < "3.6") {
      stop("Polygons with holes requires R 3.6 or above", call. = FALSE)
    }
    # Sort by group to make sure that colors, fill, etc. come in same order
    munched <- munched[order(munched$group, munched$subgroup), ]
    id <- match(munched$subgroup, unique(munched$subgroup))

    # For gpar(), there is one entry per polygon (not one entry per point).
    # We'll pull the first value from each group, and assume all these values
    # are the same within each group.
    first_idx <- !duplicated(munched$group)
    first_rows <- munched[first_idx, ]

    ggplot2:::ggname(
      "geom_polygon",
      grid::pathGrob(
        munched$x, munched$y, default.units = "native",
        id = id, pathId = munched$group,
        rule = rule,
        gp = grid::gpar(
          col = alpha(first_rows[aes_c] %>% unlist() %>% unname(), first_rows$alpha),
          fill = alpha(first_rows[aes_f] %>% unlist() %>% unname(), first_rows$alpha),
          lwd = first_rows$size * .pt,
          lty = first_rows$linetype
        )
      )
    )
  }

}
