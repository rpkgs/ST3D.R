#' @importFrom ggnewscale new_scale_fill
#' @import ggplot2
#' @import rcolors
plot_st3d <- function(dat, shp_bg, ylim = NULL) {
  polys <- dt_id2poly(dat)
  
  d_lab <- data.table(date = unique(dat$date)) %>% mutate(label = format(date))
  d_area <- cbind(sf::st_drop_geometry(polys[, 1:2]), area = st_area2(polys)) |>
    data.table() %>%
    .[, .(area = sum(area) / 1e6 / 1e6), date] %>%
    mutate(label = sprintf("%.2fM km^2", area))

  nbrk <- 20
  cols <- get_color("MPL_YlOrRd", nbrk) # %>% rev()
  cols <- get_color("amwg256", nbrk) # %>% rev()

  p = ggplot() +
    geom_raster(data = dat, aes(x, y, fill = value)) +
    # labels = scales::label_number(suffix = unit)
    scale_fill_gradientn2(na.value = "transparent", colors = cols) +
    labs(x = NULL, y = NULL, fill = "Anomaly") + 
    add_label(d_lab, 0, 1) +
    add_label(d_area, 0, 0, size = 3.5, color = "grey60") +
    new_scale_fill() +
    geom_sf(data = shp_bg, fill = NA, color = "grey60", size = 0.05) +
    geom_sf(
      data = polys, aes(color = id), linewidth = 0.4,
      fill = "transparent", show.legend = FALSE
    ) +
    geom_sf_text(
      data = polys, aes(label = id, color = id),
      color = "black",
      vjust = 1, size = 2, show.legend = FALSE
    ) +
    theme_bw() +
    theme(
      panel.spacing = unit(0, "cm"),
      plot.margin = margin(),
      legend.title = element_blank(),
      legend.margin = margin(l = -4), 
      legend.key.height = unit(2, "cm"),
      strip.text = element_blank(),
      panel.grid.major = element_line(linewidth = 0.1, linetype = 3, color = "grey90"),
      axis.ticks = element_blank(),
      axis.text = element_blank()
    ) +
    facet_wrap(~date, ncol = 6)
  if (!is.null(ylim)) p = p + scale_y_continuous(limits = ylim)
  p
}


#' add_label
#'
#' @param ... parameters to `gg.layers::geom_richtext_npc`
#'
#' @export
add_label <- function(d, npcx, npcy, ...) {
  gg.layers::geom_richtext_npc(
    data = d, aes(label = label, x = NULL, y = NULL),
    npcx = npcx, npcy = npcy, hjust = npcx, vjust = npcy, ...
  )
}
