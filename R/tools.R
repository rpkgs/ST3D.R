#' @export
zip_id <- function(x, scale_old = 1e6, scale_new = 1e2) {
  doy <- floor(x / scale_old)
  id <- x - doy * scale_old
  doy * scale_new + id
}

#' @import data.table
#' @importFrom terra rast as.polygons
#' @importFrom sf st_as_sf st_sf
#' @export
as_poly <- function(d) {
  d <- d[, .(x, y, id)]
  if (nrow(d) <= 6) {
    # 修复: more than one unique y value needed
    r = rastFromXYZ(d)
  } else {
    r <- rast(d)
  }
  p <- as.polygons(r)
  st_as_sf(p)
}

#' dt_id2poly
#' 
#' @param df A data.table with the columns of `"date", "x", "y", "id", "value"`, 
#' `value` is the anomaly.
#' 
#' @importFrom sf st_sfc st_crs
#' @export
dt_id2poly <- function(df) {
  polys_raw <- df[, as_poly(.SD), .(date)]
  g <- do.call(st_sfc, polys_raw$geometry)
  polys <- st_sf(polys_raw[, 1:2], g, crs = st_crs(4326)) %>%
    mutate(id = zip_id(id) %>% as.factor())
  polys
}

st_area2 <- function(x) as.numeric(sf::st_area(x))

#' @export
make_date_doy <- function(year, doy) as.Date(sprintf("%d%03d", year, doy), "%Y%j")
