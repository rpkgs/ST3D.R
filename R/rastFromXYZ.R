check_xy <- function(x, digits = 6) {
  if (length(x) == 1) {
    # error("rast", "cannot create a raster geometry from a single x|y coordinate")
  }

  dx <- x[-1] - x[-length(x)]
  if (length(dx) == 0) return(Inf)

  rx <- min(dx) # 这里会产生warning
  for (i in 1:5) {
    rx <- rx / i
    q <- sum(round(dx / rx, digits = digits) %% 1)
    if (q == 0) {
      break
    }
  }

  if (q > 0) {
    stop("raster,matrix(xyz)", "x|y cell sizes are not regular")
  }
  rx
}

#' @importFrom terra rast as.polygons extend crop res ext `res<-` `ext<-` cellFromXY
#' ncell nlyr `values<-`
rastFromXYZ <- function(xyz, digits = 6, crs = "", extent = NULL) {
  ln <- colnames(xyz)
  if (is.null(ln)) ln <- rep("", ncol(xyz))
  if (any(nchar(ln) < 1)) ln <- make.names(ln)

  if (inherits(xyz, "data.frame")) {
    xyz <- as.matrix(xyz)
    xyz <- matrix(as.numeric(xyz), ncol = ncol(xyz), nrow = nrow(xyz))
  }

  x <- sort(unique(xyz[, 1]))
  y <- sort(unique(xyz[, 2]))
  rx <- check_xy(x, digits)
  ry <- check_xy(y, digits)

  if (is.infinite(ry)) ry = rx
  if (is.infinite(rx)) rx = ry

  minx <- min(x) - 0.5 * rx
  maxx <- max(x) + 0.5 * rx
  miny <- min(y) - 0.5 * ry
  maxy <- max(y) + 0.5 * ry
  d <- dim(xyz)

  r <- rast(
    xmin = minx, xmax = maxx, ymin = miny, ymax = maxy,
    crs = crs, nlyrs = d[2] - 2
  )

  res(r) <- c(rx, ry)
  ext(r) <- round(ext(r), digits + 2)
  cells <- cellFromXY(r, xyz[, 1:2])
  if (d[2] > 2) {
    names(r) <- ln[-c(1:2)]
    v <- try(matrix(NA, nrow = ncell(r), ncol = nlyr(r)))
    if (inherits(v, "try-error")) {
      # error(paste("cannot make matrix with ", ncell(r), " rows"))
    }
    v[cells, ] <- xyz[, -c(1:2)]
    values(r) <- v
  }

  if (!is.null(extent)) {
    r <- extend(r, extent)
    r <- crop(r, extent)
  }
  return(r)
}
