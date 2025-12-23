###############################################################################
# SAME ALIGNMENT — xlim and ylim identical, moderate scale
###############################################################################

set.seed(1)

A <- rnorm(400, 10, 20)
B <- rnorm(400, 10, 20)
C <- rnorm(400, 10, 20)
D <- rnorm(400, 10, 24)
H <- rnorm(800, 10, 20)

###############################################################################
# Choose a COMMON scale (between tight and full range)
###############################################################################
lim <- c(-10, 30)   # << moderate, between previous tight and full

###############################################################################
# Helpers (unchanged)
###############################################################################
bxp_obj <- function(x) {
  st <- boxplot.stats(x)$stats
  list(
    stats = matrix(st, nrow = 5, ncol = 1),
    n     = length(x),
    conf  = matrix(NA_real_, 2, 1),
    out   = numeric(0),
    group = 1,
    names = ""
  )
}

draw_v <- function(x, x_at, boxw = 1.4) {   # ← wider boxes
  bxp(
    bxp_obj(x),
    at        = x_at,
    add       = TRUE,
    axes      = FALSE,
    boxfill   = "grey85",
    lwd       = 3.5,   # thicker outline
    medlwd    = 6,     # thicker median
    whisklty  = 1,
    staplewex = 0,
    boxwex    = 6.5
  )
}

panel_start <- function() {
  plot.new()
  plot.window(xlim = lim, ylim = lim, xaxs = "i", yaxs = "i")
}

###############################################################################
# Layout (unchanged)
###############################################################################
par(mfrow = c(2, 2),
    mar = c(0, 0, 0, 0),
    oma = c(0, 0, 0, 0))

###############################################################################
# Top-left
###############################################################################
panel_start()
draw_v(B, x_at = 0)

###############################################################################
# Top-right
###############################################################################
panel_start()
draw_v(B, x_at = -5)
draw_v(B, x_at = 10)

###############################################################################
# Bottom-left
###############################################################################
panel_start()
draw_v(B, x_at = -6.5)
draw_v(B, x_at = 2)

###############################################################################
# Bottom-right (vertical + horizontal)
###############################################################################
panel_start()
draw_v(B, x_at = -6.5)

bxp(
  bxp_obj(H),
  horizontal = TRUE,
  at         = -8,
  add        = TRUE,
  axes       = FALSE,
  boxfill    = "grey85",
  lwd        = 2.2,
  medlwd     = 5,
  whisklty   = 1,
  staplewex  = 0,
  boxwex     = 6.5
)

###############################################################################
# Central divider (same logic, numeric coordinates)
###############################################################################
par(fig = c(0, 1, 0, 1), new = TRUE, mar = c(0, 0, 0, 0))
plot.new()
plot.window(xlim = lim, ylim = lim)
abline(v = mean(lim), h = mean(lim), lwd = 6, col = "grey40")
