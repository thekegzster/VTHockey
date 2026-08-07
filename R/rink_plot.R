# ============================================================
# R/rink_plot.R
# Draws a top-down hockey rink using ggplot2.
# Coordinate system: standard NHL dimensions in feet.
#   x: 0 (left end boards) to 200 (right end boards)
#   y: 0 (bottom boards) to 85 (top boards)
#   VT attacks toward x = 200 (right side)
# ============================================================

library(ggplot2)

# ---- Helper: generate x/y points for a circle or arc ----
.arc_pts <- function(cx, cy, r, from = 0, to = 2 * pi, n = 200) {
  theta <- seq(from, to, length.out = n)
  data.frame(x = cx + r * cos(theta), y = cy + r * sin(theta))
}

# ---- Build the rink outline polygon (rounded corners) ----
.rink_outline <- function() {
  r <- 28  # corner radius in feet
  rbind(
    data.frame(x = r,       y = 0),
    data.frame(x = 200 - r, y = 0),
    .arc_pts(200 - r, r,       r, from = -pi / 2, to = 0,       n = 40),
    data.frame(x = 200, y = r),
    data.frame(x = 200, y = 85 - r),
    .arc_pts(200 - r, 85 - r, r, from = 0,       to = pi / 2,  n = 40),
    data.frame(x = 200 - r, y = 85),
    data.frame(x = r,        y = 85),
    .arc_pts(r,       85 - r, r, from = pi / 2,  to = pi,      n = 40),
    data.frame(x = 0, y = 85 - r),
    data.frame(x = 0, y = r),
    .arc_pts(r,       r,      r, from = pi,      to = 3 * pi / 2, n = 40),
    data.frame(x = r, y = 0)
  )
}

# ---- Main rink drawing function ----
# shots_df: optional data frame with columns x_coord, y_coord, result
#           result values: 'goal', 'save', 'missed', 'blocked'
# pending_x, pending_y: coordinates of the most recent click (shown as crosshair)

draw_rink <- function(shots_df = NULL, pending_x = NULL, pending_y = NULL,
                      attack_right = TRUE) {
  
  ICE   <- "#d6eef8"
  RED   <- "#c8102e"
  BLUE  <- "#003087"
  BLACK <- "#111111"
  
  outline <- .rink_outline()
  
  attack_right <- isTRUE(attack_right)  # coerce NA/NULL/length-0 to FALSE safely
  
  def_zone_label   <- if (attack_right) "DEF ZONE" else "OFF ZONE"
  off_zone_label   <- if (attack_right) "OFF ZONE" else "DEF ZONE"
  arrow_label      <- if (attack_right)
    "\u2192  VT attacks this way  \u2192"
  else
    "\u2190  VT attacks this way  \u2190"

  p <- ggplot() +
    # --- Ice surface ---
    geom_polygon(data = outline, aes(x = x, y = y),
                 fill = ICE, color = BLACK, linewidth = 1.2) +

    # --- Blue lines (2 ft wide each) ---
    geom_rect(aes(xmin = 75, xmax = 77, ymin = 0, ymax = 85),
              fill = BLUE, color = NA) +
    geom_rect(aes(xmin = 123, xmax = 125, ymin = 0, ymax = 85),
              fill = BLUE, color = NA) +

    # --- Center red line ---
    geom_segment(aes(x = 100, xend = 100, y = 0, yend = 85),
                 color = RED, linewidth = 2) +

    # --- Goal lines ---
    geom_segment(aes(x = 11, xend = 11, y = 11, yend = 74),
                 color = RED, linewidth = 1.5) +
    geom_segment(aes(x = 189, xend = 189, y = 11, yend = 74),
                 color = RED, linewidth = 1.5) +

    # --- Goal nets (6 ft wide, 4 ft deep) ---
    geom_rect(aes(xmin = 7,   xmax = 11,  ymin = 39.5, ymax = 45.5),
              fill = "white", color = RED, linewidth = 1) +
    geom_rect(aes(xmin = 189, xmax = 193, ymin = 39.5, ymax = 45.5),
              fill = "white", color = RED, linewidth = 1) +

    # --- Creases (semicircle in front of each net, radius 6 ft) ---
    geom_path(
      data = .arc_pts(11, 42.5, 6, from = -pi / 2, to = pi / 2),
      aes(x = x, y = y), color = BLUE, linewidth = 1
    ) +
    geom_path(
      data = .arc_pts(189, 42.5, 6, from = pi / 2, to = 3 * pi / 2),
      aes(x = x, y = y), color = BLUE, linewidth = 1
    ) +

    # --- Center ice circle & dot ---
    geom_path(data = .arc_pts(100, 42.5, 15), aes(x = x, y = y),
              color = RED, linewidth = 1) +
    geom_point(aes(x = 100, y = 42.5), color = RED, size = 3) +

    # --- End-zone faceoff circles ---
    # Left end (defensive zone for VT)
    geom_path(data = .arc_pts(31, 20.5, 15), aes(x = x, y = y),
              color = RED, linewidth = 1) +
    geom_path(data = .arc_pts(31, 64.5, 15), aes(x = x, y = y),
              color = RED, linewidth = 1) +
    geom_point(aes(x = 31, y = 20.5), color = RED, size = 3) +
    geom_point(aes(x = 31, y = 64.5), color = RED, size = 3) +
    # Right end (offensive zone for VT)
    geom_path(data = .arc_pts(169, 20.5, 15), aes(x = x, y = y),
              color = RED, linewidth = 1) +
    geom_path(data = .arc_pts(169, 64.5, 15), aes(x = x, y = y),
              color = RED, linewidth = 1) +
    geom_point(aes(x = 169, y = 20.5), color = RED, size = 3) +
    geom_point(aes(x = 169, y = 64.5), color = RED, size = 3) +

    # --- Neutral zone faceoff dots ---
    geom_point(aes(x = 76,  y = 20.5), color = RED, size = 2) +
    geom_point(aes(x = 76,  y = 64.5), color = RED, size = 2) +
    geom_point(aes(x = 124, y = 20.5), color = RED, size = 2) +
    geom_point(aes(x = 124, y = 64.5), color = RED, size = 2) +

    # --- Zone labels ---
    annotate("text", x = 43,  y = 88, label = def_zone_label,
             color = "#555555", size = 3, fontface = "italic") +
    annotate("text", x = 100, y = 88, label = "NEUTRAL",
             color = "#555555", size = 3, fontface = "italic") +
    annotate("text", x = 157, y = 88, label = off_zone_label,
             color = "#555555", size = 3, fontface = "italic") +
    annotate("text", x = 100, y = -5,
             label = arrow_label,
             color = "#861F41", size = 3.5, fontface = "bold") +

    coord_fixed(xlim = c(-8, 208), ylim = c(-12, 94)) +
    theme_void() +
    theme(
      plot.background  = element_rect(fill = "white", color = NA),
      legend.position  = "bottom",
      legend.title     = element_text(size = 11, face = "bold"),
      legend.text      = element_text(size = 10),
      plot.margin      = margin(4, 4, 4, 4)
    )
  
  # --- Danger zones layer ---
  zones <- build_danger_zones(res = 1)
  zones <- zones[zones$zone != "Low Danger", ]
  
  center_x <- 100
  
  zones_mirror <- zones
  zones_mirror$x <- 2 * center_x - zones$x
  
  zones_all <- dplyr::bind_rows(zones, zones_mirror)
  
  p <- p +
    geom_raster(
      data = zones_all,
      aes(x = x, y = y, fill = zone),
      alpha = 0.25,
      interpolate = TRUE
    ) +
    scale_fill_manual(
      values = c(
        "High Danger"   = "#d62020",
        "Medium Danger" = "#f5a623"
      ),
      name = "Danger Zone"
    )

  # --- Plot shots if provided ---
  if (!is.null(shots_df) && is.data.frame(shots_df) && nrow(shots_df) > 0) {

    shot_colors <- c(
      "goal"    = "#861F41",   # VT maroon
      "save"    = "#E5751F",   # VT orange
      "missed"  = "#888888",   # gray
      "blocked" = "#003087"    # blue
    )
    shot_shapes <- c(
      "goal"    = 8,    # asterisk / star
      "save"    = 16,   # filled circle
      "missed"  = 4,    # X
      "blocked" = 15    # filled square
    )

    p <- p +
      geom_point(
        data = shots_df,
        aes(x = x_coord, y = y_coord, color = result, shape = result),
        size = 4, stroke = 1.5, alpha = 0.85
      ) +
      scale_color_manual(values = shot_colors, name = "Shot result",
                         drop = FALSE) +
      scale_shape_manual(values = shot_shapes, name = "Shot result",
                         drop = FALSE)
  }

  # --- Show pending click location (yellow crosshair) ---
  if (!is.null(pending_x) && !is.null(pending_y)) {
    p <- p +
      geom_point(aes(x = pending_x, y = pending_y),
                 color = "gold", size = 7, shape = 10, stroke = 2)
  }

  p
}
