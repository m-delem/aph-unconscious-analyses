library(ggplot2)

# Custom pp_check function ------------------------------------------------
custom_pp_check <- function(fit, ndraws = 50) {
  brms::pp_check(
    fit, 
    ndraws = ndraws,
    type   = "dens_overlay_grouped",
    group  = "aphantasia"
  ) +
    scale_x_continuous(limits = c(0, 3), breaks = scales::breaks_pretty()) +
    labs(
      x = "Response Time (s)",
      y = "Density"
    ) +
    theme(
      axis.ticks.x       = element_line(linewidth = 0.5),
      panel.grid.major.x = element_line(linewidth = 0.5, color = "grey90"),
      panel.grid.minor.x = element_line(linewidth = 0.5, color = "grey95"),
      panel.spacing      = unit(3, "lines")
    )
}