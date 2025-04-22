library(ggplot2)

# Plotting correlations ---------------------------------------------------
plot_correlations <- function(df, x, y){
  ggpubr::ggscatter(
    data  = df,
    x     = x,
    y     = y,
    color = "aphantasia",
    shape = "aphantasia",
    mean.point      = TRUE,
    mean.point.size = 5,
    star.plot       = TRUE,
    star.plot.lwd   = .08
  ) +
    geom_smooth(
      method    = "lm",
      formula   = y ~ x,
      color     = "black",
      linewidth = .8,
      alpha     = .2,
      fullrange = TRUE
    ) +
    scale_x_continuous(
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    see::scale_color_okabeito(
      name = "Group:  ", 
      labels = c(" Control   ", " Aphantasic")
    ) +
    scale_shape_manual(
      name = "Group:  ", 
      labels = c(" Control   ", " Aphantasic"),
      values = c(16, 17)
    ) 
}