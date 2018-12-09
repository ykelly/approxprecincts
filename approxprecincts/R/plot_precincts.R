#' Produces a simple plot of census blocks in the specified county and colors them based on precinct classification.
#'
#' @param x Voterfile dataframe, output from classify_nn()
#' @param county String containing name of county to plot
#'
#'
#' @export

plot_precincts <- function(x, county){
  requireNamespace("ggplot2", quietly = TRUE)

  palette <- randomcoloR::distinctColorPalette(length(unique(x$PRECINCT_NAME)))

  ggplot2::ggplot(x) +
    ggplot2::geom_sf(aes(fill = PRECINCT_NAME), color = "white", stroke = .9) +
    ggplot2::scale_fill_manual(values = palette, na.value="#edf1f7", name = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major = element_line(colour = 'transparent'),
                   legend.position=c(.2, .2),
                   axis.title.x=element_blank(),
                   axis.text.x=element_blank(),
                   axis.ticks.x=element_blank(),
                   axis.title.y =element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks.y=element_blank(),
                   title = element_text(size = 18),
                   text = element_text(family = "Corbel", color = "#6c727c"),
                   legend.direction  = "vertical",
                   legend.background = element_rect(size=0.25, linetype="solid",
                                                    colour ="#6c727c")) +
    ggplot2::labs(title = paste0("Precinct assignments for census blocks in ", county, " County")) +
    ggplot2::guides(color = guide_legend(override.aes = list(size=4))) +
    ggplot2::guides(col = guide_legend(nrow = 8))

}
