#' PSup plot code
#'
#' Print out code for PSup matrix guides and explanations
#'
#' @export
code_psup_plot <- function(item_name = "item") {
  glue::glue('ggplot(aes(x = b, y = a, fill = mean)) +
  geom_tile() +
  scale_x_discrete(position = "top") +
  ggtext::geom_richtext(aes(label = psup_label), family = "Jost", size = 3, text.color = "black", color = NA, fill = NA) +
  scale_fill_gradient2(low = "#327291", mid = "white", high = "#f2b831", midpoint = .5, limits = c(0, 1),
                       breaks = seq(0, 1, .2),
                       labels = jimbilben::nice_num(seq(0, 1, .2), 1, remove_lead = FALSE)) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = .5, barwidth = 20, barheight = .5)) +
  labs(fill = "*PSup* - Probability *y* axis item (items on the left) > *x* axis item (items on the top)",
       subtitle = "Example interpretation: *PSup* in top-left cell is .91 for *HIG* > *GPR*. We expect ~91%<br>of people to rate *HIG* higher than *GPR*",
       x = "",
       y = "",
       title = "Probability of Superiority between names"
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text.x.top = ggtext::element_markdown(angle = 45, vjust = 0, hjust = 0),
    legend.position = "bottom",
    axis.text = ggtext::element_markdown(family = "Jost"),
    legend.text = element_text(family = "Jost"),
    legend.title = element_text(family = "Jost"),
    plot.title = ggtext::element_markdown(family = "Jost"),
    plot.subtitle = ggtext::element_markdown(family = "Jost"),
    plot.title.position = "plot",
    axis.ticks = element_blank()
  )')
}
