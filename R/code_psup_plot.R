#' PSup plot code
#'
#' Print out code for PSup matrix guides and explanations
#'
#' @export
code_psup_plot <- function() {
  print(glue::glue('ggplot(aes(x = b, y = a, fill = mean)) +'))
  print(glue::glue('  geom_tile() +'))
  print(glue::glue('  scale_x_discrete(position = "top") +'))
  print(glue::glue('  ggtext::geom_richtext(aes(label = psup_label), family = "Jost", size = 3, text.color = "black", color = NA, fill = NA) +'))
  print(glue::glue('  scale_fill_gradient2(low = "#327291", mid = "white", high = "#f2b831", '))
  print(glue::glue('                       midpoint = .5, '))
  print(glue::glue('                       limits = c(0, 1),'))
  print(glue::glue('                       breaks = seq(0, 1, .2),'))
  print(glue::glue('                       labels = jimbilben::nice_num(seq(0, 1, .2), 1, remove_lead = FALSE)) +'))
  print(glue::glue('  guides(fill = guide_colorbar(title.position = "top", '))
  print(glue::glue('                               title.hjust = .5, '))
  print(glue::glue('                               barwidth = 35, '))
  print(glue::glue('                               barheight = .5)) +'))
  print(glue::glue('  labs(fill = "PSup - Probability y axis item (items on the left) > x axis item (items on the top)",'))
  print(glue::glue('       subtitle = "Example interpretation: *PSup* in top-left cell is .76 for *AI* > *LTism*. We expect ~76%<br>of people to rate *AI* higher than *LTism*",'))
  print(glue::glue('       x = "",'))
  print(glue::glue('       y = "",'))
  print(glue::glue('       title = "Probability of Superiority between items"'))
  print(glue::glue('  ) +'))
  print(glue::glue('  theme_default() +'))
  print(glue::glue('  theme('))
  print(glue::glue('    panel.background = element_blank(),'))
  print(glue::glue('    panel.grid = element_blank(),'))
  print(glue::glue('    panel.border = element_blank(),'))
  print(glue::glue('    axis.text.x.top = ggtext::element_markdown(angle = 45, vjust = 0, hjust = 0),'))
  print(glue::glue('    legend.position = "bottom",'))
  print(glue::glue('    axis.text = ggtext::element_markdown(family = "Jost"),'))
  print(glue::glue('    legend.text = element_text(family = "Jost", size = 9, hjust = .5),'))
  print(glue::glue('    legend.title = element_text(family = "Jost", size = 10, hjust = .5),'))
  print(glue::glue('    plot.title = ggtext::element_markdown(family = "Jost"),'))
  print(glue::glue('    plot.subtitle = ggtext::element_markdown(family = "Jost"),'))
  print(glue::glue('    plot.title.position = "plot",'))
  print(glue::glue('    axis.ticks = element_blank(),'))
  print(glue::glue('    axis.line = element_blank()'))
  print(glue::glue('  )'))
}

