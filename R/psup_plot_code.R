#' PSup plot code
#'
#' Print out code for PSup matrix guides and explanations
#'
#' @export
psup_plot_code <- function(item_name = "item") {
  glue::glue('geom_richtext(aes(label = psup_label), family = "Jost", size = 3, text.color = "black", color = NA, fill = NA) +
  scale_fill_gradient2(low = "#327291", mid = "white", high = "#f2b831", midpoint = .5, limits = c(0, 1),
                       breaks = seq(0, 1, .2),
                       labels = jimbilben::nice_num(seq(0, 1, .2), 1, remove_lead = FALSE)) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = .5, barwidth = 17.5, barheight = .75)) +
  labs(fill = "*PSup* - Probability *y* axis {item_name} ({item_name}s on the left) > *x* axis {item_name} ({item_name}s on the top)",
       subtitle = "Example interpretation: *PSup* in top-left cell is .XX for *XX* > *YY*. We expect ~ XX%<br>of people to rate *XX* higher than *XX*"
       )')
}
