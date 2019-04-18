


theme_proj <- function (...) { 
  # theme_bw(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      # panel.border=element_blank(),
      panel.background = element_rect(fill = "gray99", colour = NA),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      # text = element_text(size=9),
      # strip.background = element_rect(colour = "white", fill = "white", size = 11), #muda estilo de facet_grid boxes
      # strip.text.x = element_text(size = 11, face ="bold"),
      strip.text.x = element_text(size = 11),
      # legend.justification = c(0, 0),
      # legend.position = c(0, 0),
      # legend.key.width = unit(3, "cm"),
      ...
    )
}


# theme_map <- function(base_size = 9, base_family = "") {
#   theme_bw(base_size = base_size, base_family = base_family) %+replace%
#     theme(axis.line = element_blank(),
#           axis.text = element_blank(),
#           axis.ticks = element_blank(),
#           axis.title = element_blank(),
#           panel.background = element_blank(),
#           panel.border = element_blank(),
#           panel.grid = element_blank(),
#           panel.spacing = unit(0, "lines"),
#           plot.background = element_blank(),
#           legend.justification = c(0, 0),
#           legend.position = c(0, 0))
# }
