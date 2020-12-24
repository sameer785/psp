axisLabSize = 18
legendLabSize = 18
th <- theme_bw(base_size = 18) +
  
  theme(
    legend.background = element_rect(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_blank(), 
    panel.background = element_blank(),
    
    # axis text
    axis.text.x = element_text(
      angle = 0,
      size = 18,
      vjust = 1),
    axis.text.y = element_text(
      angle = 0,
      size = 18,
      vjust = 1),
    axis.title = element_text(
      size = axisLabSize),
    axis.line = element_line(
      size = .8),
    
    # legend
    legend.key = element_blank(),
    legend.text = element_text(
      size = legendLabSize),
    title = element_text(
      size = legendLabSize),
    legend.title = element_blank(),
    #legend.direction = "horizontal",
    legend.box.background = element_rect(
      colour = "black", size=1),
    legend.margin = margin(.5,.5,.5,.5))