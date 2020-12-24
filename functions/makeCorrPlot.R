makeCorrPlot  <- function(tbl, xt, yt){
  gp <- ggplot(tbl, aes_string(x=xt, y=yt))
  gp <- gp + geom_point()
  gp <- gp + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
  gp <- gp + scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
  gp <- gp + geom_abline(slope=1, intercept = 0)
  return(gp)
}