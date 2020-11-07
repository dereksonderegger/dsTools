#' Creates a Dotplot
#'
#' The scaling for ggplot2::dotplot() is unfortunately designed
#' for more complex situations than what is used in an introductory
#' resampling-based statistics course. It would be very nice to
#' be able to make a geom_dotplot() function that respected the
#' y-axis.
#'
#' The reason dot plots are hard is because a dot is necessarily a
#' fixed ratio in the x and y direction. So to make them look circular
#' in change scales, we can end up with an oddly proportion graph. Perhaps
#' wide and short, perhaps tall and narrow. It is up to the user to specify
#' the binwidths so that the resulting graph is aesthetically pleasing.
#'
#' @param x A vector of numeric values
#' @param binwidth The width of a column of dots. If blank, we'll default
#'                 to using Sturges' rule.
#' @param dotsize The relative size of a dot. Only is used for separating columns
#'                of dots. Dots always touch vertically. Default is touching columns.
#' @param yheight The maximum height of the graph, on the count scale.
#' @param ybreaks The labeled breaks on the y-axis. On the count scale.
#' @param origin Where should the center of a stack of dots be? You may select any
#'               column of dots to specify.
#' @result A ggplot object. The x-axis is on the original scale as the input
#'         data, however the y-axis has been re-scaled so that the underlying scale
#'         is actually from 0 to 1. As a result, any additional layers to be added need
#'         to respect the [0,1] y-axis.
#' @examples
#' set.seed(8675309)
#' x <- rnorm(100)
#' ggdotplot( x ) +
#'   annotate('text', x=1.5, y=.75, label='Text to write')
#' ggdotplot( x, binwidth=0.1 )
#'
#' # Can set bins width and centers.
#' ggdotplot(x, origin=-2, binwidth=.25)
#' ggdotplot( x, binwidth=.25, origin=.1) # ugly and messes up height calculation
#' ggdotplot( x, binwidth=1, origin=1, dotsize=.8)
#'
#' # Can add space at the top of the graph by setting yheight on the count scale.
#' ggdotplot( c(1,1,2:5), yheight=6)
#'
#' # Setting the y-axis major breaks
#' ggdotplot( x, ybreaks=0:15 )
#'
#' @export
ggdotplot <- function(x, binwidth=NULL, dotsize=1, yheight=NULL, ybreaks=NULL, origin=NULL){

  data <- data.frame(x=x)

  # Address binwidth
  if(is.null(binwidth)){
    rng <- range(x, na.rm = TRUE)
    K <- round(1 + 3.322 * log( length(x) ))
    binwidth = (rng[2] - rng[1])/K
  }
  # Address bin Centers
  if(!is.null(origin)){
    origin2 = origin - binwidth/2   # geom_dotplot() uses left side of bin
  }else{
    origin2 = NULL
  }
  # Address plot Height
  if(is.null(yheight)){
    yheight  <- data %>%
      mutate( z = round(x  / binwidth)  * binwidth) %>%
      group_by(z) %>% count() %>% pull( n ) %>% max()
    yheight  <- yheight * 1.05     # Add some room at the top of the graph.
  }
  if(is.null(ybreaks)){
    ybreaks <- pretty(0:yheight) %>% round() %>% unique()
  }

  # Finally actually produce the graph!
  out <- ggplot(data, aes(x=x)) +
    geom_dotplot(binwidth = binwidth, method='histodot', dotsize=dotsize, origin=origin2) +
    scale_y_continuous(limits=c(0, 1), expand = c(0, 0),               # Clean up the
                       breaks = ybreaks/yheight,                       # dotplot by
                       labels = ybreaks) +                             # utilizing the
    coord_fixed(ratio=binwidth * dotsize * yheight)                    # yheight info

  return(out)
}



