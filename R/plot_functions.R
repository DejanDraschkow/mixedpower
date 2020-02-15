#' Multiple plot function from Cookbook-R
#'
#' \code{multiplot(..., plotlist=NULL, file, cols=1, layout=NULL)} puts
#'  multiple graphs on one page.
#'
#' @param ... ggplot objects passed to the function
#' @param plotlist List of ggplot objects that can be passed to the function
#' @param cols Number of collumns in layout
#' @param layout A matrix specifying the layout. If present, 'cols' is ignored
#'
#' @export
#'


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {


  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#' Multiple plot function to plot the mmpower output
#'
#' \code{multiplotPower(output_data, ppi = 300)} plots the output for every
#' effect in the mixed model used for power simulation.A png file with all
#' plots is saved to your working directory.
#'
#' @param output_data data frame returned from \code{fullpower} function
#' @param ppi resolution of plot. default is 300
#'
#'
#' @export

multiplotPower <- function(output_data, ppi = 300, filename = F){
  # ------------------------------------------#
  ## 1. prepare output for plotting
  plot_data_all <- reshape2::melt(output_data,
                        id.vars = c("mode", "effect"),
                        variable.name = "sample_size")


  # ------------------------------------------#
  # 2. create plot for every effect

  # how many plots do we need? and which effects are they?
  fixeffects <- unique(output_data$effect)

  cbPalette <- c("#999999", "#E69F00", "#56B4E9")

  # collect plots
  all_plots <- list()

  i <- 1 # index loop
  for (fixeff in fixeffects){

    # create subset of plot
    plot_data <- subset(plot_data_all,  effect == fixeff,
                        select = c(mode, sample_size,value))


    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = sample_size, y = value, color=mode, group = mode)) + # basic graph
      ggplot2::geom_point() + # add points
      ggplot2::geom_line()+
      ggplot2::xlab("step size") + ggplot2::ylab("power") +  # change name of x and y axes
      ggplot2::theme(axis.line = ggplot2::element_line(color = "black"), # axis line
            panel.background = ggplot2::element_rect(fill = "white"), # background
            legend.title = ggplot2::element_blank()) + # remove legend title
      #labs(linetype = "condition", color = "condition") +
      #scale_color_viridis(discrete = T, option = "inferno")  +  # color
      ggplot2::scale_color_manual(values = cbPalette) +
      ggplot2::expand_limits(y = c(0,1)) + # fix axis to 0 - 1
      ggplot2::ggtitle(fixeff) # titel

    # store plot
    all_plots[[i]] <- p
    i <- i+1
  }# end for loop


  # ------------------------------------------#
  # 3. create multiplot
  # adjust plot dimensions according to the number of effects in the data
  effect_num = length(fixeffects)
  ncol = 3
  if (effect_num%%3 == 0) {
    nrow = effect_num/3
  } else {
    nrow = floor(effect_num/3) + 1
  }

  # allow for custom filename
  if (filename == F){
     filename <- "multiplot_powerSimulation.png"
  }
  png(filename, width=12*ppi, height=3*nrow*ppi, bg = "transparent", res=ppi)
  struct <- matrix(seq(1,nrow*ncol), ncol = ncol, nrow = nrow)
  multiplot(plotlist = all_plots, layout = struct)
  dev.off()

} # end function
