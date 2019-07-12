draw_heatmap <- function(sym.matrix, fill.name="Value", xlabel="", ylabel="", lim=c(-1,1),
                         pyramid=TRUE, breaks=c(-1,-0.5,0,0.5,1), midpoint=0, barwidth=5, barheight=1,
                         text_size=8, direction="horizontal", legend=TRUE, low="blue",mid="white",high="red"){
  #'  Make a ggplot2 triangle heatmap
  #'
  #'  Plots a heatmap of a symmetric matrix.
  #'
  #'  @param sym.matrix A matrix of symmetric values indicating a relationship between variables of the rows/columns.
  #'  @param fill.name Name of values in the symmetric matrix.
  #'  @param xlabel Label to include on x-axis.
  #'  @param ylabel Label to include on y-axis.
  #'  @param lim Limits of the fill values.
  #'  @param pyramid Boolean indicates lower triangle of the heamap be removed (TRUE) or not (FALSE).
  #'  @param breaks Number of break points in the legend (if legend included).
  #'  @param midpoint The midpoint of the legend scale (if legend included).
  #'  @param barwidth Width of the legend (if legend included).
  #'  @param barheight Height of the legend (if legend included).
  #'  @param direction Legend direction either "vertical" or "horizontal".
  #'  @param legend Boolean value indicating whether or not to include a legend for the plot.
  #'
  #'  @return A ggplot heatmap object that can be further manipulated using ggplot components if necessary.
  #'
  #'  @examples
  #'  draw_heatmap(matrix(c(2,1,0,3,0,1,0,3,2),3,3), lim=c(0,3),breaks=c(0,1.5,3),midpoint=1.5)
  #'  draw_heatmap(matrix(c(-0.5,1,0,0.75,0,1,0,0.75,-0.5),3,3))

  number.of.experiments <- dim(sym.matrix)[2]
  # to remove the redundancy in the heatmap, set all cells in matrix under the diagonal
  # to NA
  if (pyramid == TRUE) {
    # check if matrix is symmetric
    if(!all(sym.matrix == t(sym.matrix))){
      stop("The matrix is not symmetric!")
    }

    #if matrix is symmetric, then remove the redundant triangle of the matrix by setting to NA
    for( i in 1:number.of.experiments){
      j <- 1
      while( j < i){
        sym.matrix[i, j] <- NA
        j <- j + 1
      }
    }
  }

  #reshape the matrix to get row/column pairs and their value (Note: dataframes will not work as expected
  # of matrices here)
  overlap.melted <- melt(sym.matrix, value.name=fill.name)
  #use ggplot2 to visualize create a ggplot2 object
  plt <- ggplot(data=overlap.melted,
                mapping = aes(x=Var1, y=Var2, fill=Value)) +
    geom_tile() +
    theme(panel.background = element_blank(),
          plot.background = element_blank(),
          axis.text=element_text(color="gray15", angle=0),
          axis.text.x=element_text(angle=90),
          axis.title = element_text(color="gray15"),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          text = element_text(size=text_size))+
    coord_equal(clip = "off") +
    scale_x_discrete(expand=c(0,0), position="top") +
    scale_y_discrete(expand=c(0,0)) +
    scale_fill_gradient2(low=low, mid=mid, high=high, midpoint=midpoint,
                         space="Lab", na.value="transparent",
                         limits=lim, breaks = breaks) +
    xlab(xlabel) +
    ylab(ylabel)

  #If a legend should be included, add it to the plt ggplot2 object
  if (legend==TRUE){
    plt <- plt +
      guides(fill=guide_colorbar(barwidth=7, barheight=1,ticks=FALSE,direction = "horizontal",
                                 title.position = "top",
                                 label.position = "bottom",
                                 title.theme = element_text(colour = "gray15",size=8),
                                 title.hjust = 0.5,
                                 label.theme = element_text(angle = 90, hjust = 0.8, vjust = 0.5,
                                                            size = 6,colour = "gray15")))

  }

  return(plt)
}


write_pine_plot <- function(h_maps,num_heatmaps=length(h_maps), height=30, width=10, leg=FALSE,
                            angle = c(-45), legend_x_offset=0.02, legend_y_offset=0, hm_y_offset=0,
                            hm_margin=0){
  #'  Build a pine plot
  #'
  #'  Make the pine plot for a list of heatmaps and organize in a stacked, pine tree shape.
  #'
  #'  @param h_maps List of heat maps to add to a pineplot.
  #'  @param num_heatmaps The total number of heat maps to add to pine plot.
  #'  @param height The height of the file to start plotting the heat maps.
  #'  @param legend Boolean indicating if plots have a legend.
  #'  @param angle angle to rotate each heat map.
  #'  @param legend_x_offset Depending on label size and length, may want to adjust position of the legend using offset.
  #'  @param legend_y_offset Depending on label size and length, may want to adjust the y position of the pine plot.
  #'  @param hm_y_offset Changes the y posiiton of heatmap shifting them higher or lower in the viewport.
  #'  @param hm_margin Changes the space left between the heat maps if they need to be closer or farther appart for athstetic purpose.
  #'
  #'  @return Pine plot written to view or a file if open. No return object.
  #'

  #check if the number of heatmaps is or num_heatmaps empty
  if(num_heatmaps < 1 | length(h_maps) == 0){
    stop("Heatmaps required.")
  }

  #check if different lengths provided for the number of heatmaps
  if(num_heatmaps != length(h_maps)){
    print("The number of heatmaps in h_maps (length) does not match num_heatmaps. Ensure this is intentional.")
  }

  #original viewport (width and height of page specified)
  pushViewport(viewport(width=unit(width,"cm"),height=unit(height,"cm")))
  #store one heatmap in list (h_maps) with hm
  hm <- c()

  #for all the heatmaps in h_maps
  for(n in 1:num_heatmaps){
    # get heatmap at position n in the list h_maps
    hm <- h_maps[[n]]
    # set width as the height of viewport divided by number of heatmaps
    w = (height/num_heatmaps)
    # set height as the height of viewport divided by number of heatmaps
    h = (height/num_heatmaps)

    #calculate the diagonal size of each heatmap to determine placement along y-axis
    hm_size = sqrt((w**2) + (h**2))/2 + hm_margin


    if(leg==TRUE){
      #extract and save the legend
      tmp <- ggplot_gtable(ggplot_build(hm))
      legend <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[legend]]
      #remove any legend from the heatmaps if it is present
      hm <- hm + theme(legend.position = "none",
                       plot.margin = unit(c(1, 1, 1, 1), "cm"))
      # place the legend or the trunk of the tree
      vp <- viewport(x = unit((width/2)+legend_x_offset,"cm"),
                     y = unit((height-0.5) - ((hm_size)*(num_heatmaps+1))+(hm_size/2) + legend_y_offset, "cm"))
      legend$vp <- vp
      grid.draw(legend)
    }
    # create viewport with equal width and height for heatmap centered
    # and placed below y coordinates of previous heatmap
    # at a 45 degree angle
    vp <- viewport(width = unit(w,"cm"),
                   height = unit(h,"cm"),
                   x = 0.5,
                   y = unit((height-0.5) - ((hm_size)*(n)) + hm_y_offset, "cm"),
                   angle=angle)

    #push viewport turning heatmaps at a 45 degree angle
    pushViewport(vp)
    #make a ggplot table
    hm <- ggplot_gtable(ggplot_build(hm))
    #turn off clipping to avoid clipping issues when rotating ggplot2 object
    hm$layout$clip[hm$layout$name=="panel"] = "off"
    hm$layout$clip = "off"
    #draw the ggplot object
    grid.draw(hm)
    #return to the original viewport (height x width)
    upViewport()

  }

}




write_pine_plot <- function(h_maps, width = 10,
                            angle = c(-45),
                            hm_margin=0, ncols=1,
                            nrows=3, path_name){
  #'  Build a pine plot
  #'
  #'  Make the pine plot for a list of heatmaps and organize in a stacked, pine tree shape.
  #'
  #'  @param h_maps List of heat maps to add to a pineplot.
  #'  @param num_heatmaps The total number of heat maps to add to pine plot.
  #'  @param height The height of the file to start plotting the heat maps.
  #'  @param legend Boolean indicating if plots have a legend.
  #'  @param angle angle to rotate each heat map.
  #'  @param legend_x_offset Depending on label size and length, may want to adjust position of the legend using offset.
  #'  @param legend_y_offset Depending on label size and length, may want to adjust the y position of the pine plot.
  #'  @param hm_y_offset Changes the y posiiton of heatmap shifting them higher or lower in the viewport.
  #'  @param hm_margin Changes the space left between the heat maps if they need to be closer or farther appart for athstetic purpose.
  #'
  #'  @return Pine plot written to view or a file if open. No return object.
  #'

  #store one heatmap in list (h_maps) with hm
  hm <- c()
  w <- width - (2*hm_margin)
  h <- sqrt(w**2 + w**2)
  i <- 0

  #create the pdf file to write the heat maps to
  pdf(path_name, width=(h/2.54)*ncols, height = nrows*((h/2)+hm_margin)/2.54)
  #create a grid of nrows rows and ncols columns
  #each heatmap will be contained in one cell of the grid
  pushViewport(viewport(layout = grid.layout(nrows, ncols)))

  for(c in 1:ncols){
    for(r in 1:nrows){
      # get heatmap at position n in the list h_maps
      if(r+i > length(h_maps)){
        stop("Not enough heat maps to fill the grid.")
      }
      hm <- h_maps[[r+i]]
      #remove any legend from the heatmaps if it is present
      hm <- hm + theme(legend.position = "none")

      # create viewport with equal width and height for heatmap centered
      # and placed below y coordinates of previous heatmap
      # at a 45 degree angle
      vp <- viewport(width = unit(w,"cm"),
                     height = unit(w,"cm"),
                     x = 0.5,
                     y = unit(1+2*hm_margin, "cm"),
                     angle=angle)

      pushViewport(viewport(layout.pos.row = r, layout.pos.col = c))
      pushViewport(vp)

      #make a ggplot table
      hm <- ggplot_gtable(ggplot_build(hm))
      #turn off clipping to avoid clipping issues when rotating ggplot2 object
      hm$layout$clip[hm$layout$name=="panel"] = "off"
      hm$layout$clip = "off"
      #draw the ggplot object
      grid.draw(hm)
      upViewport()
      upViewport()

    }
    #update i to select the next set of heat maps
    i <- i + nrows
  }

  dev.off()

}