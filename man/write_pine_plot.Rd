% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_pineplots.R
\name{write_pine_plot}
\alias{write_pine_plot}
\title{Make the pine plot for a list of heatmaps and organize in a stacked, pine tree shape.}
\usage{
write_pine_plot(h_maps, num_heatmaps = length(h_maps), height = 30,
  width = 10, leg = FALSE, angle = c(-45), legend_x_offset = 0.02,
  legend_y_offset = 0, hm_y_offset = 0, hm_margin = 0)
}
\arguments{
\item{h_maps}{List of heat maps to add to a pineplot.}

\item{num_heatmaps}{The total number of heat maps to add to pine plot.}

\item{height}{The height of the file to start plotting the heat maps.}

\item{width}{The width of the file to start plotting the heat maps.}

\item{leg}{Boolean indicating if plots have a legend.}

\item{angle}{angle to rotate each heat map.}

\item{legend_x_offset}{Depending on label size and length, may want to adjust position of the legend using offset.}

\item{legend_y_offset}{Depending on label size and length, may want to adjust the y position of the pine plot.}

\item{hm_y_offset}{Changes the y posiiton of heatmap shifting them higher or lower in the viewport.}

\item{hm_margin}{Changes the space left between the heat maps if they need to be closer or farther appart for athstetic purpose.}
}
\value{
Pine plot written to view or a file if open. No return object.
}
\description{
Make the pine plot for a list of heatmaps and organize in a stacked, pine tree shape.
}
