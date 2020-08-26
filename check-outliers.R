check_outliers <- function(data, name, colour = '#6ec499') {
  # this function detects outliers
  # using the MAD-median rule,
  # and shows a plot with outliers
  # based on standard boxplot rule.
  # dependencies: ggplot2, ggpubr
  #
  # arguments:
  # data - variable of interest, a numeric vector
  # name - name of variable, a string (goes in figure title)
  # colour - hex code for fill of boxplot, default #40a070
  
  sample_median <- median(data, na.rm = TRUE)
  sample_mad <- mad(data, na.rm = TRUE)
  mad_med <- ifelse(is.na(data), NA, data - sample_median / (1.483 * sample_mad))
  outliers <- ifelse(mad_med > 2.24, 1, 0)
  
  output <- vector('list', 2)
  output[[1]] <- which(outliers == 1)
  if (length(output[[1]]) == 0) {
    message <- paste('MAD-median rule detected ', 
                     sum(outliers == 1), 
                     ' outliers', sep = '')
  } else {
    message <- paste('MAD-median rule detected ', 
                     sum(outliers == 1), 
                     ' outlier(s) ', sep = '')
  }
  
  data_plot <- as.data.frame(data)
  output[[2]] <- ggplot2::ggplot(data = data_plot,
                                 ggplot2::aes(y = data)) +
    ggplot2::geom_boxplot(fill = colour, alpha = 0.5, width = 0.3, position = ggplot2::position_dodge(width = 0.5),
                          outlier.size = 3) +
    ggplot2::labs(x = '', y = name, subtitle = message) +
    ggpubr::theme_pubr() +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank())
  return(output)
}
