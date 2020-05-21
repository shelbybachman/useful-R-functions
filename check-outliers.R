check_outliers <- function(data, var) {
  # this function detects outliers
  # using the MAD-median rule,
  # and shows a plot with outliers
  # based on standard boxplot rule.
  # dependencies: ggplot2;
  #
  # arguments:
  # data - dataframe containing variable,
  # var1 - string, name of variable
  
  sample_median <- median(data[,var], na.rm = TRUE)
  sample_mad <- mad(data[,var], na.rm = TRUE)
  data[,'mad_med'] <- (data[,var] - sample_median) / (1.483 * sample_mad)
  data[,'outlier'] <- data[,'mad_med'] > 2.24
  
  output <- vector('list', 2)
  output[[1]] <- which(data[,'outlier'] == TRUE)
  if (length(output[[1]]) == 0) {
    message <- paste('MAD-median rule detected ', 
                     sum(data[,'outlier'] == TRUE), 
                     ' outliers', sep = '')
  } else {
    message <- paste('MAD-median rule detected ', 
                     sum(data[,'outlier'] == TRUE), 
                     ' outlier(s) ', sep = '')
  }
  
  output[[2]] <- ggplot2::ggplot(data = data,
                                 ggplot2::aes_string(y = var)) +
    ggplot2::geom_boxplot(fill = '#6ec499', alpha = 0.5, width = 0.3,
                          outlier.size = 3) +
    ggplot2::labs(x = '', subtitle = message) +
    ggpubr::theme_pubr() +
    ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank())
  return(output)
}
