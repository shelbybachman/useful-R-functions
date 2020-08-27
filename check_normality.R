check_normality <- function(data, name, colour = "#40a070") {
  # this function calculates
  # skew and tolerance for a variable,
  # prints a message about normality,
  # and shows a frequency distribution and a qq plot
  #
  # dependencies: ggplot2, ggpubr
  #
  # arguments:
  # data - variable of interest, a numeric vector
  # name - name of variable, a string (goes in figure title)
  # colour - hex code for color in plots, default #40a070
  
  data <- data[!is.na(data)]
  length_data <- length(data)
  temp <- data - mean(data)
  mom_2 <- sum(temp^2, na.rm = TRUE)/length_data
  mom_3 <- sum(temp^3, na.rm = TRUE)/length_data
  mom_4 <- sum(temp^4, na.rm = TRUE)/length_data
  
  skew_index <- mom_3 / (mom_2)^(3/2)
  kurtosis_index <- (mom_4 / (mom_2)^2) - 3
  
  output <- NULL
  output[[1]] <- c(skew_index, kurtosis_index)
  output[[2]] <- paste('Skew: ', round(skew_index, 3), 
                       ', Kurtosis: ', round(kurtosis_index, 3), sep = '')
  output[[3]] <- if (is.na(skew_index) | is.na(kurtosis_index)) {
    'One or more metrics is NA. Check plots!'
  } else if (skew_index > 3 | skew_index < -3 | 
             kurtosis_index > 10 | kurtosis_index < -10) {
    'Severe non-normality indicated!'
  } else {
    'Severe non-normality not indicated'
  }
  
  data_plot <- as.data.frame(data)
  output[[4]] <- ggpubr::ggarrange(
    ggplot2::ggplot(data = data_plot,
                    ggplot2::aes_string(x = data)) +
      ggplot2::geom_density(ggplot2::aes(y = ..count..), 
                            colour = colour) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = mean(data)),
                          linetype = 'dashed', size = 0.6) +
      ggplot2::labs(subtitle = output[[2]]) +
      ggpubr::theme_pubr(),
    ggplot2::ggplot(data = data_plot,
                    ggplot2::aes_string(sample = data)) +
      ggplot2::stat_qq() +
      ggplot2::stat_qq_line(colour = colour) +
      ggplot2::labs(subtitle = output[[3]]) +
      ggpubr::theme_pubr(),
    nrow = 1, ncol = 2)
  output[[4]] <- ggpubr::annotate_figure(output[[4]], 
                  top = ggpubr::text_grob(paste('Normality Check: ', name, sep = ''), 
                                          face = 'bold', size = 14))
  return(output) 
  
}
