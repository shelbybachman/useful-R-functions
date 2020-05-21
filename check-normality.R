check_normality <- function(data, var) {
  # this function calculates
  # skew and tolerance for a variable,
  # prints a message about normality,
  # and shows a frequency distribution,
  # a stem & leaf plot, and a qq plot;
  # dependencies: ggplot2;
  #
  # arguments:
  # data - dataframe containing variables
  # var - string, name of variable
  
  length_data <- nrow(data[,!is.na(var)])
  temp <- data[,var] - mean(data[,var], na.rm = TRUE)
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
    'Skew/kurtosis indicate severe non-normality!'
  } else {
    'Metrics do not indicate severe non-normality. \nCheck plots!'
  }
  
  output[[4]] <- ggpubr::ggarrange(
    ggplot2::ggplot(data = data,
                    ggplot2::aes_string(x = var)) +
      ggplot2::geom_density(ggplot2::aes(y = ..count..), 
                            colour = '#40a070') +
      ggplot2::geom_vline(ggplot2::aes(xintercept = mean(data[,var], na.rm = TRUE)),
                          linetype = 'dashed', size = 0.6) +
      ggplot2::labs(subtitle = output[[2]]) +
      ggpubr::theme_pubr(),
    ggplot2::ggplot(data = data,
                    ggplot2::aes_string(sample = var)) +
      ggplot2::stat_qq() +
      ggplot2::stat_qq_line(colour = '#40a070') +
      ggplot2::labs(subtitle = output[[3]]) +
      ggpubr::theme_pubr(),
    nrow = 1, ncol = 2)
  return(output) 
  
}