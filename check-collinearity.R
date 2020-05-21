check_collinearity <- function(data, var1, var2) {
  # this function calculates R squared, 
  # tolerance, and VIF for two variables,
  # prints a message about collinearity,
  # and shows a scatterplot; 
  # dependencies: ggplot2;
  #
  # arguments:
  # data - dataframe containing variables
  # var1 - string, name of first variable
  # var2 - string, name of second variable
  
  r <- (cov(data[,var1], data[,var2]))/(sd(data[,var1])*sd(data[,var2]))
  r_squared <- r^2
  tol <- 1 - r_squared
  vif <- 1/(1 - r_squared)
  output <- NULL
  output[[1]] <- c(r_squared, tol, vif)
  output[[2]] <- paste('R-squared: ', round(r_squared,3), 
                       ', Tolerance: ', round(tol,3), 
                       ', VIF: ', round(vif,3), sep = '')
  output[[3]] <- if (r_squared > 0.90 | tol < 0.10 | vif > 10.0) {
    'Extreme collinearity indicated!'
  } else {
    'No extreme collinearity indicated'
  }
  output[[4]] <- ggplot2::ggplot(data = data, 
                                 ggplot2::aes_string(var1, var2)) +
    ggplot2::geom_point(alpha = 0.8) +
    ggplot2::geom_smooth(se = TRUE, method = 'lm', colour = '#40a070') +
    ggplot2::labs(subtitle = paste(output[[2]], output[[3]], sep = '\n')) +
    ggpubr::theme_pubr()
  return(output)
}