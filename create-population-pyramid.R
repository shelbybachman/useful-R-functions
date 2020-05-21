##########################
# function to create population pyramids
# written by shelby bachman, 2020
##########################

#### references:
# https://www.r-bloggers.com/animated-population-pyramids-in-r/
# https://www.statology.org/how-to-create-a-population-pyramid-in-r/

#### this function creates a population pyramid, stratified by sex
#### and animating through years

## dependencies
# dplyr
# ggplot
# ggsci
# ggpubr
# gganimate (if using animate option)

## arguments
# data: data frame (see specifications below)
# age_continuous: TRUE if age values are continuous, FALSE if you have age brackets (defaults to FALSE)
# age_bin_width: width of bins for age axis (defaults to 5)
# animate: TRUE if you want a plot that animates over years, FALSE if you do not want to animate (defaults to TRUE)
# year_to_plot: if you are not animating and want to plot a single year

## input data format
# input data should have the following variable, named exactly as listed:
# age 
# percent_in_pop
# sex
# year

create_population_pyramid <- function(data = data,
                                      age_continuous = FALSE,
                                      age_bin_width = 5,
                                      animate = TRUE,
                                      year_to_plot = NULL) {
  data_plot <- data %>%
    mutate(base = 0) %>%
      mutate(percent_in_pop_mod = ifelse(sex == 'female', percent_in_pop,
                                         ifelse(sex == 'male', percent_in_pop*(-1), NA)))
  
  if (!is.null(year_to_plot) && isTRUE(animate)) {
    stop('You entered a year but indicated you want an animated plot. Try again!')
  }
  
  if (is.null(year_to_plot) && isFALSE(animate) && (length(unique(data$year)) > 1)) {
    warning('Warning: You did not enter a year but you have more than one year in your data, and you do not want to animate.')
  }
  
  if (!is.null(year_to_plot)) {
    data_plot <- data_plot %>%
      filter(year == year_to_plot)
  }
  
  if (isFALSE(age_continuous)) {
    
    # create plot with geom_col
    p <- ggplot(data = data_plot,
                aes(x = AgeGroup, 
                    y = percent_in_pop_mod,
                    fill = sex))
    
    pyr <- p + geom_col(alpha = 0.5) +
      scale_y_continuous(labels = abs, limits = max(data$percent_in_pop)*c(-1, 1)) +
      scale_fill_aaas() +
      guides(fill = guide_legend(reverse = TRUE)) +
      labs(x = 'Age Group', y = 'Percent of population', fill = 'Sex') +
      theme_pubr() +
      theme(legend.position = 'right',
            axis.text.y = element_text(size = rel(0.9)),
            axis.text.x = element_text(size = rel(0.9))) +
      coord_flip()
    
  } else if (isTRUE(age_continuous)) {
      
    # create plot with geom_ribbon
      p <- ggplot(data = data_plot,
                aes(x = age, 
                    ymin = base, ymax = percent_in_pop_mod,
                    fill = sex))
      
      pyr <- p + geom_ribbon(alpha = 0.5) +
        scale_y_continuous(labels = abs, limits = max(data_plot$percent_in_pop)*c(-1, 1)) +
        scale_x_continuous(breaks = seq(floor(min(data_plot$age)), ceiling(max(data_plot$age)), age_bin_width)) +
        scale_fill_aaas() +
        guides(fill = guide_legend(reverse = TRUE)) +
        labs(x = 'Age', y = 'Percent of population', fill = 'Sex') +
        theme_pubr() +
        theme(legend.position = 'right',
              axis.text.y = element_text(size = rel(0.9)),
              axis.text.x = element_text(size = rel(0.9))) +
        coord_flip()
  }
  
  if (isTRUE(animate)) {
    pyr <- pyr + transition_time(year) +
      labs(title = 'Year: {frame_time}')
    }

  return(pyr)
  
}
