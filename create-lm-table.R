# this function creates a dataframe
# containing coefficients, SEs, CIs, t-values, and p-values
# resulting from the linear model fit in R
# written by shelby bachman, 2020

# dependencies: dplyr, weights, tidyr
# inputs: lm.object is the output of a call to lm(),
# include.intercept is 1 if you would like intercept stats reported and 0 if not,
# vector.of.term.names is a vector of strings containing the names of the terms to be listed in the table's first column

create_lm_table <- function(lm.object, include.intercept, vector.of.term.names) {
        summary_temp <- summary(lm.object)

        # coefficients
        if (include.intercept == 1) {
                coeffs_temp <- as.data.frame(summary_temp$coefficients)
        } else if (include.intercept == 0) {
                coeffs_temp <- as.data.frame(summary_temp$coefficients)
                coeffs_temp <- coeffs_temp[row.names(coeffs_temp) != '(Intercept)',]
        }
        n_coeffs <- nrow(coeffs_temp) # number of coeffs including intercept
        names(coeffs_temp) <- c('Beta', 'SE', 't', 'p')
        betas <- round(coeffs_temp$Beta, digits = 3)
        SEs <- round(coeffs_temp$SE, digits = 3)
        ts <- round(coeffs_temp$t, digits = 2)
        ps <- NULL
        for (ii in 1:length(coeffs_temp$p)) {
                if (coeffs_temp$p[ii]<.001) {
                        ps[ii] <- '<.001'
                } else {
                        ps[ii] <- rd(coeffs_temp$p[ii], digits = 3)
                }
        }

        # confidence intervals
        confint_temp <- as.data.frame(confint(lm.object, level = 0.95))
        if (include.intercept == 0) {
                confint_temp <- confint_temp[-1,]
        }

        names(confint_temp) <- c('lower', 'upper')

        confint_temp <- confint_temp %>%
                mutate(upperRounded = round(upper, digits = 3),
                       lowerRounded = round(lower, digits = 3))
        confint_temp <- unite(confint_temp, col = CI, upperRounded:lowerRounded, sep = ', ', remove = FALSE)
        CIs <- confint_temp$CI

        # set up table with coefficients, SEs, etc.
        results_table <- as.data.frame(cbind(
                vector.of.term.names, betas, SEs, CIs, ts, ps
        ))
        names(results_table) <- c('Predictor', 'Beta', 'SE', '95% CI', 't', 'p')

        # output to return
        return(results_table)

}

