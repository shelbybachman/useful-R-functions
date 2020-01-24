# this function creates a string summarizing a linear model fit in R
# written by shelby bachman, 2020

# dependencies: dplyr, weights, stringr
# inputs: lm.object is the output of a call to the lm() function

create_lm_summary <- function(lm.object) {
        summary_temp <- summary(lm.object)

        # model fit summaries
        f <- summary_temp$fstatistic
        df1 <- round(f[2], digits = 0)
        df2 <- round(f[3], digits = 0)
        p <- pf(f[1], f[2], f[3], lower=FALSE)
        if (p<.001) {
                p <- ' < .001'
        } else {
                p <- rd(p, digits = 3)
                p <- str_c(' = ')
        }
        f <- round(f[1], digits = 2)
        r_squared <- round(summary_temp$r.squared, digits = 3)
        r_squared_adj <- round(summary_temp$adj.r.squared, digits = 3)

        # set up statement with model fit summary
        results_summary <- str_c('(F(', df1, ', ', df2, ') = ', f, ', p', p, ', ', 'R^2 = ', r_squared, ', adjusted R^2 = ', r_squared_adj, ')', sep = '')

        # output to return
        return(results_summary)
}
