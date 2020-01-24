# this function creates a dataframe containing a summary
# of clusters resulting from multiple comparison correction in freesurfer's group analysis stream
# written by shelby bachman, 2020

# inputs: filename_left is the full filepath to the file containing the left hemisphere cluster output,
# filename_right is the full filepath to the file containing the right hemisphere cluster output,
# include.association.sign is 1 if a column should be included as to whether the cluster reflects a positive or negative association with thickness
# and 0 if this column should not be included
# dependencies: data.table, dplyr, Hmisc
create_freesurfer_cluster_table <- function(filename_left, filename_right,
                                            include.association.sign) {

        # load data and fix column names
        # then create left and right tables, keeping only relevant columns
        temp_left <- fread(filename_left, fill = TRUE, skip = 39)
        if (nrow(temp_left) > 0) {
                names(temp_left) <- c(names(temp_left)[2:length(temp_left)], 'NA')
                temp_left <- temp_left[,!'NA']

                temp_left <- temp_left %>%
                        mutate(Hemisphere = 'Left',
                               `Association with thickness` = ifelse(Max > 0, 'Positive',
                                                                     ifelse(Max < 0, 'Negative', NA)),
                               CWP_round = round(CWP, digits = 4),
                               Region = capitalize(Annot)) %>%
                        select(Region, Hemisphere, `Association with thickness`,
                               `Size (mm^2)` = `Size(mm^2)`, X = MNIX, Y = MNIY, Z = MNIZ, CWP = CWP_round)

                temp_left <- temp_left %>%
                        arrange(desc(`Size (mm^2)`))

                if (include.association.sign == 0) {
                        temp_left <- temp_left %>%
                                select(-`Association with thickness`)
                }

        }

        temp_right <- fread(filename_right, fill = TRUE, skip = 39)
        if (nrow(temp_right) > 0) {

                names(temp_right) <- c(names(temp_right)[2:length(temp_right)], 'NA')
                temp_right <- temp_right[,!'NA']

                temp_right <- temp_right %>%
                        mutate(Hemisphere = 'Right',
                               `Association with thickness` = ifelse(Max > 0, 'Positive',
                                                                     ifelse(Max < 0, 'Negative', NA)),
                               CWP_round = round(CWP, digits = 4),
                               Region = capitalize(Annot)) %>%
                        select(Region, Hemisphere, `Association with thickness`,
                               `Size (mm^2)` = `Size(mm^2)`, X = MNIX, Y = MNIY, Z = MNIZ, CWP = CWP_round)

                temp_right <- temp_right %>%
                        arrange(desc(`Size (mm^2)`))

                if (include.association.sign == 0) {
                        temp_right <- temp_right %>%
                                select(-`Association with thickness`)
                }
        }

        # combine into single table
        if (nrow(temp_left) > 0 & nrow(temp_right) > 0) {
                cluster_table <- as.data.frame(rbind(temp_left, temp_right))
        } else if (nrow(temp_left) == 0 & nrow(temp_right) > 0) {
                cluster_table <- temp_right
        } else if (nrow(temp_left) > 0 & nrow(temp_right) == 0) {
                cluster_table <- temp_left
        } else if (nrow(temp_left) == 0 & nrow(temp_right) == 0) {
                cluster_table <- NULL
        }

        return(cluster_table)

}
