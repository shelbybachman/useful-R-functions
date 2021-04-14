# this function computes the normalized cross-correlation coefficient
# reflecting the similarity between two nifti images
# written by shelby bachman, 2021

# dependencies: neurobase
# inputs: filename1 and filename2 are filepaths to nifti files to correlate
# notes: niftis must be in the same space

compute_ncc <- function(filename1, filename2) {
  
  # load images
  img1 <- neurobase::readNIfTI2(filename1)
  img2 <- neurobase::readNIfTI2(filename2)
  
  # reshape images to vectors
  vec1 <- c(img1)
  vec2 <- c(img2)
  
  # take the mean of each vector
  mean_data1 <- mean(data1)
  mean_data2 <- mean(data2)
  
  # compute the mean differences
  mean_diffs1 <- data1 - mean_data1
  mean_diffs2 <- data2 - mean_data2
  
  # take the sum of the squared mean differences
  sum_sq_diffs1 <- sum(mean_diffs1^2)
  sum_sq_diffs2 <- sum(mean_diffs2^2)
  
  # compute ncc
  num <- sum(mean_diffs1*mean_diffs2)
  denom <- sqrt(sum_sq_diffs1)*sqrt(sum_sq_diffs2)
  ncc <- num/denom
  
  return(ncc)
  
}