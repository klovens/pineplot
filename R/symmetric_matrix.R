symmetric_matrix_generator <- function(exp_matrix, max.expression) {
  #' Function that calculates the mutual expression between genes in a expression table.
  #'
  #' @param exp_matrix The expression matrix where rows are the genes
  #' and columns are the samples/observations.
  #' @param max.expression is the maximum average value to normalize the average values
  #' of variables in exp_matrix.
  #'
  #' @return A symmetric matrix, \code{sym_mat}, that contains mutual expression value
  #' between 0 and 1 to symbolize relationship between variable x and variable y.

  # extract the rownames of the expression matrix
  rows <- rownames(exp_matrix)
  cols <- rows
  # initialize a new symmetric matrix that is n by n where n is the number of genes in
  # exp_matrix
  sym_mat <- matrix(0, nrow = nrow(exp_matrix), ncol = nrow(exp_matrix))
  # convert to dataframe for easy row and column name manipulation
  sym_mat <- as.data.frame(sym_mat)
  # v is a vector containing the row mean expression/value across each variable
  v <- rowMeans(exp_matrix)
  # calculate number of variables
  number_of_genes <- length(v)
  # calculate mutual expression between each variable across rows and across columns
  for (i in 1:number_of_genes) {
    for (j in 1:number_of_genes) {
      # get mean expression value for row i
      x <- v[i]
      # get mean expression value for row j
      y <- v[j]
      # calculate the mutual expression, keeping it between 0 and 1 by dividing by the maximum
      # average expression
      s <- (x / max.expression) * (y / max.expression)
      # add the value to the symmetric matrix
      sym_mat[i, j] <- s
    }
  }
  # set the row names and column names of the symmetric dataframe
  rownames(sym_mat) <- rows
  colnames(sym_mat) <- cols
  # convert to a symmetric matrix
  as.matrix(sym_mat)
}


calculate_maximum <- function(profiles) {
  #' List of profiles to calculate the maximum average value of variable across
  #' all profiles in the list.
  #'
  #' @param profiles A list of data frames or matrices
  #'
  #'
  v <- c()
  for (profile in profiles) {
    average.expression <- rowMeans(profile)
    v <- c(v, average.expression)
  }
  return(max(v))
}
