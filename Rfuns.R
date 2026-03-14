# Collection of R functions

#' Get all unique row-column combinations from a symmetric N x N matrix
#'
#' When performing comparison between a vector and itself,
#' often we can organize the data in an N x N matrix or use
#' `base::expand.grid` to obtain all possible combinations.
#' However, because (N_1, N_2) is equivalent to (N_2, N_1),
#' this representation can lead to inefficiencies with larger
#' vectors as it performs about double the calculations needed.
#'
#' @param N The length of the vector. Must be a single integer value. Easiest to specify with `length(x)`.
#'
#' @return List with two integer vectors, representing the matched indices for comparisons.
#'
idx_combos <- function(N) {
  # Checks
  if (length(N) != 1L) {
    stop("N must be a single value.")
  }
  if (inherits(N, "integer") || inherits(N, "numeric")) {
    if (identical(round(N), N)) {
      N <- as.integer(N)
    } else {
      stop("N must be an integer or a numeric coercible to an integer.")
    }
  }
  if (N < 2L) {
    warning("N is 1 so no comparisons are possible")
    return(NA_integer_)
  }

  cbind(
    X1 = sequence(seq(1, N - 1)),
    X2 = rep(seq(2, N), times = seq(1, N - 1))
  )
}



#' Applies a function to a combinations of column indices
#'
#' Used in conjuction with `idx_combos`, this takes the data in matrix(-like) form and
#' applies a function to it.
#'
#' @param mat The data in matrix(-like) form.
#' @param idxs The matrix of indices produced by `idx_combos`.
#' @param FUN The function to apply.
#'
#' @return The `idxs` matrix with the results in the new function.
#'
apply2idx <- function(mat, idx, FUN) {
  if (max(idx) != ncol(mat)) {
    stop("Columns in mat and values in idxs do not match for this task.")
  }
  cbind(
    idx,
    mapply(
      function(x, y) do.call(FUN, list(mat[, x], mat[, y])),
      idx[, 1], idx[, 2]
    )
  )
}
