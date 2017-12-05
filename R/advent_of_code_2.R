#' Read Table Data to Matrux
#'
#' @param path [\code{character(1)}]\cr
#'   path to the file.
#'
#' @return [\code{matrix}]\cr 
#'   retrurns data in matrix format.
#' @export
#'
#' @examples readDataMatrix("data/advent_of_code_2.txt")
readDataMatrix <- function(path) {
  as.matrix(read.table(path))
}

#' Calcuate Difference in Spread
#'
#' @param x [\code{numeric}]\cr
#'   vector of numbers.
#'
#' @return [\code{numeric(1)}]\cr
#'   diference of maximum and minimum value.
#' @export
#'
#' @examples calculateDiffMaxMin(c(0, 2, 10, 1))
calculateDiffMaxMin <- function(x) {
  max(x) - min(x)
}

#' Ratio for Numbers Divisible without Remainder
#'
#' @param x [\code{numeric}]\cr
#'   vector of unique numbers.
#' @return
#' @export
#'
#' @examples calculateRatio(c(2, 9, 3))
calculateRatio <- function(x) {
  ratio_matrix <- sapply(x, function(y) {x / y})
  # only works if numbers are not equal
  ratio_matrix[ratio_matrix %% 1 == 0 & ratio_matrix > 1]
}

#' Apply and Sum to Matrix
#'
#' @param data_matrix [\code{matrix}]\cr
#'   numeric matrix.
#' @param check_fun [\code{function}]\cr
#'   aggregator function for each vector per margin.
#' @param margin [\code{integer(1)}]\cr
#'   margin to agrregate over: 1 for rows and 2 for columns.
#'
#' @return [\code{numeric(1)}]\cr
#'   sum of aggregator function results.
#' @export
#'
#' @examples applyAndSum(matrix(c(1, 2, 10, 0), ncol = 2), max)
applyAndSum <- function(data_matrix, check_fun, margin = 1) {
  sum(apply(data_matrix, margin, check_fun))
}

# calcuate results ------------------------------------------------------------
data_matrix <- readDataMatrix("data/advent_of_code_2.txt")

# part 1 ----------------------------------------------------------------------
applyAndSum(data_matrix, calculateDiffMaxMin)

# part 2 ----------------------------------------------------------------------
applyAndSum(data_matrix, calculateRatio)
