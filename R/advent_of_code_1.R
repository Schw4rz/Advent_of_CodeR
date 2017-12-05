#' Read String Data to Vector
#'
#' @param path [\code{character(1)}]\cr
#'   path to the file.
#'
#' @return [\code{vector}]\cr 
#'   returns data in as vector of single numbers.
#' @export
#'
#' @examples readDataVec("data/advent_of_code_1.txt")
readDataVec <- function(path) {
  as.numeric(strsplit(readLines(path), "")[[1]])
}

#' Sum by Match
#'
#' @param in_vec [\code{numeric}]\cr
#'   numeric vector.
#' @param distance [\code{integer(1)}]\cr
#'   distance to compare with.
#' @return [\code{integer(1)}]\cr
#'   sum of all numbers where number and number \code{distance} positions ahead
#'   match.
#' @export
#'
#' @examples calculateSumByMatch("1221", 1)
calculateSumByMatch <- function(in_vec, distance) {
  pad_vec_idx <- seq_len(distance)
  in_vec_lag <- c(in_vec[-pad_vec_idx], in_vec[pad_vec_idx])
  sum(in_vec[in_vec == in_vec_lag])
}

# calcuate results ------------------------------------------------------------
in_vec <- readDataVec("data/advent_of_code_1.txt")

# part 1
calculateSumByMatch(in_vec, 1)

# part 2
calculateSumByMatch(in_vec, length(in_vec) / 2)
