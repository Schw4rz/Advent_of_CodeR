#' Advent of Code 1 - Sum by Match
#'
#' @param input [\code{character(1)}]\cr
#'   input as single character.
#' @param distance [\code{integer(1)}]\cr
#'   distance to compare with.
#' @return input [\code{integer(1)}]\cr
#'   sum of all numbers where number and number \code{distance} positions ahed
#'   match.
#' @export
#'
#' @examples calculateSumByMatch("1221", 1)
calculateSumByMatch <- function(input, distance) {
  in_vec <- as.numeric(strsplit(input, "")[[1]])
  pad_vec_idx <- seq_len(distance)
  in_vec_lag <- c(in_vec[-pad_vec_idx], in_vec[pad_vec_idx])
  sum(in_vec[in_vec == in_vec_lag])
}

# calcuate results ------------------------------------------------------------
input <- readLines("data/advent_of_code_1.txt")

# part 1
calculateSumByMatch(input, 1)

# part 2
calculateSumByMatch(input, nchar(input) / 2)
