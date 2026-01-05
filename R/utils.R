#' Utility Functions
#'
#' Miscellaneous helper functions for formatting values in tables.
#'
#' @name utils
NULL

#' Add Comma Separators to Numbers
#'
#' Formats numbers with comma separators for thousands.
#'
#' @param x Numeric vector
#' @param ... Additional arguments passed to format()
#'
#' @return Character vector with formatted numbers
#' @export
#' @examples
#' add_comma(1234567)
#' add_comma(c(1000, 2000000, 500))
add_comma <- function(x, ...) {
    format(x, ..., big.mark = ",", scientific = FALSE, trim = TRUE)
}

#' Convert Number to Word
#'
#' Converts small integers (1-10) to their word equivalents.
#'
#' @param x Integer vector
#' @return Character vector with numbers as words (for 1-10) or as-is
#' @export
#' @examples
#' number_to_word(3)
#' number_to_word(c(1, 5, 15))
number_to_word <- function(x) {
    words <- c("one", "two", "three", "four", "five",
               "six", "seven", "eight", "nine", "ten")
    ifelse(x > 10 | x < 1, as.character(x), words[x])
}

#' Round to One Decimal Place
#'
#' @param x Numeric vector
#' @return Numeric vector rounded to 1 decimal place
#' @export
#' @examples
#' round1(3.456)
round1 <- function(x) {
    round(as.numeric(x), 1)
}

#' Round to Two Decimal Places
#'
#' @param x Numeric vector
#' @return Numeric vector rounded to 2 decimal places
#' @export
#' @examples
#' round2(3.456)
round2 <- function(x) {
    round(as.numeric(x), 2)
}

#' Replace NA with Dash
#'
#' Converts NA values to "-" for table display.
#'
#' @param x Vector to process
#' @return Character vector with NAs replaced by "-"
#' @export
#' @examples
#' na_to_dash(c(1, NA, 3))
na_to_dash <- function(x) {
    x <- as.character(x)
    ifelse(is.na(x) | x == "NA", "-", x)
}

#' Replace NA with Blank
#'
#' Converts NA values to empty string for table display.
#'
#' @param x Vector to process
#' @return Character vector with NAs replaced by ""
#' @export
#' @examples
#' na_to_blank(c(1, NA, 3))
na_to_blank <- function(x) {
    x <- as.character(x)
    ifelse(is.na(x) | x == "NA", "", x)
}
