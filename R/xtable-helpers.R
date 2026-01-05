#' Xtable Helper Functions
#'
#' Wrapper functions for xtable with automatic output format detection.
#'
#' @name xtable-helpers
NULL
#' Print Xtable with Auto Format Detection
#'
#' Prints an xtable with automatic format detection. For HTML output,
#' displays in RStudio viewer if available.
#'
#' @param x An object that can be coerced to xtable
#' @param ... Additional arguments passed to print.xtable
#'
#' @export
#' @examples
#' \dontrun{
#' print_xtable(head(mtcars))
#' }
print_xtable <- function(x, ...) {

    xtable_form <- get_xtable_format()
    xtab <- xtable::xtable(x)

    if (xtable_form == "latex") {
        print(xtab, ...)
    }

    if (xtable_form == "html") {
        temp_file <- tempfile(fileext = ".html")
        print(xtab, file = temp_file, type = xtable_form, ...)

        # Try to display in RStudio viewer if available
        if (requireNamespace("rstudioapi", quietly = TRUE) &&
            rstudioapi::isAvailable()) {
            rstudioapi::viewer(temp_file)
        } else {
            utils::browseURL(temp_file)
        }
    }
}
