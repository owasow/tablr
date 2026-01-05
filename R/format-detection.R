#' Output Format Detection
#'
#' These functions detect the current R Markdown output format and return
#' the appropriate format string for various table packages.
#'
#' @name format-detection
#' @return A character string indicating the output format
NULL

#' Detect format for stargazer
#'
#' Returns "latex", "html", or "text" depending on knitr output format.
#'
#' @export
#' @examples
#' get_star_format()
get_star_format <- function() {
    dplyr::case_when(
        knitr::is_latex_output() ~ "latex",
        knitr::is_html_output()  ~ "html",
        TRUE                     ~ "text"
    )
}

#' Detect format for xtable
#'
#' Returns "latex" or "html" depending on knitr output format.
#'
#' @export
#' @examples
#' get_xtable_format()
get_xtable_format <- function() {
    dplyr::case_when(
        knitr::is_latex_output() ~ "latex",
        TRUE                     ~ "html"
    )
}

#' Detect format for kable
#'
#' Returns "latex", "html", or "markdown" depending on knitr output format.
#'
#' @export
#' @examples
#' get_kable_format()
get_kable_format <- function() {
    dplyr::case_when(
        knitr::is_latex_output() ~ "latex",
        knitr::is_html_output()  ~ "html",
        TRUE                     ~ "markdown"
    )
}
