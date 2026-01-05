#' Kable Helper Functions
#'
#' Wrapper functions for kable with sensible defaults and automatic
#' output format detection.
#'
#' @name kable-helpers
NULL

#' Basic Kable Wrapper
#'
#' Kable with sensible defaults: auto-detected format, booktabs styling,
#' and hold_position for LaTeX.
#'
#' @param ... Arguments passed to knitr::kable
#' @param format Output format. If NULL (default), auto-detected.
#'
#' @return A kable object with styling applied
#' @export
#' @examples
#' \dontrun{
#' kable0(head(mtcars))
#' }
kable0 <- function(..., format = NULL) {
    if (is.null(format)) format <- get_kable_format()

    knitr::kable(...,
                 format = format,
                 booktabs = TRUE) |>
        kableExtra::kable_styling(latex_options = c("hold_position"))
}

#' Kable with Scale Down
#'
#' Kable wrapper that scales down wide tables to fit the page.
#'
#' @inheritParams kable0
#' @export
kable_scaled <- function(..., format = NULL) {
    if (is.null(format)) format <- get_kable_format()

    knitr::kable(...,
                 format = format,
                 booktabs = TRUE) |>
        kableExtra::kable_styling(
            latex_options = c("hold_position", "scale_down")
        )
}

#' Kable with Striped Rows
#'
#' Kable wrapper with alternating row colors for readability.
#'
#' @inheritParams kable0
#' @param font_size Font size for the table
#' @export
kable_striped <- function(..., format = NULL, font_size = NULL) {
    if (is.null(format)) format <- get_kable_format()

    tbl <- knitr::kable(...,
                        format = format,
                        booktabs = TRUE) |>
        kableExtra::kable_styling(
            latex_options = c("hold_position", "striped")
        )

    if (!is.null(font_size)) {
        tbl <- kableExtra::kable_styling(tbl, font_size = font_size)
    }

    tbl
}
