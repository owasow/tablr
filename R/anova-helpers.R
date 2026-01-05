#' ANOVA Table Helpers
#'
#' Functions for formatting and printing ANOVA tables.
#'
#' @name anova-helpers
NULL

#' Convert Model Terms for ANOVA Display
#'
#' Converts technical model term names to human-readable labels,
#' specialized for ANOVA table output.
#'
#' @param x Character vector of model terms
#' @return Character vector with human-readable labels
#' @export
convert_model <- function(x) {

    # Normalize wave suffixes
    x <- .norm_strip_wave(x)

    x |>
        stringr::str_replace("\\*", "x") |>

        # Party ID
        stringr::str_replace("^pid3_16Dem$", "Party ID: Dem") |>
        stringr::str_replace("^pid3_16Ind$", "Party ID: Ind") |>
        stringr::str_replace("^pid3_16Rep$", "Party ID: Rep") |>
        stringr::str_replace("^pid3_16$", "Party ID (excl. Other)") |>
        stringr::str_replace("^pid(?:3|4)(?:_(?:16|20|24))?Dem$", "Party ID: Dem") |>
        stringr::str_replace("^pid(?:3|4)(?:_(?:16|20|24))?Ind$", "Party ID: Ind") |>
        stringr::str_replace("^pid(?:3|4)(?:_(?:16|20|24))?Rep$", "Party ID: Rep") |>
        stringr::str_replace("^pid(?:3|4)(?:_(?:16|20|24))?Other$", "Party ID: Other") |>
        stringr::str_replace("^pid(?:3|4)(?:_(?:16|20|24))?$", "Party ID") |>

        # Other political
        stringr::str_replace("^ideo7(?:_20)?$", "Ideology (1-7)") |>
        stringr::str_replace("^ft_clinton$", "Feeling Therm: Clinton") |>
        stringr::str_replace("^ft_trump20$", "Feeling Therm: Trump 2020") |>
        stringr::str_replace("^ft_trump$", "Feeling Therm: Trump 2016") |>

        # Demographics
        stringr::str_replace("mode[wW]eb", "Survey Mode") |>
        stringr::str_replace("female(?:16|20|24)?", "Female") |>
        stringr::str_replace("educ$|education(?:16|20|24)?", "Education") |>
        stringr::str_replace("^age$", "Age") |>
        stringr::str_replace("income(?:16|20|24)?", "Income") |>
        stringr::str_replace("pol_attn(?:16|20|24)?", "Political Attention") |>

        # Race
        stringr::str_replace("race.*black", "Race: Black") |>
        stringr::str_replace("race.*hispanic", "Race: Hispanic") |>
        stringr::str_replace("race.*other", "Race: Other") |>
        stringr::str_replace("race.*white", "Race: White") |>
        stringr::str_replace("race.*native_american", "Race: Native American") |>
        stringr::str_replace("race4", "Race") |>

        # Attitudes
        stringr::str_replace("racial_resent(?:16|20|24)?", "Racial Resentment") |>
        stringr::str_replace("sexism(?:16|20|24)?", "Hostile Sexism") |>
        stringr::str_replace("authorit(?:16|20|24)?", "Authoritarianism") |>

        # Outcomes
        stringr::str_replace("vote_confirmed", "Validated Turnout 2016") |>
        stringr::str_replace("turnout20_bin", "Validated Turnout 2020") |>
        stringr::str_replace("vote_pres_dem", "Vote Clinton") |>
        stringr::str_replace("vote_pres_rep", "Vote Trump") |>

        # Study design
        stringr::str_replace("reg_intent", "Reg/Vote Status") |>
        stringr::str_replace("likely_vote", "Likely Vote") |>
        stringr::str_replace("mode", "Mode")
}

#' Print Model List for ANOVA Tables
#'
#' Generates a LaTeX itemize list describing the models used in an ANOVA.
#'
#' @param anova_model An anova object from stats::anova()
#'
#' @return Character string with LaTeX markup for model list
#' @export
print_models <- function(anova_model) {

    # Get the chunk name automatically if in knitr context
    chunk_name <- tryCatch(
        knitr::opts_current$get("label"),
        error = function(e) "anova-table"
    )
    if (is.null(chunk_name)) chunk_name <- "anova-table"

    tidy_model <- broom::tidy(anova_model)
    tidy_model$term <- convert_model(tidy_model$term)

    # Using glue_data for dynamic number of models
    models <- glue::glue_data(
        tidy_model,
        "\\item Model {1:nrow(tidy_model)}: {term}"
    )

    # Replace tilde with latex \sim
    models <- stringr::str_replace_all(models, "~", "$\\\\sim$")

    # Print the results with header and within itemize list
    glue::glue_collapse(
        c("\\noindent Models used in Table \\ref{tab:", chunk_name, "}
\\begin{itemize}
", models,
          "\\end{itemize}"
        ),
        sep = ""
    )
}

#' Print Formatted ANOVA Table
#'
#' Formats an ANOVA table for LaTeX output with proper p-value formatting.
#'
#' @param anova_model An anova data frame
#' @param caption Table caption
#'
#' @return A kable object
#' @export
print_anova <- function(anova_model, caption) {

    anova_model |>
        dplyr::mutate(
            `Pr(>Chi)` = scales::pvalue(`Pr(>Chi)`, accuracy = 0.00001)
        ) |>
        dplyr::mutate(
            dplyr::across(
                dplyr::any_of(c("Resid. Dev", "Deviance")),
                round2
            )
        ) |>
        dplyr::mutate(
            dplyr::across(
                dplyr::any_of(c("Df", "Deviance", "Pr(>Chi)")),
                na_to_blank
            )
        ) |>
        knitr::kable(
            format = "latex",
            booktabs = TRUE,
            digits = 20,
            escape = FALSE,
            caption = caption
        ) |>
        kableExtra::kable_styling(
            latex_options = c("hold_position")
        )
}
