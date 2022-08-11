#' Open Exercises
#'
#' @description Calling \code{open_exercises} with a session number as argument will open the exercise html for that session.
#' The html will be opened in your default browser. This function is inspired by Schmitz & Esser (see references).
#'
#' @param session_number The number of the session.
#'
#' @author D. Schmitz
#'
#' @references Schmitz, D., & Esser, J. (2021). SfL: Statistics for Linguistics. R package version 0.3. URL: https://github.com/dosc91/SfL
#'
#' @examples
#' open_exercises(02)
#'
#' @export

open_exercises <- function(session) {

  num <- stringr::str_pad(session, 2, pad = '0')

  if (session > 2 & session < 7) {

    real_session <- session - 2

    utils::browseURL(exerciselinks[real_session])

  } else {

    cli::cli_alert_danger(

      glue::glue("I am not aware of exercises for session {num}! Currently, I only know of exercises for sessions 3, 4, 5, and 6!")

    )
  }
}


exerciselinks <- c(
  "https://dosc91.github.io/gdsm/exercises/Statistik_semantischer_Vektoren.html",
  "https://dosc91.github.io/gdsm/exercises/Visualisierung_semantischer_Vektoren.html",
  "https://dosc91.github.io/gdsm/exercises/fastText.html",
  "https://dosc91.github.io/gdsm/exercises/Naive_Discriminative_Learning.html"
)



