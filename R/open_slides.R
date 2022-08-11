#' Open Slides
#'
#' @description Calling \code{open_slides} with a session number as argument will open the slides for that session.
#' The slides will be opened in your default browser. This function is inspired by Schmitz & Esser (see references).
#'
#' @param session_number The number of the session.
#'
#' @author D. Schmitz
#'
#' @references Schmitz, D., & Esser, J. (2021). SfL: Statistics for Linguistics. R package version 0.3. URL: https://github.com/dosc91/SfL
#'
#' @examples
#' open_slides(01)
#'
#' @export

open_slides <- function(session) {

  num <- stringr::str_pad(session, 2, pad = '0')

  real_session <- session

  if (real_session > 0 & real_session < 4) {

    utils::browseURL(sessionlinks[real_session])

  } else if(session == 5) {

    real_session <- session - 1

    utils::browseURL(sessionlinks[real_session])

  } else if(session == 6){

    real_session <- session - 1

    utils::browseURL(sessionlinks[real_session])

  } else if(session == 8){

    real_session <- session - 2

    utils::browseURL(sessionlinks[real_session])

  } else {

    cli::cli_alert_danger(

      glue::glue("I am not aware of slides for session {num}! Currently, I only know of slides for sessions 1, 2, 3, 5, 6, and 8!")

    )
  }
}


sessionlinks <- c(
  "https://dosc91.github.io/gdsm/slides/01_Willkommen_und_Organisatorisches.pdf",
  "https://dosc91.github.io/gdsm/slides/02_Einfuhrung_in_die_Distributionelle_Semantik.pdf",
  "https://dosc91.github.io/gdsm/slides/03_Statistik_semantischer_Vektoren.pdf",
  "https://dosc91.github.io/gdsm/slides/05_fastText.pdf",
  "https://dosc91.github.io/gdsm/slides/06_Naive_Discriminative_Learning.pdf",
  "https://dosc91.github.io/gdsm/slides/08_Abschlussdiskussion.pdf"
)



