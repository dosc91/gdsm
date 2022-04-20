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
#' open_slides(02)
#'
#' @export

open_slides <- function(session) {

  num <- stringr::str_pad(session, 2, pad = '0')

  real_session <- session - 1

  if (real_session > 0 & real_session < 1) {

    utils::browseURL(sessionlinks[real_session])

  } else {

    cli::cli_alert_danger(

      glue::glue("I am not aware of slides for session {num}! Currently, I only know of slides for sessions ...!")

    )
  }
}


sessionlinks <- c(
  "",
  ""
)



