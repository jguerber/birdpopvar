#' catchConditions
#'
#' A function that executes codes and saves all warnings/errors encountered
#'
#' @param expr an expression that could be raising warnings or errors
#' @author Mathilde Vimont
#'
#' @return a list of 3 objects :
#' - value, an object resulting from the expression when possible
#' - warnings, a `vector` of messages associated with the warnings. Can be `NULL`.
#' - error, a `string` containing the message associated with the error. Can be `NULL`.
#'
#' @export
catchConditions <- function(expr) {

  # Initialisation of the results, warning and error variables
  resValue <- NULL
  warnValue <- NULL
  errValue <- NULL
  messageValue <- NULL

  # Function that saves all encountered warnings
  wHandler <- function(w) {
    warnValue <<- c(warnValue, w$message) # double arrow modifies variables in parent level
    invokeRestart("muffleWarning")
  }

  # Function that saves encountered error
  eHandler <- function(e) {
    errValue <<- e$message
    NULL
  }

  messageHandler <- function(m) {
    messageValue <<- m$message
    invokeRestart("muffleMessage")
  }

  # Execute expression and saves warnings/errors if encountered
  resValue <- tryCatch(
    withCallingHandlers(
      expr,
      warning = wHandler,
      message = messageHandler
    ), error = eHandler
  )

  return(list(value = resValue,
              warnings = warnValue,
              error = errValue,
              message = messageValue))

}


#' Call a function with arguments inside catchConditions()
try_safe <- function(f, ...) {
  out <- catchConditions(
    f(...)
  )

  if (length(out$error) == 0) {
    result <- out$value
  } else {
    result <- NULL
  }

  return(result)
}
