# -------------------------------------------------------------------------
#' Format and interpolate a string
#'
# -------------------------------------------------------------------------
#' @description
#'
#' Inputs enclosed by braces (e.g. `{name}`) are looked up in the provided
#' environment (akin to calling [get()]). Single braces can be escaped by
#' doubling them up. Variables are recycled to the length of the largest one.
#'
#' `glue()` operates on the string as is.
#'
#' `glut()` will [`trim`][super::trim] the input prior to glueing.
#'
# -------------------------------------------------------------------------
#' @param x `[character string]`
#'
#' @param env `[environment]`
#'
#' Where to look up the embraced input.
#'
#' Can be an environment or a list-like object that will be converted in the
#' underlying function via `list2env()`.
#'
# -------------------------------------------------------------------------
#' @return
#' A `character` object.
#'
# -------------------------------------------------------------------------
#' @seealso
#' [glue::glue_safe()] and [glue::glue_data_safe()] on which which this
#' function is an evolution.
#'
# -------------------------------------------------------------------------
#' @examples
#' name <- "Fred"
#' age <- 50
#' cat(glue("My name is {name} and my age next year is {age}"))
#'
#' # glut first trims the output
#' anniversary <- as.Date("1991-10-12")
#' cat(glut("
#'     My name is {name},
#'     my age next year is {age},
#'     my anniversary is {anniversary}.
#' "))
#'
#' # single braces can be inserted by doubling them
#' glue("My name is {name}, not {{name}}.")
#'
#' # List like objects can be used in place of an environment
#' dat <- cbind(car = rownames(mtcars), mtcars)
#' glue("{car} does {mpg} mpg.", dat)
#'
# -------------------------------------------------------------------------
#' @export
glue <- function(x, env = parent.frame()) {

	if (!is.character(x) || length(x) > 1)
		stop("`x` must be a character vector of length <= 1.")

	if (!length(x))
		return (character(0L))

	if (!is.environment(env)) {
		if (!is.list(env))
			stop("`env` must be an environment or list like object.")
		env <- list2env(env)
	}

	on.exit(.Call(C_glue_free))
	res <- .Call(C_glue, x, env)
	if (any(vapply(res, is.function, TRUE)))
		stop("`glue()` cannot interpolate functions into strings.")
	do.call(paste0, res)
}

#' @rdname glue
#' @export
glut <- function(x, env = parent.frame()) {

	if (!is.character(x) || length(x) > 1)
		stop("`x` must be a character vector of length <= 1.")

	if (!length(x))
		return (character(0L))

	if (!is.environment(env)) {
		if (!is.list(env))
			stop("`env` must be an environment or list like object.")
		env <- list2env(env)
	}

	x <- trim(x)

	on.exit(.Call(C_glue_free))
	res <- .Call(C_glue, x, env)
	if (any(vapply(res, is.function, TRUE)))
		stop("`glue()` cannot interpolate functions into strings.")
	do.call(paste0, res)
}

