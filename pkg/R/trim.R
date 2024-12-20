# -------------------------------------------------------------------------
#' Trim a character vector
#'
# -------------------------------------------------------------------------
#' @description
#' Almost identical to [glue::trim()] save a slight difference in error
#' handling for non-character input. This function trims a character
#' vector according to the trimming rules used by glue. These follow similar
#' rules to [Python Docstrings](https://www.python.org/dev/peps/pep-0257/),
#' with the following features:
#'
#' - Leading and trailing whitespace from the first and last lines is removed.
#'
#' - A uniform amount of indentation is stripped from the second line on, equal
#'   to the minimum indentation of all non-blank lines after the first.
#'
#' - Lines can be continued across newlines by using `\\`.
#'
# -------------------------------------------------------------------------
#' @param x `[character]`.
#'
# -------------------------------------------------------------------------
#' @returns A character vector.
#'
# -------------------------------------------------------------------------
#' @examples
#' cat(trim("
#'     A formatted string
#'     Can have multiple lines
#'       with additional indentation preserved
#'     "))
#'
#' cat(trim("
#'   \ntrailing or leading newlines can be added explicitly\n
#'   "))
#'
#' cat(trim("
#'     A formatted string \\
#'     can also be on a \\
#'     single line
#'     "))
#'
# -------------------------------------------------------------------------
#' @seealso [glue::trim()].
#'
# -------------------------------------------------------------------------
#' @export
trim <- function(x) {
	if (!is.character(x))
		stop("`x` must be a character vector.")

	has_newline <- function(x) any(grepl("\\n", x))

	if (length(x) == 0 || !has_newline(x))
		return(x)

	.Call(C_trim, x)
}
