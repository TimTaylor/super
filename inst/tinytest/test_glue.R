# -------------------------------------------------------------------------
# Errors ------------------------------------------------------------------
# -------------------------------------------------------------------------

# glue errors with input length > 1
expect_error(glue(letters), "`x` must be a character vector of length <= 1.")

# glue errors with invalid second argument
expect_error(glue("test", "a"), "must be an environment")

# glue errors if the expression fails
expect_error(glue("{NoTfOuNd}"), "object .* not found")

# glue errors if invalid format
expect_error(glue("x={x"), "Expecting '}'")

# glue throws informative error if interpolating a function
expect_error(glue("{cat}"), "cannot interpolate functions into strings.")

# glue does not execute code
expect_error(glue("{1+1}"), "object '1\\+1' not found")

# glut errors with input length > 1
expect_error(glut(letters), "`x` must be a character vector of length <= 1.")

# glut errors with invalid second argument
expect_error(glut("test", "a"), "must be an environment")

# glut throws informative error if interpolating a function
expect_error(glut("{cat}"), "cannot interpolate functions into strings.")


# -------------------------------------------------------------------------
# Implementation ----------------------------------------------------------
# -------------------------------------------------------------------------

# character(0) input returns character(0)
expect_identical(glue(character(0L)), character(0L))

# glue returns length 1 string from length 1 input
expect_identical(glue(""), "")

# glue returns length 1 string from length 1 input
expect_identical(glue(""), "")

# glue works with single expressions
foo <- "foo"
expect_identical(glue("{foo}"), foo)

"1 + 1" <- 5
expect_identical(glue("{1 + 1}"), "5")

# glue can handle interesting names without quoting
`odd name\`` <- "indeed"
expect_identical(glue("{odd name`}"), "indeed")

# glue works with repeated expressions
foo <- "foo"
expect_identical(glue("{foo} {foo}"), paste(foo, foo))

foo <- 1L
expect_identical(glue("{foo} {foo}"), paste(foo, foo))

foo <- as.raw(1)
expect_identical(glue("{foo} {foo}"), paste(foo, foo))

foo <- TRUE
expect_identical(glue("{foo} {foo}"), paste(foo, foo))

foo <- as.Date("2016-01-01")
expect_identical(glue("{foo} {foo}"), paste(foo, foo))

# glue works with multiple expressions
foo <- "foo"
bar <- "bar"
expect_identical(glue("{foo} {bar}"), paste(foo, bar))

foo <- 1L
bar <- 2L
expect_identical(glue("{foo} {bar}"), paste(foo, bar))

foo <- as.raw(1)
bar <- as.raw(2)
expect_identical(glue("{foo} {bar}"), paste(foo, bar))

foo <- TRUE
bar <- FALSE
expect_identical(glue("{foo} {bar}"), paste(foo, bar))

foo <- as.Date("2016-01-01")
bar <- as.Date("2016-01-02")
expect_identical(glue("{foo} {bar}"), paste(foo, bar))

# doubled braces are converted to single braces
expect_identical(glue("{{foo}}"), "{foo}")

# glue works with large outputs
# initial buffer allocates input string length + 1024, 40 * 26 = 1040 # TODO - check this comment from glue tests
foo <- paste(rep(letters, 40), collapse = "")
# re-allocation on result # TODO - check this comment from glue tests
expect_identical(glue("{foo}"), foo)

# glue always returns UTF-8 encoded strings regardless of input encodings
x <- "fa\xE7ile"
Encoding(x) <- "latin1"
x_out <- enc2utf8(x)
expect_identical(glue(x), x_out)
expect_identical(glue("{x}"), x_out)
expect_equal(Encoding(glue(x)), "UTF-8")
expect_equal(Encoding(glue("{x}")), "UTF-8")

y <- "p\u00E4o"
Encoding(y) <- "UTF-8"
y_out <- enc2utf8(y)
expect_identical(glue(y), y_out)
expect_identical(glue("{y}"), y_out)
expect_equal(Encoding(glue(y)), "UTF-8")
expect_equal(Encoding(glue("{y}")), "UTF-8")

xy_out <- paste0(x_out, y_out)
expect_identical(glue("{x}{y}"), xy_out)
expect_equal(Encoding(glue("{x}{y}")), "UTF-8")


encoding_test <- function() {
    old <- Sys.getlocale("LC_CTYPE")
    on.exit(Sys.setlocale("LC_CTYPE", old)) # In case test interrupted
    Sys.setlocale("LC_CTYPE", "Chinese (Simplified)_China.936")
    z <- format(as.Date("2018-01-04"), "%Y\U5E74")
    z_out <- glue(z)
    expect_equal(Encoding(z_out), "UTF-8")
    expect_equal(z_out, "2018\U5E74")
}
os <- tolower(Sys.info()[["sysname"]])
if (!os %in% c("darwin", "linux")) {
    encoding_test()
}

# glue drops any NULL input
x <- NULL
expect_identical(glue("{x}"),character())

# glue works within functions
x <- 1
f <- function(msg) glue(msg, env = parent.frame())
expect_identical(f("{x}"), "1")

# scoping works within lapply (#42 in original glue repo)
f <- function(msg) glue(msg, env = parent.frame())
expect_identical(lapply(1:2, function(x) f("{x}")), list("1", "2"))

# glue works with list input
name <- "Fred"
res <- glue(
    'My name is {name}, my age next year is {age} and a dot is a {.}',
    list(name = "Joe", age = 40, . = "'.'")
)
expect_identical(res, "My name is Joe, my age next year is 40 and a dot is a '.'")

# glut works with list input
name <- "Fred"
res <- glut(
    'My name is {name}
    my age next year is {age}
    a dot is a {.}',
    list(name = "Joe", age = 40, . = "'.'")
)
expect_identical(res, "My name is Joe\nmy age next year is 40\na dot is a '.'")

# glut trims values before evaluation
x <- " a1\n b2\n c3"
expect_identical(
    glut("
  A
  {x}
  B
  "),
        "A
 a1
 b2
 c3
B"
)

# character(0) input returns character(0)
expect_identical(glut(character(0L)), character(0L))
