litedown::reactor(print = NA)

library(super)

bar <- "baz"
glue("foo{bar}")

dat <- head(cbind(car = rownames(mtcars), mtcars))
glue("{car} does {mpg} mpg.", dat)

name <- "Fred"
age <- 50
anniversary <- as.Date("1991-10-12")
out <- glut("
    My name is {name},
    my age next year is {age},
    my anniversary is {anniversary}.
")
cat(out)

head(glue("Item {LETTERS}"))

tryCatch(
    glue(letters),
    error = function(e) conditionMessage(e)
)

library(microbenchmark)

bar <- "baz"
bob <- 20

microbenchmark(
    sprintf    = sprintf("foo%s %d", bar, bob),
    paste0     = paste0("foo", bar, " ", bob),
    super   = super::glue("foo{bar} {bob}"),
    glue    = as.character(glue::glue_safe("foo{bar} {bob}", .trim = FALSE)),
    unit    = "relative",
    check   = "identical"
)

dat <- head(cbind(car = rownames(mtcars), mtcars))

microbenchmark(
    sprintf = with(dat, sprintf("%s does %.3g mpg.", car, mpg)),
    paste0  = with(dat, paste(car, "does", mpg, "mpg.")),
    super   = super::glue("{car} does {mpg} mpg.", dat),
    glue    = as.character(glue::glue_data(dat, "{car} does {mpg} mpg.")),
    unit    = "relative",
    check   = "identical"
)

microbenchmark(
    super   = super::glut("
                  My name is {name},
                  my age next year is {age},
                  my anniversary is {anniversary}.
              "),
    glue    = as.character(glue::glue("
                  My name is {name},
                  my age next year is {age},
                  my anniversary is {anniversary}.
              ")),
    unit    = "relative",
    check   = "identical"
)

bar <- rep("baz", 1e5)
microbenchmark(
    sprintf    = sprintf("foo%s %d", bar, bob),
    paste0     = paste0("foo", bar, " ", bob),
    super   = super::glue("foo{bar} {bob}"),
    glue    = as.character(glue::glue_safe("foo{bar} {bob}", .trim = FALSE)),
    unit    = "relative",
    check   = "identical"
)

