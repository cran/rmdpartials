context("Test nested partials")

knitr::opts_chunk$set(error = FALSE)

test_that("Nesting documents", {
  wd <- getwd()
  test_dir <- tempfile("testing_rmdpartials")
  dir.create(test_dir)
  setwd(test_dir)
  on.exit({
    setwd(wd)
    unlink(test_dir, recursive = TRUE)
  })
  cat(
"
0
```{r}
partial('one.Rmd')
```
", file = "zero.Rmd")

  cat(
"
1
```{r}
partial('two.Rmd')
```
", file = "one.Rmd")

  cat(
    "
2
```{r}
partial('three.Rmd')
```
", file = "two.Rmd")

  cat(
    "
3
", file = "three.Rmd")

  text <- paste0(readLines("zero.Rmd"), collapse = "\n")

  expect_silent(md <- knitr::knit(text = text, quiet = TRUE))

  expect_match(md, "0")
  expect_match(md, "1")
  expect_match(md, "2")
  expect_match(md, "3")

  unlink(test_dir, recursive = TRUE)
})


setup_files <- function() {
  test_dir <- tempfile("test_rmdpartials")
  stopifnot(dir.create(test_dir))
  setwd(test_dir)
  test_dir <- getwd() # dumb trick to get a proper path without double slashes

  cat(
    "
test0
```{r}
plot(0)
partial('one.Rmd')
```
", file = "zero.Rmd")

  cat(
    "
test1
```{r}
plot(1)
partial('two.Rmd')
```
", file = "one.Rmd")

  cat(
    "
test2
```{r}
plot(2)
partial('three.Rmd')
```
", file = "two.Rmd")

  cat(
    "
test3
```{r}
plot(3)
```
", file = "three.Rmd")

  test_dir
}

test_that("Nesting with plots knitr", {
  test_dir <- setup_files()
  on.exit({
    unlink(test_dir, recursive = TRUE)
  })
  text <- paste0(readLines("zero.Rmd"), collapse = "\n")

  expect_silent(md <- knitr::knit(text = text, quiet = TRUE))

  expect_match(md, "test0", fixed = TRUE)
  expect_match(md, "test1", fixed = TRUE)
  expect_match(md, "test2", fixed = TRUE)
  expect_match(md, "test3", fixed = TRUE)
  expect_equal(4, length(list.files(
    file.path(test_dir, "figure"))))
})

test_that("Nesting with plots rmarkdown", {
  test_dir <- setup_files()
  on.exit({
    unlink(test_dir, recursive = TRUE)
  })

  expect_message(file <- rmarkdown::render("zero.Rmd",
                                           output_format =
                                             rmarkdown::html_document(
                                               self_contained = FALSE)
                                           ))

  html <- paste0(readLines(file), collapse = "\n")

  expect_match(html, "test0", fixed = TRUE)
  expect_match(html, "test1", fixed = TRUE)
  expect_match(html, "test2", fixed = TRUE)
  expect_match(html, "test3", fixed = TRUE)

  # no new temp directory generated when a top-level document exists
  expect_equal(dirname(file), test_dir)

  expect_equal(4, length(list.files(
    file.path(test_dir, "zero_files/figure-html"))))
})

test_that("Nesting with plots partial", {
  test_dir <- setup_files()
  on.exit({
    unlink(test_dir, recursive = TRUE)
    Sys.unsetenv("TESTTHAT_interactive")
  })

  Sys.setenv(TESTTHAT_interactive = "true")
  expect_silent(md <- partial("zero.Rmd"))
  output.dir <- attributes(md)$knit_meta$output.dir

  expect_error(expect_equal(output.dir, test_dir))

  expect_match(md, "test0", fixed = TRUE)
  expect_match(md, "test1", fixed = TRUE)
  expect_match(md, "test2", fixed = TRUE)
  expect_match(md, "test3", fixed = TRUE)
  expect_equal(4, length(list.files(
    file.path(output.dir, "index_files/figure-html"))))

  unlink(test_dir, recursive = TRUE)
  unlink(output.dir, recursive = TRUE)
})

