test_that("cannot append with the wrong headings", {
  tf <- tempfile()
  write_tsv_at(iris, tf)
  expect_error(
    write_tsv_at(iris["Petal.Length"], tf, append = T),
    regex = "Could not append to file.+ not equivalent."
  )
})
