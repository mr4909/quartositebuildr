test_that("create_site_structure fails with incorrect input", {
  expect_error(create_site_structure(type = NULL), "'type' must be a character string.", fixed = TRUE)
  expect_error(create_site_structure(type = 123), "'type' must be a character string.", fixed = TRUE)
  expect_error(create_site_structure(type = TRUE), "'type' must be a character string.", fixed = TRUE)
  expect_error(create_site_structure(type = list("cfa")), "'type' must be a character string.", fixed = TRUE)
  expect_error(create_site_structure(type = c("cfa", "other")), "'type' must be a single character string.", fixed = TRUE)

  # expect_error(create_site_structure(type = "cfa", subfolder = NULL), "'subfolder' must be a character string.", fixed = TRUE)
  # expect_error(create_site_structure(type = "cfa", subfolder = 123), "'subfolder' must be a character string.", fixed = TRUE)
  # expect_error(create_site_structure(type = "cfa", subfolder = TRUE), "'subfolder' must be a character string.", fixed = TRUE)
  # expect_error(create_site_structure(type = "cfa", subfolder =  c("abc", "other")), "'subfolder' must be a single character string.", fixed = TRUE)
})

