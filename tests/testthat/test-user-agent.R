test_that("cancensus calls identify tongfen in the user agent", {
  ua <- paste0("tongfen/", utils::packageVersion("tongfen"))
  withr::local_options(cancensus.user_agent_prefix = NULL)
  expect_equal(with_tongfen_user_agent(getOption("cancensus.user_agent_prefix")), ua)
  expect_null(getOption("cancensus.user_agent_prefix"))
})

test_that("a user set user agent prefix is kept and restored", {
  ua <- paste0("tongfen/", utils::packageVersion("tongfen"))
  withr::local_options(cancensus.user_agent_prefix = " myscript/1.0 ")
  expect_equal(with_tongfen_user_agent(getOption("cancensus.user_agent_prefix")),
               paste("myscript/1.0", ua))
  expect_equal(getOption("cancensus.user_agent_prefix"), " myscript/1.0 ")
  withr::local_options(cancensus.user_agent_prefix = "")
  expect_equal(tongfen_user_agent_prefix(), ua)
})

test_that("the option is restored when the cancensus call fails", {
  withr::local_options(cancensus.user_agent_prefix = NULL)
  expect_error(with_tongfen_user_agent(stop("boom")), "boom")
  expect_null(getOption("cancensus.user_agent_prefix"))
})
