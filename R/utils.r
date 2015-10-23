validate_port <- function(port)
{
  assert_that(is.count(port))
  assert_that(port < 65536)
}

