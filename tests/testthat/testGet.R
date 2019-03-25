context("dependency download commands")

test_that("Non-transitive dependency download generates correct cmd option", {
  args <- .dependency_get_args("com.foo:bar:1.0-SNAPSHOT", dep_plugin_version = "3.1.1", transitive = FALSE)
  expected_args <- c("org.apache.maven.plugins:maven-dependency-plugin:3.1.1:get",
                     "-Dartifact=com.foo:bar:1.0-SNAPSHOT",
                     "-Dtransitive=false")
  expect_equal(args, expected_args)
})

test_that("Dependency with no transitive arg is transitive by default", {
  args <- .dependency_get_args("com.foo:bar:1.0-SNAPSHOT", dep_plugin_version = "3.1.1")
  expected_args <- c("org.apache.maven.plugins:maven-dependency-plugin:3.1.1:get",
                     "-Dartifact=com.foo:bar:1.0-SNAPSHOT",
                     "-Dtransitive=true")
  expect_equal(args, expected_args)
})
