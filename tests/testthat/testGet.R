context("dependency download commands")

test_that("Non-transitive dependency download generates correct cmd option", {
  cmd <- .download_dependency_cmd('bar', 'com.foo', '1.0-SNAPSHOT', mvn = '/usr/bin/mvn', transitive = FALSE, dep_plugin_version = "3.1.1")
  expect_equal(cmd, '/usr/bin/mvn org.apache.maven.plugins:maven-dependency-plugin:3.1.1:get -Dartifact=com.foo:bar:1.0-SNAPSHOT -Dtransitive=false')
})

test_that("Dependency with no transitive arg is transitive by default", {
  cmd <- .download_dependency_cmd('bar', 'com.foo', '1.0-SNAPSHOT', mvn = '/usr/bin/mvn', dep_plugin_version = "3.1.1")
  expect_equal(cmd, '/usr/bin/mvn org.apache.maven.plugins:maven-dependency-plugin:3.1.1:get -Dartifact=com.foo:bar:1.0-SNAPSHOT -Dtransitive=true')
})