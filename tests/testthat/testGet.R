context("dependency download commands")

test_that("Non-transitive dependency download generates correct cmd option", {
  cmd <- .download_dependency_cmd('bar','com.foo','1.0-SNAPSHOT',mvn='/usr/bin/mvn', java_home='/usr/bin/java', transitive=FALSE)
  expect_equal(cmd,'/usr/bin/mvn dependency:get -Dartifact=com.foo:bar:1.0-SNAPSHOT -Dtransitive=false')
})

test_that("Dependency with no transitive arg is transitive by default", {
  cmd <- .download_dependency_cmd('bar','com.foo','1.0-SNAPSHOT',mvn='/usr/bin/mvn', java_home='/usr/bin/java')
  expect_equal(cmd,'/usr/bin/mvn dependency:get -Dartifact=com.foo:bar:1.0-SNAPSHOT -Dtransitive=true')
})