context("pom.xml generation")

test_that("1-dependency pom write out correctly", {
  pom <- write_pom(deps = "test-group:test-dep:test-version")
  pomlines <- readLines(pom)

  expectedlines <- c("<project xmlns=\"http://maven.apache.org/POM/4.0.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"",
                     "xsi:schemaLocation=\"http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd\">",
                     "\t<modelVersion>4.0.0</modelVersion>",
                     "",
                     "\t<artifactId>temp-proj</artifactId>",
                     "\t<groupId>temp-group</groupId>",
                     "\t<version>0.0.1-SNAPSHOT</version>",
                     "",
                     "\t<dependencies>",
                     "\t\t<dependency>",
                     "\t\t\t<groupId>test-group</groupId>",
                     "\t\t\t<artifactId>test-dep</artifactId>",
                     "\t\t\t<version>test-version</version>",
                     "\t\t</dependency>",
                     "\t</dependencies>",
                     "</project>")

  expect_equal(pomlines, expectedlines)
  unlink(pom)
})

test_that("3-dependency pom write out correctly", {
  pom <- write_pom(dep = c("test-group1:test-dep1:test-version1",
                           "test-group2:test-dep2:test-version2",
                           "test-group3:test-dep3:test-version3"))
  pomlines <- readLines(pom)

  expectedlines <- c("<project xmlns=\"http://maven.apache.org/POM/4.0.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"",
                     "xsi:schemaLocation=\"http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd\">",
                     "\t<modelVersion>4.0.0</modelVersion>",
                     "",
                     "\t<artifactId>temp-proj</artifactId>",
                     "\t<groupId>temp-group</groupId>",
                     "\t<version>0.0.1-SNAPSHOT</version>",
                     "",
                     "\t<dependencies>",
                     "\t\t<dependency>",
                     "\t\t\t<groupId>test-group1</groupId>",
                     "\t\t\t<artifactId>test-dep1</artifactId>",
                     "\t\t\t<version>test-version1</version>",
                     "\t\t</dependency>",
                     "\t\t<dependency>",
                     "\t\t\t<groupId>test-group2</groupId>",
                     "\t\t\t<artifactId>test-dep2</artifactId>",
                     "\t\t\t<version>test-version2</version>",
                     "\t\t</dependency>",
                     "\t\t<dependency>",
                     "\t\t\t<groupId>test-group3</groupId>",
                     "\t\t\t<artifactId>test-dep3</artifactId>",
                     "\t\t\t<version>test-version3</version>",
                     "\t\t</dependency>",
                     "\t</dependencies>",
                     "</project>")

  expect_equal(pomlines, expectedlines)
  unlink(pom)
})
