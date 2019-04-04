context("Test classpath finder")

test_that("Classpath properly parsed from mvn output", {
  expected_cp <- c("/m2repo/org/myorg/abdc-parsers/1.0.1-SNAPSHOT/abdc-parsers-1.0.1-SNAPSHOT.jar",
                   "/m2repo/org/myorg/abdc-native-model/1.0.1-SNAPSHOT/abdc-native-model-1.0.1-SNAPSHOT.jar",
                   "/m2repo/com/google/guava/guava/18.0/guava-18.0.jar",
                   "/m2repo/org/myorg/geolib/3.0.2/geolib-3.0.2.jar",
                   "/m2repo/org/myorg/geolibprimitive/1.0.1-SNAPSHOT/geolibprimitive-1.0.1-SNAPSHOT.jar",
                   "/m2repo/org/apache/logging/log4j/log4j-api/2.5/log4j-api-2.5.jar",
                   "/m2repo/org/apache/logging/log4j/log4j-core/2.5/log4j-core-2.5.jar")

  mvn_output <- c("[INFO] Scanning for projects...",
                  "[INFO]",
                  "[INFO] ------------------------------------------------------------------------",
                  "[INFO] Building tempproj 0.0.1-SNAPSHOT",
                  "[INFO] ------------------------------------------------------------------------",
                  "[INFO] ",
                  "[INFO] --- maven-dependency-plugin:2.8:build-classpath (default-cli) @ tempproj ---",
                  "[INFO] Dependencies classpath:",
                  paste0(expected_cp, collapse=ifelse(is_windows(), ";", ":")),
                  "[INFO] ------------------------------------------------------------------------",
                  "[INFO] BUILD SUCCESS",
                  "[INFO] ------------------------------------------------------------------------",
                  "[INFO] Total time: 2.233 s",
                  "[INFO] Finished at: 2017-02-06T11:43:59-05:00",
                  "[INFO] Final Memory: 13M/150M",
                  "[INFO] ------------------------------------------------------------------------")

  cp <- parse_classpath_from_mvn(mvn_output)
  expect_equal(cp, expected_cp)
})
