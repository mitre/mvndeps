mvndeps
================

A package to simplify how java dependencies are handled. See <https://mitre.github.io/mvndeps/> for documentation.

Use
---

Java and R work reasonably well together via the `rJava` package. However it can be difficult to manage java dependencies with R packages. In R packages the easiest solution has been to provide a compiled jar with the package in `inst/java`. This however does not work particularly well with version control and may require several large artifacts shipping with an R package. Furthermore, if several R packages all use the same java dependency, either those packages need to declare dependencies on one another and be intentional about sharing the artifact or else the java dependencies will be replicated on the user's system.

These problems are resolved using `mvndeps`. This package uses [Apache Maven](https://maven.apache.org/) to let java dependencies be managed in a reasonably efficient way and separately from the R code using those dependencies.

Prerequisite
------------

The `mvndeps` package **will not** install maven on your system. It will however try to find maven on your system if it has already been installed. To use `mvndeps`, and therefore any package that relies on `mvndeps` to manage java dependencies, it is necessary that the user first install Apache Maven on his or her system.

Key Features
------------

The primary feature is that, using `get_classpath` and frieds, the java classpath required to properly initilize `rJava` (e.g., using `rJava::.jinit(classpath=...)` or `rJava::.jpackage(..., morePaths=...)`) is easily found. These functions will also trigger java dependencies (and transitive dependencies) to be downloaded if they are not found in the local maven repository.

Though the motivating force behind `mvndeps` is to simplify how R packages are developed and maintained, it works well outside that context also. For example, the `download_dependency` function and `get_dependency_path` allow users to retrieve and find the local path to a required java dependency, respectively.

Installation
------------

``` r
# install.packages("devtools")
devtools::install_github("mitre/mvndeps")
```
