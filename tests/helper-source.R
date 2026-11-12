# Run all tests from the orion-chat folder with testthat::test_dir("tests").
library(ellmer)
library(glue)
library(purrr)
library(stringr)
for (file in list.files("../R", pattern = "[.]R$", full.names = TRUE)) source(file)
