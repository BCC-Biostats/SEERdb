library(testthat)
library(dplyr)

# Sample data for testing
test_data <- tibble(
  ID = c(1, 1, 1, 2, 2),
  start_date = as.Date(c("2022-01-01", "2022-01-01", "2022-01-01", "2022-01-01", "2022-01-01")),
  end_date = as.Date(c("2022-12-31", "2022-12-31", "2022-12-31", "2022-12-31", "2022-12-31")),
  claim_start_date = as.Date(c("2022-01-15", "2022-06-01", "2022-12-01", "2022-05-15", "2022-08-01")),
  claim_end_date = as.Date(c("2022-01-20", "2022-06-10", "2022-12-05", "2022-05-20", "2022-08-10")),
  claim_type = c("M", "M", "M", "M", "M"),
  dx1 = c("410", "428", "I21", "J44", "I110"),
  dx2 = c(NA, NA, "I252", NA, NA)
)

# Basic test for correct filtering with no ruleout
test_that("comorb function filters claims within the window correctly without ruleout", {
  result <- comorb(test_data, id = "ID", start_date = "start_date", end_date = "end_date",
                   claim_start_date = "claim_start_date", claim_end_date = "claim_end_date",
                   claim_type = "claim_type", dx_var_list = c("dx1", "dx2"), ruleout = FALSE)

  expect_true(nrow(result) > 0)
  expect_equal(n_distinct(result$ID), 2)
})

# Test for ruleout logic
test_that("comorb function applies ruleout logic correctly", {
  result <- comorb(test_data, id = "ID", start_date = "start_date", end_date = "end_date",
                   claim_start_date = "claim_start_date", claim_end_date = "claim_end_date",
                   claim_type = "claim_type", dx_var_list = c("dx1", "dx2"), ruleout = TRUE)

  expect_true(all(result$claim_start_date >= result$start_date - 30))
})

# Test for ICD-9 code detection
test_that("comorb function detects ICD-9 codes correctly", {
  result <- comorb(test_data, id = "ID", start_date = "start_date", end_date = "end_date",
                   claim_start_date = "claim_start_date", claim_end_date = "claim_end_date",
                   claim_type = "claim_type", dx_var_list = c("dx1", "dx2"), ruleout = FALSE)

  expect_equal(result$acute_mi[1], 1)
  expect_equal(result$chf[2], 1)
})

# Test for ICD-10 code detection
test_that("comorb function detects ICD-10 codes correctly", {
  result <- comorb(test_data, id = "ID", start_date = "start_date", end_date = "end_date",
                   claim_start_date = "claim_start_date", claim_end_date = "claim_end_date",
                   claim_type = "claim_type", dx_var_list = c("dx1", "dx2"), ruleout = FALSE)

  expect_equal(result$acute_mi[3], 1)
  expect_equal(result$history_mi[3], 1)
})

# Test for Charlson score calculation
test_that("comorb function calculates Charlson score correctly", {
  result <- comorb(test_data, id = "ID", start_date = "start_date", end_date = "end_date",
                   claim_start_date = "claim_start_date", claim_end_date = "claim_end_date",
                   claim_type = "claim_type", dx_var_list = c("dx1", "dx2"), ruleout = FALSE)

  expect_equal(result$Charlson[1], 1)  # acute_mi = 1
  expect_equal(result$Charlson[2], 1)  # chf = 1
})

# Test for NCI index calculation
test_that("comorb function calculates NCI index correctly", {
  result <- comorb(test_data, id = "ID", start_date = "start_date", end_date = "end_date",
                   claim_start_date = "claim_start_date", claim_end_date = "claim_end_date",
                   claim_type = "claim_type", dx_var_list = c("dx1", "dx2"), ruleout = FALSE)

  expect_equal(result$NCI_index[1], 0.12624)  # NCI index for acute_mi
  expect_equal(result$NCI_index[2], 0.64441)  # NCI index for chf
})

# Test for missing diagnosis codes handling
test_that("comorb function handles missing diagnosis codes", {
  test_data_missing <- test_data
  test_data_missing$dx1 <- NA

  result <- comorb(test_data_missing, id = "ID", start_date = "start_date", end_date = "end_date",
                   claim_start_date = "claim_start_date", claim_end_date = "claim_end_date",
                   claim_type = "claim_type", dx_var_list = c("dx1", "dx2"), ruleout = FALSE)

  expect_true(all(result$acute_mi == 0))
  expect_true(all(result$chf == 0))
})

