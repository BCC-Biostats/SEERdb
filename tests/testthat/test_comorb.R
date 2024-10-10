library(testthat)

# Sample data for testing
claim_data <- data.frame(
  patient_id = c(1, 1, 2, 2),
  claim_start = c("2015-01-01", "2016-01-01", "2016-05-01", "2017-01-01"),
  claim_end = c("2015-01-15", "2016-01-15", "2016-05-15", "2017-01-15"),
  dx1 = c("410", "428", "I21", "I252"),
  dx2 = c(NA, "40291", "4439", NA)
)

test_that("comorb function returns the expected columns", {
  result <- comorb(claim_data, id = "patient_id", start_date = "2015-01-01",
                   end_date = "2017-01-01", claim_start_date = "claim_start",
                   claim_end_date = "claim_end", claim_type = NULL,
                   dx_var_list = c("dx1", "dx2"))

  expect_true("Charlson" %in% colnames(result))
  expect_true("NCI_index" %in% colnames(result))
})

test_that("comorb filters data based on the date range", {
  result <- comorb(claim_data, id = "patient_id", start_date = "2015-01-01",
                   end_date = "2016-12-31", claim_start_date = "claim_start",
                   claim_end_date = "claim_end", claim_type = NULL,
                   dx_var_list = c("dx1", "dx2"))

  expect_equal(nrow(result), 2)
})

test_that("comorb calculates Charlson and NCI indices correctly", {
  result <- comorb(claim_data, id = "patient_id", start_date = "2015-01-01",
                   end_date = "2017-01-01", claim_start_date = "claim_start",
                   claim_end_date = "claim_end", claim_type = NULL,
                   dx_var_list = c("dx1", "dx2"))

  expect_equal(result$Charlson[1], 2) # 410 for MI, 428 for CHF
  expect_equal(result$NCI_index[1], 0.12624 + 0.64441) # MI + CHF
})
