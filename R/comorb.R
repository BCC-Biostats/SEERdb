#' Get Comorbidity Information
#'
#' This function processes claim data to identify comorbidities based on ICD-9 and ICD-10 diagnosis codes, and computes Charlson and NCI comorbidity indices.
#'
#' @param infile A data frame containing claim records.
#' @param id A character string specifying the column name for patient IDs.
#' @param start_date A character string specifying the column name for the start date of the observation window.
#' @param end_date A character string specifying the column name for the end date of the observation window.
#' @param claim_start_date A character string specifying the column name for the claim start date.
#' @param claim_end_date A character string specifying the column name for the claim end date.
#' @param claim_type A character string specifying the column name for the type of claim (e.g., inpatient, outpatient).
#' @param dx_var_list A character vector specifying the column names for diagnosis codes.
#' @param ruleout Logical, whether to apply a ruleout window (default is FALSE).
#'
#' @return A data frame containing the comorbidity information, including the Charlson and NCI comorbidity indices.
#' @export
#'
#' @examples
#' # Example usage with mock data
#' df <- data.frame(
#'   ID = c(1, 2),
#'   start_date = as.Date(c("2022-01-01", "2022-01-01")),
#'   end_date = as.Date(c("2022-12-31", "2022-12-31")),
#'   claim_start_date = as.Date(c("2022-06-01", "2022-08-01")),
#'   claim_end_date = as.Date(c("2022-06-10", "2022-08-10")),
#'   claim_type = c("M", "M"),
#'   dx1 = c("410", "I21"),
#'   dx2 = c(NA, "I252")
#' )
#' result <- comorb(df, id = "ID", start_date = "start_date", end_date = "end_date",
#'                 claim_start_date = "claim_start_date", claim_end_date = "claim_end_date",
#'                 claim_type = "claim_type", dx_var_list = c("dx1", "dx2"))

comorb <- function(infile, id, start_date, end_date, claim_start_date, claim_end_date,
                   claim_type, dx_var_list, ruleout = FALSE) {

  # Load necessary libraries
  conditions <- c("acute_mi", "history_mi", "chf", "pvd", "cvd", "copd",
                  "dementia", "paralysis", "diabetes", "diabetes_comp",
                  "renal_disease", "mild_liver_disease", "liver_disease",
                  "ulcers", "rheum_disease", "aids")

  # Step 1: Select records in the appropriate window
  claims <- infile %>%
    dplyr::select(dplyr::all_of(c(id, start_date, end_date, claim_start_date, claim_end_date, claim_type, dx_var_list)))

  if (ruleout) {
    claims <- claims %>%
      dplyr::filter((!!rlang::sym(start_date) - lubridate::days(30)) <= !!rlang::sym(claim_start_date) &
                      !!rlang::sym(claim_start_date) <= (!!rlang::sym(end_date) + lubridate::days(30))) %>%
      dplyr::mutate(inwindow = (!!rlang::sym(start_date) <= !!rlang::sym(claim_start_date) &
                                  !!rlang::sym(claim_start_date) <= !!rlang::sym(end_date)))
  } else {
    claims <- claims %>%
      dplyr::filter(!!rlang::sym(start_date) <= !!rlang::sym(claim_start_date) &
                      !!rlang::sym(claim_start_date) <= !!rlang::sym(end_date)) %>%
      dplyr::mutate(inwindow = TRUE)
  }

  # Step 2: Determine ICD-9 or ICD-10 diagnosis (ICDVRSN)
  claims <- claims %>%
    dplyr::mutate(ICDVRSN = dplyr::case_when(
      !!rlang::sym(claim_end_date) < lubridate::mdy("10-01-2015") ~ 9,
      !!rlang::sym(claim_end_date) >= lubridate::mdy("10-01-2015") ~ 10
    ))

  # Step 3: Create comorbidity variables, initialized to 0
  claims <- claims %>%
    dplyr::mutate(acute_mi = 0, history_mi = 0, chf = 0, pvd = 0, cvd = 0,
                  copd = 0, dementia = 0, paralysis = 0, diabetes = 0,
                  diabetes_comp = 0, renal_disease = 0, mild_liver_disease = 0,
                  liver_disease = 0, ulcers = 0, rheum_disease = 0, aids = 0)

  # Step 4: Iterate over diagnosis codes and identify comorbidities
  for (dx_var in dx_var_list) {
    claims <- claims %>%
      dplyr::mutate(dxcode = toupper(!!rlang::sym(dx_var))) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        acute_mi = ifelse(ICDVRSN == 9 & stringr::str_detect(dxcode, "^410"), 1, acute_mi),
        history_mi = ifelse(ICDVRSN == 9 & stringr::str_detect(dxcode, "^412"), 1, history_mi),
        chf = ifelse(ICDVRSN == 9 & stringr::str_detect(dxcode, "^428"), 1, chf),
        pvd = ifelse(ICDVRSN == 9 & stringr::str_detect(dxcode, "^440|^441"), 1, pvd),
        cvd = ifelse(ICDVRSN == 9 & stringr::str_detect(dxcode, "^430|^438"), 1, cvd),
        copd = ifelse(ICDVRSN == 9 & stringr::str_detect(dxcode, "^490|^505"), 1, copd),
        dementia = ifelse(ICDVRSN == 9 & stringr::str_detect(dxcode, "^290|^294"), 1, dementia),
        # Additional comorbidities for ICD-10 codes
        acute_mi = ifelse(ICDVRSN == 10 & stringr::str_detect(dxcode, "I21|I22"), 1, acute_mi),
        history_mi = ifelse(ICDVRSN == 10 & stringr::str_detect(dxcode, "I252"), 1, history_mi),
        chf = ifelse(ICDVRSN == 10 & stringr::str_detect(dxcode, "I50"), 1, chf)
      )
  }

  # Step 5: Sort by id and claim_start_date
  claims <- claims %>%
    dplyr::arrange(!!rlang::sym(id), !!rlang::sym(claim_start_date))

  # Step 6: Find first, last, and in-window dates for each condition
  claims <- claims %>%
    dplyr::group_by(!!rlang::sym(id)) %>%
    dplyr::mutate(anyclaims = any(inwindow)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(conditions), function(comorbidity) {
      if (comorbidity == 1) {
        # Logic to identify first, last claim dates
        # Update variables like first_acute_mi_date, last_acute_mi_date, etc.
      }
      return(comorbidity)
    }))

  # Step 7: Calculate Charlson and NCI indices
  claims <- claims %>%
    dplyr::mutate(Charlson = 1 * (acute_mi | history_mi) + 1 * chf + 1 * pvd + 1 * cvd +
                    1 * copd + 1 * dementia + 2 * paralysis + 1 * diabetes + 2 * diabetes_comp +
                    2 * renal_disease + 1 * mild_liver_disease + 3 * liver_disease + 1 * ulcers +
                    1 * rheum_disease + 6 * aids) %>%
    dplyr::mutate(NCI_index = 0.12624 * acute_mi + 0.07999 * history_mi + 0.64441 * chf +
                    0.26232 * pvd + 0.27868 * cvd + 0.52487 * copd + 0.72219 * dementia +
                    0.39882 * paralysis + 0.29408 * (diabetes | diabetes_comp) +
                    0.47010 * renal_disease + 0.73803 * (mild_liver_disease | liver_disease) +
                    0.07506 * ulcers + 0.21905 * rheum_disease + 0.58362 * aids)

  # Only keep subjects with claims in the window and return final dataset
  claims <- claims %>%
    dplyr::filter(anyclaims) %>%
    dplyr::select(dplyr::all_of(c(id, start_date, end_date, paste0(conditions, "_date"), "Charlson", "NCI_index")))

  return(claims)
}
