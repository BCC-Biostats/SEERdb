#' Get Comorbidity Information
#'
#' This function processes claim data to identify comorbidities based on ICD-9 and ICD-10 diagnosis codes,
#' and computes Charlson and NCI comorbidity indices.
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

  conditions <- c("acute_mi", "history_mi", "chf", "pvd", "cvd", "copd",
                  "dementia", "paralysis", "diabetes", "diabetes_comp",
                  "renal_disease", "mild_liver_disease", "liver_disease",
                  "ulcers", "rheum_disease", "aids")

  dx_code_list <- paste0("dx_code_", 1:length(dx_var_list))

  dat <- infile |>
    dplyr::select(dplyr::all_of(c(id,
                                  claim_start_date,
                                  claim_end_date,
                                  dx_var_list))) |>
    dplyr::rename_with(~ paste0("dx_code_", seq_along(.)), all_of(dx_var_list)) |>
    dplyr::rename(patient_id = !!id,
                  claim_start = !!claim_start_date,
                  claim_end = !!claim_end_date) |>
    dplyr::mutate(dx_codes = purrr::pmap(list(!!!rlang::syms(dx_code_list)), c)) |>
    dplyr::select(-dplyr::starts_with("dx_code_")) |>
    dplyr::mutate(claim_start = lubridate::ymd(claim_start),
                  claim_end = lubridate::ymd(claim_end),
                  icd_vrsn = ifelse(claim_end < as.Date("2015-10-1"), 9, 10)) |>
    dplyr::mutate(acute_mi = purrr::map_lgl(dx_codes, ~ any(.x %in% c("410", "I21", "I22"))),
                  history_mi = purrr::map_lgl(dx_codes, ~ any(.x %in% c("412", "I252"))),
                  chf = purrr::map_lgl(dx_codes, ~ any(.x %in% c('39891','40201','40211','40291','40401','40403','40411',
                                                                 '40413','40491','40493','428', '4254', '4255', '4256',
                                                                 '4257', '4258', '4259'))),
                  pvd = purrr::map_lgl(dx_codes, ~ any(.x %in% c('0930','440','441','4471','5571','5579','V434', '4431',
                                                                 '4432', '4433', '4434' ,'4435', '4436', '4437', '4438', "4439"))),
                  cvd = purrr::map_lgl(dx_codes, ~ any(.x %in% c('36234', '430', '431', '432', '433', '434', '435', '436',
                                                                 '437', '438', 'G45','G46','H340','I6'))),
                  copd = purrr::map_lgl(dx_codes, ~ any(.x %in% c('4168','4169','5064','5081','5088', '490', '490', '491',
                                                                  '492', '493', '494', '495', '496', '497', '498', '499',
                                                                  '500', '501', '502', '503', '504', '505', 'I278','I279',
                                                                  'J684','J701','J703', 'J40', 'J41', 'J42', 'J43', 'J44',
                                                                  'J45', 'J46', 'J47', 'J60', 'J61', 'J62', 'J63', 'J64',
                                                                  'J65', 'J66', 'J67'))),
                  dementia = purrr::map_lgl(dx_codes, ~ any(.x %in% c('290','2941','3312', 'F051','G30','G311',
                                                                      'F00', 'F01','F02', 'F03'))),
                  paralysis = purrr::map_lgl(dx_codes, ~ any(.x %in% c('3341','342','343','3449', '3440', '3441', '3442',
                                                                       '3443', '3444', '3445', '3446'))),
                  diabetes = purrr::map_lgl(dx_codes, ~ any(.x %in% c('E100','E101','E106','E108','E109','E110','E111','E116',
                                                                      'E118','E119','E130','E131','E136','E138','E139',
                                                                      '2500','2501','2502','2503','2508','2509'))),
                  diabetes_comp = purrr::map_lgl(dx_codes, ~ any(.x %in% c('2504','2505','2506','2507', 'E102','E103','E104',
                                                                           'E105','E107','E112','E113','E114','E115','E117',
                                                                           'E132','E133','E134','E135','E137'))),
                  renal_disease = purrr::map_lgl(dx_codes, ~ any(.x %in% c('40301','40311','40391','40402','40403','40412',
                                                                           '40413','40492','40493','582','585','586','5880',
                                                                           'V420','V451','V56', '5830', '5831','5832' ,'5833',
                                                                           '5834','5835', '5836', '5837'))),
                  mild_liver_disease = purrr::map_lgl(dx_codes, ~ any(.x %in% c('07022','07023','07032','07033','07044','07054','0706',
                                                                                '0709','570','571','5733','5734','5738','5739','V427',
                                                                                'B18','K709','K717','K73','K74','K760','K768','K769','Z944',
                                                                                'K700', 'K701', 'K702','K703', 'K713', 'K714','K715',
                                                                                'K762', 'K763', 'K764'))),
                  liver_disease = purrr::map_lgl(dx_codes, ~ any(.x %in% c('I850','I859','I864','I982','K704','K711','K721','K729','K765','K766','K767',
                                                                           '4560', '4561', '4562', '5722', '5723','5724' ,'5725' ,'5726' ,'5727' ,'5728'))),
                  ulcers = purrr::map_lgl(dx_codes, ~ any(.x %in% c('531', '532','533' ,'534', 'K25', 'K26','K27','K28' ))),
                  rheum_disease = purrr::map_lgl(dx_codes, ~ any(.x %in% c('M05','M06','M315','M32','M33','M34','M351','M353','M360',
                                                                           '4465','7148','725', '7100', '7101', '7102','7103' ,'7104',
                                                                           '7140', '7141', '7142'))),
                  aids = purrr::map_lgl(dx_codes, ~ any(.x %in% c('B20','B21','B22','B24', '042', '043', '044')))
    ) |>
    dplyr::mutate(Charlson = 1*(acute_mi | history_mi) +
                    1*(chf) +
                    1*(pvd) +
                    1*(cvd) +
                    1*(copd) +
                    1*(dementia) +
                    2*(paralysis) +
                    1*(diabetes & !diabetes_comp) +
                    2*(diabetes_comp) +
                    2*(renal_disease) +
                    1*(mild_liver_disease &  !liver_disease) +
                    3*(liver_disease) +
                    1*(ulcers) +
                    1*(rheum_disease) +
                    6*(aids)) |>
    dplyr::mutate(NCI_index = 0.12624*(acute_mi) +
                    0.07999*(history_mi) +
                    0.64441*(chf) +
                    0.26232*(pvd) +
                    0.27868*(cvd) +
                    0.52487*(copd) +
                    0.72219*(dementia) +
                    0.39882*(paralysis) +
                    0.29408*(diabetes | diabetes_comp) +
                    0.47010*(renal_disease) +
                    0.73803*(mild_liver_disease | liver_disease) +
                    0.07506*(ulcers) +
                    0.21905*(rheum_disease) +
                    0.58362*(aids))


  dat <- dat |>
    dplyr::filter(claim_start >= {{ start_date }},
                  claim_end <= {{ end_date }})


  return(dat)
}
