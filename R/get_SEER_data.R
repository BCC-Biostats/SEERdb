#' Create SEER dataset
#'
#' Function will identify all of the SEER datasets in your directory and create a dataset for them
#'
#' @param db_directory path to directory where data files are held
#' @param hospital_files vector of names of hospital files
#'
#' @return a list of dataframes from each given hospital file
#' @export
#'
#' @examples
#' hospitalDB <- SEERdb::get_hospital_data("../../master data files - SEER Medicare 2024/")
#'
#'
#' hospitalDB <- SEERdb::get_hospital_data(
#'                 "../../master data files - SEER Medicare 2024/",
#'                 c("hospital1996.withzip.txt.gz", "hospital2000.withzip.txt.gz")
#'                 )
#'

get_SEER_data <- function(
    db_directory = ".",
    hospital_files = NULL
){

}
