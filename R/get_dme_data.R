#' Create durable medical equipment dataset
#'
#' Function will identify all of the hospice claims datasets in your directory and create a dataset for them.
#' There are 6 segments provided: base, revenue, condition, occurence, span, and value
#'
#' PATIENT_ID and CLM_ID should be used for merging files
#'
#' @param db_directory path to directory where data files are held
#' @param dme_base_files vector of names of hsp.base files
#' @param dme_demo_files vector of names of hsp.condition files
#' @param dme_line_files vector of names of hsp.occurence files
#'
#' @return a list of dataframes from each given hsp file
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

get_dme_data <- function(
    db_directory = ".",
    dme_base_files = NULL,
    dme_demo_files = NULL,
    dme_line_files = NULL
){

  # if hospital_files is null, prompt the user if files in directory are correct
  if (all(is.null(c({{ dme_base_files }},
                    {{ dme_demo_files }},
                    {{ dme_line_files }}
  )))) {

    files_in_directory <- list.files(path = db_directory)

    dme_files <- files_in_directory[grepl("dme", files_in_directory)] |>
      as.vector()

    dme_base_files <- grep("dme\\d{4}\\.base", dme_files, value = TRUE)
    dme_demo_files <- grep("dme\\d{4}\\.demo", dme_files, value = TRUE)
    dme_line_files <- grep("dme\\d{4}\\.line", dme_files, value = TRUE)

    cat(crayon::blue$bold(paste0("ℹ ",
                                 length(dme_files),
                                 " files found:\n"
    )))

    cat(crayon::blue(".base files:\n"))

    print(dme_base_files)

    cat(crayon::blue(".demo files:\n"))

    print(dme_demo_files)

    cat(crayon::blue(".line files:\n"))

    print(dme_line_files)

    cat("\n")

    cat(crayon::blue$bold("Unzip files to create dataset?"))

    cat("\n")

    unzip_files_q <- readline(prompt = "yes/no: ")

    # if yes then process the files
    if(tolower(unzip_files_q) %in% c("y", "yes", "ye")) {

    } else {

      stop("Specify values using function parameters")

    }


  } else {


  }


}
