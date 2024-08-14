#' Create hsp dataset
#'
#' Function will identify all of the hsp datasets in your directory and create a dataset for them.
#' There are 6 segments provided: base, revenue, condition, occurence, span, and value
#'
#' PATIENT_ID and CLM_ID should be used for merging files
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

get_hsp_data <- function(
    db_directory = ".",
    hsp_base_files = NULL,
    hsp_condition_files = NULL,
    hsp_occurence_files = NULL,
    hsp_revenue_files = NULL,
    hsp_span_files = NULL,
    hsp_value_files = NULL
){

  # if hospital_files is null, prompt the user if files in directory are correct
  if (all(is.null(c({{ hsp_base_files }},
                    {{ hsp_condition_files }},
                    {{ hsp_occurence_files }},
                    {{ hsp_revenue_files }},
                    {{ hsp_span_files }},
                    {{ hsp_condition_files }}
                    )
                  )
          )
      ) {
    files_in_directory <- list.files(path = db_directory)

    hsp_files <- files_in_directory[grepl("hsp", files_in_directory)] |>
      as.vector()

    hsp_base_files <- grep("hsp\\d{4}\\.base", hsp_files, value = TRUE)
    hsp_condition_files <- grep("hsp\\d{4}\\.condition", hsp_files, value = TRUE)
    hsp_occurence_files <- grep("hsp\\d{4}\\.occurrence", hsp_files, value = TRUE)
    hsp_revenue_files <- grep("hsp\\d{4}\\.revenue", hsp_files, value = TRUE)
    hsp_span_files <- grep("hsp\\d{4}\\.span", hsp_files, value = TRUE)
    hsp_value_files <- grep("hsp\\d{4}\\.value", hsp_files, value = TRUE)


    cat(crayon::blue$bold(paste0("",
               length(hsp_files),
               " files found:\n"
    )))

    cat(crayon::blue(".base files:\n"))

    print(hsp_base_files)

    cat(crayon::blue(".condition files:\n"))

    print(hsp_condition_files)

    cat(crayon::blue(".occurence files:\n"))

    print(hsp_occurence_files)

    cat(crayon::blue(".revenue files:\n"))

    print(hsp_revenue_files)

    cat(crayon::blue(".span files:\n"))

    print(hsp_span_files)

    cat(crayon::blue(".value files:\n"))

    print(hsp_value_files)

    cat("\n")

    cat(crayon::blue$bold("Unzip files to create dataset?"))

    cat("\n")

    unzip_files_q <- readline(prompt = "yes/no: ")

    # if yes then process the files
    if(tolower(unzip_files_q) %in% c("y", "yes", "ye")) {

      # Print the loading bar
      cat("\r[",
          paste(rep("=", 0), collapse = ""),
          paste(rep(" ", length(hsp_files) - 0), collapse = ""),
          "] ",
          0,
          "/",
          length(hospital_files),
          " Files Processed",
          sep = ""
      )

      flush.console()

      results_list <- list()


      # Go through each filetype and return a list of dataframe for each
      # for each filetype

    }


  }


}
