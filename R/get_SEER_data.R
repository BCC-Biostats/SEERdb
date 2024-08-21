#' Create SEER dataset
#'
#' Function will identify all of the SEER datasets in your directory and create a dataset for them
#'
#' @param db_directory path to directory where data files are held
#' @param SEER_files vector of names of SEER files
#'
#' @return a list of dataframes from each given SEER file
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
    SEER_files = NULL
){

  # If there is no given vector of SEER files, prompt the users
  if (is.null({{ SEER_files }})) {

    # Get list of files in directory
    files_in_directory <- list.files(path = db_directory)

    SEER_files <- files_in_directory[grepl("SEER", files_in_directory)] |>
      as.vector()

    # Show files found and prompt user
    cat(crayon::blue$bold(paste0("ℹ ",
                                 length(SEER_files),
                                 " files found:"
    )))

    cat("\n\n")

    print(SEER_files)

    cat("\n\n")

    cat(crayon::blue$bold("Unzip files to create dataset?"))

    cat("\n\n")

    unzip_files_q <- readline(prompt = "yes/no: ")

    # If yes then start going through and getting the data

    if(tolower(unzip_files_q) %in% c("y", "yes", "ye")) {

      # Print the loading bar
      cat("\r[",
          paste(rep("=", 0), collapse = ""),
          paste(rep(" ", length(SEER_files) - 0), collapse = ""),
          "] ",
          0,
          "/",
          length(SEER_files),
          " Files Processed",
          sep = ""
      )

      flush.console()

      results_list <- list()

      for (i in 1:length(SEER_files)) {

        current_file <- paste0(db_directory, SEER_files[i])

        cat(current_file)

        R.utils::gunzip(current_file,
                        remove = FALSE)

        unzip_current_file <- sub("\\.gz$", "", current_file)


        read_data <- suppressWarnings(suppressMessages(readr::read_fwf(unzip_current_file,
                                                                       col_positions = readr::fwf_positions(
                                                                         start = SEERdb::SEER.input_labels$Start,
                                                                         end = SEERdb::SEER.input_labels$Stop,
                                                                         col_names = SEERdb::SEER.input_labels$name
                                                                       )
        )))

        read_data <- labelled::set_variable_labels(read_data,
                                                   .labels = setNames(
                                                     as.list(SEERdb::SEER.input_labels$label),
                                                     SEERdb::SEER.input_labels$name
                                                   )
        )

        results_list[[sub("SEER\\.([^.]+)\\.cancer\\.txt", "\\1", SEER_files[i])]] <- read_data

        file.remove(unzip_current_file)

        # Print the loading bar
        cat("\r[",
            paste(rep("=", i), collapse = ""),
            paste(rep(" ", length(SEER_files) - i), collapse = ""),
            "] ",
            i,
            "/",
            length(SEER_files),
            " Files Processed ",
            sep = ""
        )

        flush.console()

      }
    } else {

      stop("All SEER files should include the word 'SEER'")

          }
  } else {

    # Get files in the directory
    files_in_directory <- list.files(path = db_directory)

    # check that the files list is all in the directory
    if (all(SEER_files %in% files_in_directory)) {

      results_list <- list()

      for (i in 1:length(SEER_files)) {

        current_file <- paste0(db_directory, SEER_files[i])

        R.utils::gunzip(current_file,
                        remove = FALSE)

        unzip_current_file <- sub("\\.gz$", "", current_file)

        read_data <- suppressWarnings(suppressMessages(read_fwf(unzip_current_file,
                                                                col_positions = fwf_positions(
                                                                  start = SEERdb::SEER.input_labels$Start,
                                                                  end = SEERdb::SEER.input_labels$Stop,
                                                                  col_names = SEERdb::SEER.input_labels$name
                                                                )
        )))

        read_data <- labelled::set_variable_labels(read_data,
                                                   .labels = setNames(
                                                     as.list(SEERdb::SEER.input_labels$label),
                                                     SEERdb::SEER.input_labels$name
                                                   )
        )

        results_list[[sub("SEER\\.([^.]+)\\.cancer\\.txt\\..*", "\\1", SEER_files[i])]] <- read_data

        file.remove(unzip_current_file)

      }

    } else {

      stop(paste0(
        "\n'",
        SEER_files[!(SEER_files %in% files_in_directory)],
        "' was not found in given directory"))

    }


  }

  return(results_list)

}
