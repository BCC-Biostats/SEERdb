#' Create hospital dataset
#'
#' Function will identify all of the hospital datasets in your directory and create a dataset for them
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

get_hospital_data <- function(
    db_directory = ".",
    hospital_files = NULL
  ) {

  # If there is no given vector of hospital files, prompt the users
  if (is.null({{ hospital_files }})) {

    # Get list of files in directory
    files_in_directory <- list.files(path = db_directory)

    hospital_files <- files_in_directory[grepl("hospital", files_in_directory)] |>
      as.vector()

    # Show files found and prompt user
    cat(paste0("     ",
               length(hospital_files),
               " files found:"
    )
    )

    cat("\n\n")

    print(hospital_files)

    cat("\n\n")

    cat("     Unzip files to create dataset?")

    cat("\n\n")

    unzip_files_q <- readline(prompt = "yes/no: ")

    # If yes then start going through and getting the data

    if(tolower(unzip_files_q) %in% c("y", "yes", "ye")) {

      # Print the loading bar
      cat("\r[",
          paste(rep("=", 0), collapse = ""),
          paste(rep(" ", length(hospital_files) - 0), collapse = ""),
          "] ",
          0,
          "/",
          length(hospital_files),
          " Files Processed",
          sep = ""
      )

      flush.console()

      results_list <- list()

      for (i in 1:length(hospital_files)) {

        current_file <- paste0(db_directory, hospital_files[i])

        cat(current_file)

        R.utils::gunzip(current_file,
                        remove = FALSE)

        unzip_current_file <- sub("\\.gz$", "", current_file)


        read_data <- suppressWarnings(suppressMessages(readr::read_fwf(unzip_current_file,
                         col_positions = readr::fwf_positions(
                           start = SEERdb::hospital_file_labels$Start,
                           end = SEERdb::hospital_file_labels$Stop,
                           col_names = SEERdb::hospital_file_labels$name
                         )
        )))

        read_data <- labelled::set_variable_labels(read_data,
                                      .labels = setNames(
                                        as.list(SEERdb::hospital_file_labels$label),
                                        SEERdb::hospital_file_labels$name
                                      )
        )

        results_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", hospital_files[i])]] <- read_data

        file.remove(unzip_current_file)

        # Print the loading bar
        cat("\r[",
            paste(rep("=", i), collapse = ""),
            paste(rep(" ", length(hospital_files) - i), collapse = ""),
            "] ",
            i,
            "/",
            length(hospital_files),
            " Files Processed ",
            sep = ""
            )

        flush.console()

      }

    } else {

      cat("\nAll hospital files should include the word 'hospital'\n")

    }

  }
  else {

    # Get files in the directory
    files_in_directory <- list.files(path = db_directory)

    # check that the files list is all in the directory
    if (all(hospital_files %in% files_in_directory)) {

      results_list <- list()

      for (i in 1:length(hospital_files)) {

        current_file <- paste0(db_directory, hospital_files[i])

        R.utils::gunzip(current_file,
                        remove = FALSE)

        unzip_current_file <- sub("\\.gz$", "", current_file)


        read_data <- suppressWarnings(suppressMessages(read_fwf(unzip_current_file,
                                                                col_positions = fwf_positions(
                                                                  start = SEERdb::hospital_file_labels$Start,
                                                                  end = SEERdb::hospital_file_labels$Stop,
                                                                  col_names = SEERdb::hospital_file_labels$name
                                                                )
        )))

        read_data <- labelled::set_variable_labels(read_data,
                                      .labels = setNames(
                                        as.list(SEERdb::hospital_file_labels$label),
                                        SEERdb::hospital_file_labels$name
                                      )
        )

        results_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", hospital_files[i])]] <- read_data

        file.remove(unzip_current_file)

      }

    } else {

      stop(paste0(
        "\n'",
        hospital_files[!(hospital_files %in% files_in_directory)],
        "' was not found in given directory"))

    }

  }

  return(results_list)

}
