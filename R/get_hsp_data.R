#' Create hsp dataset
#'
#' Function will identify all of the hospice claims datasets in your directory and create a dataset for them.
#' There are 6 segments provided: base, revenue, condition, occurence, span, and value
#'
#' PATIENT_ID and CLM_ID should be used for merging files
#'
#' @param db_directory path to directory where data files are held
#' @param hsp_files vector of names of hsp files
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

get_hsp_data <- function(
    db_directory = ".",
    hsp_base_files = NULL,
    hsp_condition_files = NULL,
    hsp_occurence_files = NULL,
    hsp_revenue_files = NULL,
    hsp_span_files = NULL,
    hsp_value_files = NULL,
    hsp_demo_files = NULL
){

  # if hospital_files is null, prompt the user if files in directory are correct
  if (all(is.null(c({{ hsp_base_files }},
                    {{ hsp_condition_files }},
                    {{ hsp_occurence_files }},
                    {{ hsp_revenue_files }},
                    {{ hsp_span_files }},
                    {{ hsp_condition_files }},
                    {{ hsp_condition_files }},
                    {{ hsp_demo_files }}
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
    hsp_demo_files <- grep("hsp\\d{4}\\.demo", hsp_files, value = TRUE)


    cat(crayon::blue$bold(paste0("ℹ ",
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

    cat(crayon::blue(".demo files:\n"))

    print(hsp_demo_files)

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
          length(hsp_files),
          " Files Processed",
          sep = ""
      )

      flush.console()

      results_list <- list()


      # Go through each filetype and return a list of dataframe for each
      # for each filetype
      file_i <- 0

      # base file
      base_list <- list()

      for (i in 1:length(hsp_base_files)) {

        # Get the current hsp base file
        current_file <- paste0(db_directory, hsp_base_files[i])

        # output the current file being worked on
        cat(current_file)

        # unzip the current file
        R.utils::gunzip(current_file,
                        remove = FALSE)

        # Get the unzipped file
        unzip_current_file <- sub("\\.gz$", "", current_file)

        # Read the data
        read_data <- suppressWarnings(suppressMessages(readr::read_fwf(unzip_current_file,
                                                                       col_positions = readr::fwf_positions(
                                                                         start = SEERdb::hsp.base_labels$Start,
                                                                         end = SEERdb::hsp.base_labels$Stop,
                                                                         col_names = SEERdb::hsp.base_labels$name
                                                                       )
        )))

        # Add the labels
        read_data <- labelled::set_variable_labels(read_data,
                                                   .labels = setNames(
                                                     as.list(SEERdb::hsp.base_labels$label),
                                                     SEERdb::hsp.base_labels$name
                                                   )
        )

        # asign the files to the base list
        base_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", hsp_base_files[i])]] <- read_data

        # remove the unzipped file
        file.remove(unzip_current_file)

        # increase the file counter for loading bar
        file_i <- file_i + 1

        # Print the loading bar
        cat("\r[",
            paste(rep("=", file_i), collapse = ""),
            paste(rep(" ", length(hsp_files) - file_i), collapse = ""),
            "] ",
            file_i,
            "/",
            length(hsp_files),
            " Files Processed ",
            sep = ""
        )

        flush.console()

      }

      # condition file
      condition_list <- list()

      for (i in 1:length(hsp_condition_files)) {

        # Get the current hsp base file
        current_file <- paste0(db_directory, hsp_condition_files[i])

        # output the current file being worked on
        cat(current_file)

        # unzip the current file
        R.utils::gunzip(current_file,
                        remove = FALSE)

        # Get the unzipped file
        unzip_current_file <- sub("\\.gz$", "", current_file)

        # Read the data
        read_data <- suppressWarnings(suppressMessages(readr::read_fwf(unzip_current_file,
                                                                       col_positions = readr::fwf_positions(
                                                                         start = SEERdb::hsp.condition_labels$Start,
                                                                         end = SEERdb::hsp.condition_labels$Stop,
                                                                         col_names = SEERdb::hsp.condition_labels$name
                                                                       )
        )))

        # Add the labels
        read_data <- labelled::set_variable_labels(read_data,
                                                   .labels = setNames(
                                                     as.list(SEERdb::hsp.condition_labels$label),
                                                     SEERdb::hsp.condition_labels$name
                                                   )
        )

        # asign the files to the base list
        condition_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", hsp_condition_files[i])]] <- read_data

        # remove the unzipped file
        file.remove(unzip_current_file)

        # increase the file counter for loading bar
        file_i <- file_i + 1

        # Print the loading bar
        cat("\r[",
            paste(rep("=", file_i), collapse = ""),
            paste(rep(" ", length(hsp_files) - file_i), collapse = ""),
            "] ",
            file_i,
            "/",
            length(hsp_files),
            " Files Processed ",
            sep = ""
        )

        flush.console()

      }

      # occurence file
      ocurrence_list <- list()

      for (i in 1:length(hsp_occurence_files)) {

        # Get the current hsp base file
        current_file <- paste0(db_directory, hsp_occurence_files[i])

        # output the current file being worked on
        cat(current_file)

        # unzip the current file
        R.utils::gunzip(current_file,
                        remove = FALSE)

        # Get the unzipped file
        unzip_current_file <- sub("\\.gz$", "", current_file)

        # Read the data
        read_data <- suppressWarnings(suppressMessages(readr::read_fwf(unzip_current_file,
                                                                       col_positions = readr::fwf_positions(
                                                                         start = SEERdb::hsp.occurrence_labels$Start,
                                                                         end = SEERdb::hsp.occurrence_labels$Stop,
                                                                         col_names = SEERdb::hsp.occurrence_labels$name
                                                                       )
        )))

        # Add the labels
        read_data <- labelled::set_variable_labels(read_data,
                                                   .labels = setNames(
                                                     as.list(SEERdb::hsp.occurrence_labels$label),
                                                     SEERdb::hsp.occurrence_labels$name
                                                   )
        )

        # asign the files to the base list
        ocurrence_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", hsp_occurence_files[i])]] <- read_data

        # remove the unzipped file
        file.remove(unzip_current_file)

        # increase the file counter for loading bar
        file_i <- file_i + 1

        # Print the loading bar
        cat("\r[",
            paste(rep("=", file_i), collapse = ""),
            paste(rep(" ", length(hsp_files) - file_i), collapse = ""),
            "] ",
            file_i,
            "/",
            length(hsp_files),
            " Files Processed ",
            sep = ""
        )

        flush.console()

      }

      # revenue file
      revenue_list <- list()

      for (i in 1:length(hsp_revenue_files)) {

        # Get the current hsp base file
        current_file <- paste0(db_directory, hsp_revenue_files[i])

        # output the current file being worked on
        cat(current_file)

        # unzip the current file
        R.utils::gunzip(current_file,
                        remove = FALSE)

        # Get the unzipped file
        unzip_current_file <- sub("\\.gz$", "", current_file)

        # Read the data
        read_data <- suppressWarnings(suppressMessages(readr::read_fwf(unzip_current_file,
                                                                       col_positions = readr::fwf_positions(
                                                                         start = SEERdb::hsp.revenue_labels$Start,
                                                                         end = SEERdb::hsp.revenue_labels$Stop,
                                                                         col_names = SEERdb::hsp.revenue_labels$name
                                                                       )
        )))

        # Add the labels
        read_data <- labelled::set_variable_labels(read_data,
                                                   .labels = setNames(
                                                     as.list(SEERdb::hsp.revenue_labels$label),
                                                     SEERdb::hsp.revenue_labels$name
                                                   )
        )

        # asign the files to the base list
        revenue_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", hsp_revenue_files[i])]] <- read_data

        # remove the unzipped file
        file.remove(unzip_current_file)

        # increase the file counter for loading bar
        file_i <- file_i + 1

        # Print the loading bar
        cat("\r[",
            paste(rep("=", file_i), collapse = ""),
            paste(rep(" ", length(hsp_files) - file_i), collapse = ""),
            "] ",
            file_i,
            "/",
            length(hsp_files),
            " Files Processed ",
            sep = ""
        )

        flush.console()

      }

      # span file
      span_list <- list()

      for (i in 1:length(hsp_span_files)) {

        # Get the current hsp base file
        current_file <- paste0(db_directory, hsp_span_files[i])

        # output the current file being worked on
        cat(current_file)

        # unzip the current file
        R.utils::gunzip(current_file,
                        remove = FALSE)

        # Get the unzipped file
        unzip_current_file <- sub("\\.gz$", "", current_file)

        # Read the data
        read_data <- suppressWarnings(suppressMessages(readr::read_fwf(unzip_current_file,
                                                                       col_positions = readr::fwf_positions(
                                                                         start = SEERdb::hsp.span_labels$Start,
                                                                         end = SEERdb::hsp.span_labels$Stop,
                                                                         col_names = SEERdb::hsp.span_labels$name
                                                                       )
        )))

        # Add the labels
        read_data <- labelled::set_variable_labels(read_data,
                                                   .labels = setNames(
                                                     as.list(SEERdb::hsp.span_labels$label),
                                                     SEERdb::hsp.span_labels$name
                                                   )
        )

        # asign the files to the base list
        span_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", hsp_span_files[i])]] <- read_data

        # remove the unzipped file
        file.remove(unzip_current_file)

        # increase the file counter for loading bar
        file_i <- file_i + 1

        # Print the loading bar
        cat("\r[",
            paste(rep("=", file_i), collapse = ""),
            paste(rep(" ", length(hsp_files) - file_i), collapse = ""),
            "] ",
            file_i,
            "/",
            length(hsp_files),
            " Files Processed ",
            sep = ""
        )

        flush.console()

      }

      # value file
      value_list <- list()

      for (i in 1:length(hsp_value_files)) {

        # Get the current hsp base file
        current_file <- paste0(db_directory, hsp_value_files[i])

        # output the current file being worked on
        cat(current_file)

        # unzip the current file
        R.utils::gunzip(current_file,
                        remove = FALSE)

        # Get the unzipped file
        unzip_current_file <- sub("\\.gz$", "", current_file)

        # Read the data
        read_data <- suppressWarnings(suppressMessages(readr::read_fwf(unzip_current_file,
                                                                       col_positions = readr::fwf_positions(
                                                                         start = SEERdb::hsp.value_labels$Start,
                                                                         end = SEERdb::hsp.value_labels$Stop,
                                                                         col_names = SEERdb::hsp.value_labels$name
                                                                       )
        )))

        # Add the labels
        read_data <- labelled::set_variable_labels(read_data,
                                                   .labels = setNames(
                                                     as.list(SEERdb::hsp.value_labels$label),
                                                     SEERdb::hsp.value_labels$name
                                                   )
        )

        # asign the files to the base list
        value_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", hsp_value_files[i])]] <- read_data

        # remove the unzipped file
        file.remove(unzip_current_file)

        # increase the file counter for loading bar
        file_i <- file_i + 1

        # Print the loading bar
        cat("\r[",
            paste(rep("=", file_i), collapse = ""),
            paste(rep(" ", length(hsp_files) - file_i), collapse = ""),
            "] ",
            file_i,
            "/",
            length(hsp_files),
            " Files Processed ",
            sep = ""
        )

        flush.console()

      }

      # demo file
      demo_list <- list()

      for (i in 1:length(hsp_demo_files)) {

        # Get the current hsp base file
        current_file <- paste0(db_directory, hsp_demo_files[i])

        # output the current file being worked on
        cat(current_file)

        # unzip the current file
        R.utils::gunzip(current_file,
                        remove = FALSE)

        # Get the unzipped file
        unzip_current_file <- sub("\\.gz$", "", current_file)

        # Read the data
        read_data <- suppressWarnings(suppressMessages(readr::read_fwf(unzip_current_file,
                                                                       col_positions = readr::fwf_positions(
                                                                         start = SEERdb::hsp.demo_labels$Start,
                                                                         end = SEERdb::hsp.demo_labels$Stop,
                                                                         col_names = SEERdb::hsp.demo_labels$name
                                                                       )
        )))

        # Add the labels
        read_data <- labelled::set_variable_labels(read_data,
                                                   .labels = setNames(
                                                     as.list(SEERdb::hsp.demo_labels$label),
                                                     SEERdb::hsp.demo_labels$name
                                                   )
        )

        # asign the files to the base list
        demo_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", hsp_demo_files[i])]] <- read_data

        # remove the unzipped file
        file.remove(unzip_current_file)

        # increase the file counter for loading bar
        file_i <- file_i + 1

        # Print the loading bar
        cat("\r[",
            paste(rep("=", file_i), collapse = ""),
            paste(rep(" ", length(hsp_files) - file_i), collapse = ""),
            "] ",
            file_i,
            "/",
            length(hsp_files),
            " Files Processed ",
            sep = ""
        )

        flush.console()

      }

      # get return list
      results_list <- list(
        base = base_list,
        condition = condition_list,
        ocurrence = ocurrence_list,
        revenue = revenue_list,
        span = span_list,
        value = value_list,
        demo = demo_list
      )


    }


  }


}
