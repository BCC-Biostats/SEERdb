#' Create nch dataset
#'
#' Function will identify all of the nch datasets in your directory and create a dataset for them
#'
#' @param db_directory path to directory where data files are held
#' @param nch_base_files vector of names of nch base files
#' @param nch_demo_files vector of names of nch demo files
#' @param nch_line_files vector of names of nch line files
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

get_nch_data <- function(
    db_directory = ".",
    nch_base_files = NULL,
    nch_demo_files = NULL,
    nch_line_files = NULL
){

  # if hospital_files is null, prompt the user if files in directory are correct
  if (all(is.null(c({{ nch_base_files }},
                    {{ nch_demo_files }},
                    {{ nch_line_files }}
  )))) {

    files_in_directory <- list.files(path = db_directory)

    nch_files <- files_in_directory[grepl("nch", files_in_directory)] |>
      as.vector()

    nch_base_files <- grep("nch\\d{4}\\.base", nch_files, value = TRUE)
    nch_demo_files <- grep("nch\\d{4}\\.demo", nch_files, value = TRUE)
    nch_line_files <- grep("nch\\d{4}\\.line", nch_files, value = TRUE)

    cat(crayon::blue$bold(paste0("ℹ ",
                                 length(nch_files),
                                 " files found:\n"
    )))

    cat(crayon::blue(".base files:\n"))

    print(nch_base_files)

    cat(crayon::blue(".demo files:\n"))

    print(nch_demo_files)

    cat(crayon::blue(".line files:\n"))

    print(nch_line_files)

    cat("\n")

    cat(crayon::blue$bold("Unzip files to create dataset?"))

    cat("\n")

    unzip_files_q <- readline(prompt = "yes/no: ")

    # if yes then process the files
    if(tolower(unzip_files_q) %in% c("y", "yes", "ye")) {

      # Print the loading bar
      cat("\r[",
          paste(rep("=", 0), collapse = ""),
          paste(rep(" ", length(nch_files) - 0), collapse = ""),
          "] ",
          0,
          "/",
          length(nch_files),
          " Files Processed",
          sep = ""
      )

      flush.console()

      file_i <- 0

      # base file
      base_list <- list()

      for (i in 1:length(nch_base_files)) {

        # Get the current hsp base file
        current_file <- paste0(db_directory, nch_base_files[i])

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
                                                                         start = SEERdb::nch.base_labels$Start,
                                                                         end = SEERdb::nch.base_labels$Stop,
                                                                         col_names = SEERdb::nch.base_labels$name
                                                                       )
        )))

        # Add the labels
        read_data <- labelled::set_variable_labels(read_data,
                                                   .labels = setNames(
                                                     as.list(SEERdb::nch.base_labels$label),
                                                     SEERdb::nch.base_labels$name
                                                   )
        )

        # asign the files to the base list
        base_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", nch_base_files[i])]] <- read_data

        # remove the unzipped file
        file.remove(unzip_current_file)

        # increase the file counter for loading bar
        file_i <- file_i + 1

        # Print the loading bar
        cat("\r[",
            paste(rep("=", file_i), collapse = ""),
            paste(rep(" ", length(nch_files) - file_i), collapse = ""),
            "] ",
            file_i,
            "/",
            length(nch_files),
            " Files Processed ",
            sep = ""
        )

        flush.console()

      }

      # demo file
      demo_list <- list()

      for (i in 1:length(nch_demo_files)) {

        # Get the current hsp base file
        current_file <- paste0(db_directory, nch_demo_files[i])

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
                                                                         start = SEERdb::nch.demo_labels$Start,
                                                                         end = SEERdb::nch.demo_labels$Stop,
                                                                         col_names = SEERdb::nch.demo_labels$name
                                                                       )
        )))

        # Add the labels
        read_data <- labelled::set_variable_labels(read_data,
                                                   .labels = setNames(
                                                     as.list(SEERdb::nch.demo_labels$label),
                                                     SEERdb::nch.demo_labels$name
                                                   )
        )

        # asign the files to the base list
        demo_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", nch_demo_files[i])]] <- read_data

        # remove the unzipped file
        file.remove(unzip_current_file)

        # increase the file counter for loading bar
        file_i <- file_i + 1

        # Print the loading bar
        cat("\r[",
            paste(rep("=", file_i), collapse = ""),
            paste(rep(" ", length(nch_files) - file_i), collapse = ""),
            "] ",
            file_i,
            "/",
            length(nch_files),
            " Files Processed ",
            sep = ""
        )

        flush.console()

      }

      # line file
      line_list <- list()

      for (i in 1:length(nch_line_files)) {

        # Get the current hsp base file
        current_file <- paste0(db_directory, nch_line_files[i])

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
                                                                         start = SEERdb::nch.line_labels$Start,
                                                                         end = SEERdb::nch.line_labels$Stop,
                                                                         col_names = SEERdb::nch.line_labels$name
                                                                       )
        )))

        # Add the labels
        read_data <- labelled::set_variable_labels(read_data,
                                                   .labels = setNames(
                                                     as.list(SEERdb::nch.line_labels$label),
                                                     SEERdb::nch.line_labels$name
                                                   )
        )

        # asign the files to the base list
        line_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", nch_line_files[i])]] <- read_data

        # remove the unzipped file
        file.remove(unzip_current_file)

        # increase the file counter for loading bar
        file_i <- file_i + 1

        # Print the loading bar
        cat("\r[",
            paste(rep("=", file_i), collapse = ""),
            paste(rep(" ", length(nch_files) - file_i), collapse = ""),
            "] ",
            file_i,
            "/",
            length(nch_files),
            " Files Processed ",
            sep = ""
        )

        flush.console()

      }

      results_list <- list(
        base = base_list,
        demo = demo_list,
        line = line_list
      )



    } else {
      stop("Specify values using function parameters")

    }

  } else {

    # base file
    base_list <- list()

    for (i in 1:length(nch_base_files)) {

      # Get the current hsp base file
      current_file <- paste0(db_directory, nch_base_files[i])

      # unzip the current file
      R.utils::gunzip(current_file,
                      remove = FALSE)

      # Get the unzipped file
      unzip_current_file <- sub("\\.gz$", "", current_file)

      # Read the data
      read_data <- suppressWarnings(suppressMessages(readr::read_fwf(unzip_current_file,
                                                                     col_positions = readr::fwf_positions(
                                                                       start = SEERdb::nch.base_labels$Start,
                                                                       end = SEERdb::nch.base_labels$Stop,
                                                                       col_names = SEERdb::nch.base_labels$name
                                                                     )
      )))

      # Add the labels
      read_data <- labelled::set_variable_labels(read_data,
                                                 .labels = setNames(
                                                   as.list(SEERdb::nch.base_labels$label),
                                                   SEERdb::nch.base_labels$name
                                                 )
      )

      # asign the files to the base list
      base_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", nch_base_files[i])]] <- read_data

      # remove the unzipped file
      file.remove(unzip_current_file)

    }

    # demo file
    demo_list <- list()

    for (i in 1:length(nch_demo_files)) {

      # Get the current hsp base file
      current_file <- paste0(db_directory, nch_demo_files[i])

      # unzip the current file
      R.utils::gunzip(current_file,
                      remove = FALSE)

      # Get the unzipped file
      unzip_current_file <- sub("\\.gz$", "", current_file)

      # Read the data
      read_data <- suppressWarnings(suppressMessages(readr::read_fwf(unzip_current_file,
                                                                     col_positions = readr::fwf_positions(
                                                                       start = SEERdb::nch.demo_labels$Start,
                                                                       end = SEERdb::nch.demo_labels$Stop,
                                                                       col_names = SEERdb::nch.demo_labels$name
                                                                     )
      )))

      # Add the labels
      read_data <- labelled::set_variable_labels(read_data,
                                                 .labels = setNames(
                                                   as.list(SEERdb::nch.demo_labels$label),
                                                   SEERdb::nch.demo_labels$name
                                                 )
      )

      # asign the files to the base list
      demo_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", nch_demo_files[i])]] <- read_data

      # remove the unzipped file
      file.remove(unzip_current_file)

    }

    # line file
    line_list <- list()

    for (i in 1:length(nch_line_files)) {

      # Get the current hsp base file
      current_file <- paste0(db_directory, nch_line_files[i])

      # unzip the current file
      R.utils::gunzip(current_file,
                      remove = FALSE)

      # Get the unzipped file
      unzip_current_file <- sub("\\.gz$", "", current_file)

      # Read the data
      read_data <- suppressWarnings(suppressMessages(readr::read_fwf(unzip_current_file,
                                                                     col_positions = readr::fwf_positions(
                                                                       start = SEERdb::nch.line_labels$Start,
                                                                       end = SEERdb::nch.line_labels$Stop,
                                                                       col_names = SEERdb::nch.line_labels$name
                                                                     )
      )))

      # Add the labels
      read_data <- labelled::set_variable_labels(read_data,
                                                 .labels = setNames(
                                                   as.list(SEERdb::nch.line_labels$label),
                                                   SEERdb::nch.line_labels$name
                                                 )
      )

      # asign the files to the base list
      line_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", nch_line_files[i])]] <- read_data

      # remove the unzipped file
      file.remove(unzip_current_file)

    }

    results_list <- list(
      base = base_list,
      demo = demo_list,
      line = line_list
    )

  }

}
