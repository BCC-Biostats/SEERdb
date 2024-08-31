#' Create outpat dataset
#'
#' Function will identify all of the outpat datasets in your directory and create a dataset for them
#'
#' @param db_directory path to directory where data files are held
#' @param outpat_base_files vector of names of outpat base files
#' @param outpat_condition_files vector of names of outpat condition files
#' @param outpat_demo_files vector of names of outpat demo files
#' @param outpat_occurrence_files vector of names of outpat occurrence files
#' @param outpat_revenue_files vector of names of outpat revenue files
#' @param outpat_span_files vector of names of outpat span files
#' @param outpat_value_files vector of names of outpat value files
#'
#' @return a list of dataframes from each given outpat file
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

get_outpat_data <- function(
    db_directory = ".",
    outpat_base_files = NULL,
    outpat_condition_files = NULL,
    outpat_demo_files = NULL,
    outpat_occurrence_files = NULL,
    outpat_revenue_files = NULL,
    outpat_span_files = NULL,
    outpat_value_files = NULL
){

  if (all(is.null(c({{ outpat_base_files }},
                    {{ outpat_condition_files }},
                    {{ outpat_occurrence_files }},
                    {{ outpat_revenue_files }},
                    {{ outpat_span_files }},
                    {{ outpat_value_files }},
                    {{ outpat_demo_files }})))) {

    files_in_directory <- list.files(path = db_directory)

    outpat_files <- files_in_directory[grepl("outpat", files_in_directory)] |>
      as.vector()

    outpat_base_files <- grep("outpat\\d{4}\\.base", outpat_files, value = TRUE)
    outpat_condition_files <- grep("outpat\\d{4}\\.condition", outpat_files, value = TRUE)
    outpat_occurrence_files <- grep("outpat\\d{4}\\.occurrence", outpat_files, value = TRUE)
    outpat_revenue_files <- grep("outpat\\d{4}\\.revenue", outpat_files, value = TRUE)
    outpat_span_files <- grep("outpat\\d{4}\\.span", outpat_files, value = TRUE)
    outpat_value_files <- grep("outpat\\d{4}\\.value", outpat_files, value = TRUE)
    outpat_demo_files <- grep("outpat\\d{4}\\.demo", outpat_files, value = TRUE)


    cat(crayon::blue$bold(paste0("ℹ ",
                                 length(outpat_files),
                                 " files found:\n"
    )))

    cat(crayon::blue(".base files:\n"))

    print(outpat_base_files)

    cat(crayon::blue(".condition files:\n"))

    print(outpat_condition_files)

    cat(crayon::blue(".occurence files:\n"))

    print(outpat_occurrence_files)

    cat(crayon::blue(".revenue files:\n"))

    print(outpat_revenue_files)

    cat(crayon::blue(".span files:\n"))

    print(outpat_span_files)

    cat(crayon::blue(".value files:\n"))

    print(outpat_value_files)

    cat(crayon::blue(".demo files:\n"))

    print(outpat_demo_files)

    cat("\n")

    cat(crayon::blue$bold("Unzip files to create dataset?"))

    cat("\n")

    unzip_files_q <- readline(prompt = "yes/no: ")

    # if yes then process the files
    if(tolower(unzip_files_q) %in% c("y", "yes", "ye")) {

      # Print the loading bar
      cat("\r[",
          paste(rep("=", 0), collapse = ""),
          paste(rep(" ", length(outpat_files) - 0), collapse = ""),
          "] ",
          0,
          "/",
          length(outpat_files),
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

      for (i in 1:length(outpat_base_files)) {

        # Get the current outpat base file
        current_file <- paste0(db_directory, outpat_base_files[i])

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
                                                                         start = SEERdb::outpat.base_labels$Start,
                                                                         end = SEERdb::outpat.base_labels$Stop,
                                                                         col_names = SEERdb::outpat.base_labels$name
                                                                       )
        )))

        # Add the labels
        read_data <- labelled::set_variable_labels(read_data,
                                                   .labels = setNames(
                                                     as.list(SEERdb::outpat.base_labels$label),
                                                     SEERdb::outpat.base_labels$name
                                                   )
        )

        # asign the files to the base list
        base_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", outpat_base_files[i])]] <- read_data

        # remove the unzipped file
        file.remove(unzip_current_file)

        # increase the file counter for loading bar
        file_i <- file_i + 1

        # Print the loading bar
        cat("\r[",
            paste(rep("=", file_i), collapse = ""),
            paste(rep(" ", length(outpat_files) - file_i), collapse = ""),
            "] ",
            file_i,
            "/",
            length(outpat_files),
            " Files Processed ",
            sep = ""
        )

        flush.console()

      }

      # condition file
      condition_list <- list()

      for (i in 1:length(outpat_condition_files)) {

        # Get the current outpat base file
        current_file <- paste0(db_directory, outpat_condition_files[i])

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
                                                                         start = SEERdb::outpat.condition_labels$Start,
                                                                         end = SEERdb::outpat.condition_labels$Stop,
                                                                         col_names = SEERdb::outpat.condition_labels$name
                                                                       )
        )))

        # Add the labels
        read_data <- labelled::set_variable_labels(read_data,
                                                   .labels = setNames(
                                                     as.list(SEERdb::outpat.condition_labels$label),
                                                     SEERdb::outpat.condition_labels$name
                                                   )
        )

        # asign the files to the base list
        condition_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", outpat_condition_files[i])]] <- read_data

        # remove the unzipped file
        file.remove(unzip_current_file)

        # increase the file counter for loading bar
        file_i <- file_i + 1

        # Print the loading bar
        cat("\r[",
            paste(rep("=", file_i), collapse = ""),
            paste(rep(" ", length(outpat_files) - file_i), collapse = ""),
            "] ",
            file_i,
            "/",
            length(outpat_files),
            " Files Processed ",
            sep = ""
        )

        flush.console()

      }

      # occurence file
      ocurrence_list <- list()

      for (i in 1:length(outpat_occurrence_files)) {

        # Get the current outpat base file
        current_file <- paste0(db_directory, outpat_occurrence_files[i])

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
                                                                         start = SEERdb::outpat.occurence_labels$Start,
                                                                         end = SEERdb::outpat.occurence_labels$Stop,
                                                                         col_names = SEERdb::outpat.occurence_labels$name
                                                                       )
        )))

        # Add the labels
        read_data <- labelled::set_variable_labels(read_data,
                                                   .labels = setNames(
                                                     as.list(SEERdb::outpat.occurence_labels$label),
                                                     SEERdb::outpat.occurence_labels$name
                                                   )
        )

        # asign the files to the base list
        ocurrence_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", outpat_occurrence_files[i])]] <- read_data

        # remove the unzipped file
        file.remove(unzip_current_file)

        # increase the file counter for loading bar
        file_i <- file_i + 1

        # Print the loading bar
        cat("\r[",
            paste(rep("=", file_i), collapse = ""),
            paste(rep(" ", length(outpat_files) - file_i), collapse = ""),
            "] ",
            file_i,
            "/",
            length(outpat_files),
            " Files Processed ",
            sep = ""
        )

        flush.console()

      }

      # revenue file
      revenue_list <- list()

      for (i in 1:length(outpat_revenue_files)) {

        # Get the current outpat base file
        current_file <- paste0(db_directory, outpat_revenue_files[i])

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
                                                                         start = SEERdb::outpat.revenue_labels$Start,
                                                                         end = SEERdb::outpat.revenue_labels$Stop,
                                                                         col_names = SEERdb::outpat.revenue_labels$name
                                                                       )
        )))

        # Add the labels
        read_data <- labelled::set_variable_labels(read_data,
                                                   .labels = setNames(
                                                     as.list(SEERdb::outpat.revenue_labels$label),
                                                     SEERdb::outpat.revenue_labels$name
                                                   )
        )

        # asign the files to the base list
        revenue_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", outpat_revenue_files[i])]] <- read_data

        # remove the unzipped file
        file.remove(unzip_current_file)

        # increase the file counter for loading bar
        file_i <- file_i + 1

        # Print the loading bar
        cat("\r[",
            paste(rep("=", file_i), collapse = ""),
            paste(rep(" ", length(outpat_files) - file_i), collapse = ""),
            "] ",
            file_i,
            "/",
            length(outpat_files),
            " Files Processed ",
            sep = ""
        )

        flush.console()

      }

      # span file
      span_list <- list()

      for (i in 1:length(outpat_span_files)) {

        # Get the current outpat base file
        current_file <- paste0(db_directory, outpat_span_files[i])

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
                                                                         start = SEERdb::outpat.span_labels$Start,
                                                                         end = SEERdb::outpat.span_labels$Stop,
                                                                         col_names = SEERdb::outpat.span_labels$name
                                                                       )
        )))

        # Add the labels
        read_data <- labelled::set_variable_labels(read_data,
                                                   .labels = setNames(
                                                     as.list(SEERdb::outpat.span_labels$label),
                                                     SEERdb::outpat.span_labels$name
                                                   )
        )

        # asign the files to the base list
        span_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", outpat_span_files[i])]] <- read_data

        # remove the unzipped file
        file.remove(unzip_current_file)

        # increase the file counter for loading bar
        file_i <- file_i + 1

        # Print the loading bar
        cat("\r[",
            paste(rep("=", file_i), collapse = ""),
            paste(rep(" ", length(outpat_files) - file_i), collapse = ""),
            "] ",
            file_i,
            "/",
            length(outpat_files),
            " Files Processed ",
            sep = ""
        )

        flush.console()

      }

      # value file
      value_list <- list()

      for (i in 1:length(outpat_value_files)) {

        # Get the current outpat base file
        current_file <- paste0(db_directory, outpat_value_files[i])

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
                                                                         start = SEERdb::outpat.value_labels$Start,
                                                                         end = SEERdb::outpat.value_labels$Stop,
                                                                         col_names = SEERdb::outpat.value_labels$name
                                                                       )
        )))

        # Add the labels
        read_data <- labelled::set_variable_labels(read_data,
                                                   .labels = setNames(
                                                     as.list(SEERdb::outpat.value_labels$label),
                                                     SEERdb::outpat.value_labels$name
                                                   )
        )

        # asign the files to the base list
        value_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", outpat_value_files[i])]] <- read_data

        # remove the unzipped file
        file.remove(unzip_current_file)

        # increase the file counter for loading bar
        file_i <- file_i + 1

        # Print the loading bar
        cat("\r[",
            paste(rep("=", file_i), collapse = ""),
            paste(rep(" ", length(outpat_files) - file_i), collapse = ""),
            "] ",
            file_i,
            "/",
            length(outpat_files),
            " Files Processed ",
            sep = ""
        )

        flush.console()

      }

      # demo file
      demo_list <- list()

      for (i in 1:length(outpat_demo_files)) {

        # Get the current outpat base file
        current_file <- paste0(db_directory, outpat_demo_files[i])

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
                                                                         start = SEERdb::outpat.demo_labels$Start,
                                                                         end = SEERdb::outpat.demo_labels$Stop,
                                                                         col_names = SEERdb::outpat.demo_labels$name
                                                                       )
        )))

        # Add the labels
        read_data <- labelled::set_variable_labels(read_data,
                                                   .labels = setNames(
                                                     as.list(SEERdb::outpat.demo_labels$label),
                                                     SEERdb::outpat.demo_labels$name
                                                   )
        )

        # asign the files to the base list
        demo_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", outpat_demo_files[i])]] <- read_data

        # remove the unzipped file
        file.remove(unzip_current_file)

        # increase the file counter for loading bar
        file_i <- file_i + 1

        # Print the loading bar
        cat("\r[",
            paste(rep("=", file_i), collapse = ""),
            paste(rep(" ", length(outpat_files) - file_i), collapse = ""),
            "] ",
            file_i,
            "/",
            length(outpat_files),
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

    } else {

      stop("Specify values using function parameters")

    }


  } else {

    # base file
    base_list <- list()
    if(!is.null({{ outpat_base_files }})){
    for (i in 1:length(outpat_base_files)) {

      # Get the current outpat base file
      current_file <- paste0(db_directory, outpat_base_files[i])

      # unzip the current file
      R.utils::gunzip(current_file,
                      remove = FALSE)

      # Get the unzipped file
      unzip_current_file <- sub("\\.gz$", "", current_file)

      # Read the data
      read_data <- suppressWarnings(suppressMessages(readr::read_fwf(unzip_current_file,
                                                                     col_positions = readr::fwf_positions(
                                                                       start = SEERdb::outpat.base_labels$Start,
                                                                       end = SEERdb::outpat.base_labels$Stop,
                                                                       col_names = SEERdb::outpat.base_labels$name
                                                                     )
      )))

      # Add the labels
      read_data <- labelled::set_variable_labels(read_data,
                                                 .labels = setNames(
                                                   as.list(SEERdb::outpat.base_labels$label),
                                                   SEERdb::outpat.base_labels$name
                                                 )
      )

      # asign the files to the base list
      base_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", outpat_base_files[i])]] <- read_data

      # remove the unzipped file
      file.remove(unzip_current_file)


    }
    }

    # condition file
    condition_list <- list()
    if(!is.null({{ outpat_condition_files }})){
    for (i in 1:length(outpat_condition_files)) {

      # Get the current outpat base file
      current_file <- paste0(db_directory, outpat_condition_files[i])

      # unzip the current file
      R.utils::gunzip(current_file,
                      remove = FALSE)

      # Get the unzipped file
      unzip_current_file <- sub("\\.gz$", "", current_file)

      # Read the data
      read_data <- suppressWarnings(suppressMessages(readr::read_fwf(unzip_current_file,
                                                                     col_positions = readr::fwf_positions(
                                                                       start = SEERdb::outpat.condition_labels$Start,
                                                                       end = SEERdb::outpat.condition_labels$Stop,
                                                                       col_names = SEERdb::outpat.condition_labels$name
                                                                     )
      )))

      # Add the labels
      read_data <- labelled::set_variable_labels(read_data,
                                                 .labels = setNames(
                                                   as.list(SEERdb::outpat.condition_labels$label),
                                                   SEERdb::outpat.condition_labels$name
                                                 )
      )

      # asign the files to the base list
      condition_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", outpat_condition_files[i])]] <- read_data

      # remove the unzipped file
      file.remove(unzip_current_file)

    }
    }

    # occurence file
    ocurrence_list <- list()
    if(!is.null({{ outpat_occurrence_files }})){
    for (i in 1:length(outpat_occurrence_files)) {

      # Get the current outpat base file
      current_file <- paste0(db_directory, outpat_occurrence_files[i])

      # unzip the current file
      R.utils::gunzip(current_file,
                      remove = FALSE)

      # Get the unzipped file
      unzip_current_file <- sub("\\.gz$", "", current_file)

      # Read the data
      read_data <- suppressWarnings(suppressMessages(readr::read_fwf(unzip_current_file,
                                                                     col_positions = readr::fwf_positions(
                                                                       start = SEERdb::outpat.occurence_labels$Start,
                                                                       end = SEERdb::outpat.occurence_labels$Stop,
                                                                       col_names = SEERdb::outpat.occurence_labels$name
                                                                     )
      )))

      # Add the labels
      read_data <- labelled::set_variable_labels(read_data,
                                                 .labels = setNames(
                                                   as.list(SEERdb::outpat.occurence_labels$label),
                                                   SEERdb::outpat.occurence_labels$name
                                                 )
      )

      # asign the files to the base list
      ocurrence_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", outpat_occurrence_files[i])]] <- read_data

      # remove the unzipped file
      file.remove(unzip_current_file)

    }
    }

    # revenue file
    revenue_list <- list()
    if(!is.null({{ outpat_revenue_files }})){
    for (i in 1:length(outpat_revenue_files)) {

      # Get the current outpat base file
      current_file <- paste0(db_directory, outpat_revenue_files[i])

      # unzip the current file
      R.utils::gunzip(current_file,
                      remove = FALSE)

      # Get the unzipped file
      unzip_current_file <- sub("\\.gz$", "", current_file)

      # Read the data
      read_data <- suppressWarnings(suppressMessages(readr::read_fwf(unzip_current_file,
                                                                     col_positions = readr::fwf_positions(
                                                                       start = SEERdb::outpat.revenue_labels$Start,
                                                                       end = SEERdb::outpat.revenue_labels$Stop,
                                                                       col_names = SEERdb::outpat.revenue_labels$name
                                                                     )
      )))

      # Add the labels
      read_data <- labelled::set_variable_labels(read_data,
                                                 .labels = setNames(
                                                   as.list(SEERdb::outpat.revenue_labels$label),
                                                   SEERdb::outpat.revenue_labels$name
                                                 )
      )

      # asign the files to the base list
      revenue_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", outpat_revenue_files[i])]] <- read_data

      # remove the unzipped file
      file.remove(unzip_current_file)

    }
    }

    # span file
    span_list <- list()
    if(!is.null({{ outpat_span_files }})){
    for (i in 1:length(outpat_span_files)) {

      # Get the current outpat base file
      current_file <- paste0(db_directory, outpat_span_files[i])

      # unzip the current file
      R.utils::gunzip(current_file,
                      remove = FALSE)

      # Get the unzipped file
      unzip_current_file <- sub("\\.gz$", "", current_file)

      # Read the data
      read_data <- suppressWarnings(suppressMessages(readr::read_fwf(unzip_current_file,
                                                                     col_positions = readr::fwf_positions(
                                                                       start = SEERdb::outpat.span_labels$Start,
                                                                       end = SEERdb::outpat.span_labels$Stop,
                                                                       col_names = SEERdb::outpat.span_labels$name
                                                                     )
      )))

      # Add the labels
      read_data <- labelled::set_variable_labels(read_data,
                                                 .labels = setNames(
                                                   as.list(SEERdb::outpat.span_labels$label),
                                                   SEERdb::outpat.span_labels$name
                                                 )
      )

      # asign the files to the base list
      span_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", outpat_span_files[i])]] <- read_data

      # remove the unzipped file
      file.remove(unzip_current_file)

    }
    }

    # value file
    value_list <- list()
    if(!is.null({{ outpat_value_files }})){
    for (i in 1:length(outpat_value_files)) {

      # Get the current outpat base file
      current_file <- paste0(db_directory, outpat_value_files[i])

      # unzip the current file
      R.utils::gunzip(current_file,
                      remove = FALSE)

      # Get the unzipped file
      unzip_current_file <- sub("\\.gz$", "", current_file)

      # Read the data
      read_data <- suppressWarnings(suppressMessages(readr::read_fwf(unzip_current_file,
                                                                     col_positions = readr::fwf_positions(
                                                                       start = SEERdb::outpat.value_labels$Start,
                                                                       end = SEERdb::outpat.value_labels$Stop,
                                                                       col_names = SEERdb::outpat.value_labels$name
                                                                     )
      )))

      # Add the labels
      read_data <- labelled::set_variable_labels(read_data,
                                                 .labels = setNames(
                                                   as.list(SEERdb::outpat.value_labels$label),
                                                   SEERdb::outpat.value_labels$name
                                                 )
      )

      # asign the files to the base list
      value_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", outpat_value_files[i])]] <- read_data

      # remove the unzipped file
      file.remove(unzip_current_file)

    }
    }

    # demo file
    demo_list <- list()
    if(!is.null({{ outpat_demo_files }})){
    for (i in 1:length(outpat_demo_files)) {

      # Get the current outpat base file
      current_file <- paste0(db_directory, outpat_demo_files[i])

      # unzip the current file
      R.utils::gunzip(current_file,
                      remove = FALSE)

      # Get the unzipped file
      unzip_current_file <- sub("\\.gz$", "", current_file)

      # Read the data
      read_data <- suppressWarnings(suppressMessages(readr::read_fwf(unzip_current_file,
                                                                     col_positions = readr::fwf_positions(
                                                                       start = SEERdb::outpat.demo_labels$Start,
                                                                       end = SEERdb::outpat.demo_labels$Stop,
                                                                       col_names = SEERdb::outpat.demo_labels$name
                                                                     )
      )))

      # Add the labels
      read_data <- labelled::set_variable_labels(read_data,
                                                 .labels = setNames(
                                                   as.list(SEERdb::outpat.demo_labels$label),
                                                   SEERdb::outpat.demo_labels$name
                                                 )
      )

      # asign the files to the base list
      demo_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", outpat_demo_files[i])]] <- read_data

      # remove the unzipped file
      file.remove(unzip_current_file)
    }
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
