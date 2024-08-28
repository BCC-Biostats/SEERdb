#' Create census dataset
#'
#' Function will identify all of the census datasets in your directory and create a dataset for them
#'
#' @param db_directory path to directory where data files are held
#' @param census_tract_files vector of names of census tract files
#' @param census_zipcode_files  vector of names of census zipcode files
#'
#' @return a list of dataframes from each given census file
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

get_census_data <- function(
    db_directory = ".",
    census_tract_files = NULL,
    census_zipcode_files = NULL
){

  # if files are null, prompt the user if files in directory are correct
  if (all(is.null(c({{ census_tract_files }},
                    {{ census_zipcode_files }}
  )))) {


    files_in_directory <- list.files(path = db_directory)

    census_files <- files_in_directory[grepl("census", files_in_directory)] |>
      as.vector()

    census_tract_files <- grep("tract", census_files, value = TRUE)
    census_zipcode_files <- grep("zipcode", census_files, value = TRUE)

    cat(crayon::blue$bold(paste0("ℹ ",
                                 length(census_files),
                                 " files found:\n"
    )))

    cat(crayon::blue("tract files:\n"))

    print(census_tract_files)

    cat(crayon::blue("zipcode files:\n"))

    print(census_zipcode_files)

    cat("\n")

    cat(crayon::blue$bold("Unzip files to create dataset?"))

    cat("\n")

    unzip_files_q <- readline(prompt = "yes/no: ")

    # if yes then process the files
    if(tolower(unzip_files_q) %in% c("y", "yes", "ye")) {

      # Print the loading bar
      cat("\r[",
          paste(rep("=", 0), collapse = ""),
          paste(rep(" ", length(census_files) - 0), collapse = ""),
          "] ",
          0,
          "/",
          length(census_files),
          " Files Processed",
          sep = ""
      )

      flush.console()

      file_i <- 0

      # tract file
      tract_list <- list()

      for (i in 1:length(census_tract_files)) {

        # Get the current hsp base file
        current_file <- paste0(db_directory, census_tract_files[i])

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
                                                                         start = SEERdb::tract.census.file_labels$Start,
                                                                         end = SEERdb::tract.census.file_labels$Stop,
                                                                         col_names = SEERdb::tract.census.file_labels$name
                                                                       )
        )))

        # Add the labels
        read_data <- labelled::set_variable_labels(read_data,
                                                   .labels = setNames(
                                                     as.list(SEERdb::tract.census.file_labels$label),
                                                     SEERdb::tract.census.file_labels$name
                                                   )
        )

        # asign the files to the base list
        tract_list[[paste0("tract", i)]] <- read_data

        # remove the unzipped file
        file.remove(unzip_current_file)

        # increase the file counter for loading bar
        file_i <- file_i + 1

        # Print the loading bar
        cat("\r[",
            paste(rep("=", file_i), collapse = ""),
            paste(rep(" ", length(census_tract_files) - file_i), collapse = ""),
            "] ",
            file_i,
            "/",
            length(census_tract_files),
            " Files Processed ",
            sep = ""
        )

        flush.console()

      }

      # zipcode file
      zipcode_list <- list()

      for (i in 1:length(census_zipcode_files)) {

        # Get the current hsp base file
        current_file <- paste0(db_directory, census_zipcode_files[i])

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
                                                                         start = SEERdb::zipcode.census.file_labels$Start,
                                                                         end = SEERdb::zipcode.census.file_labels$Stop,
                                                                         col_names = SEERdb::zipcode.census.file_labels$name
                                                                       )
        )))

        # Add the labels
        read_data <- labelled::set_variable_labels(read_data,
                                                   .labels = setNames(
                                                     as.list(SEERdb::zipcode.census.file_labels$label),
                                                     SEERdb::zipcode.census.file_labels$name
                                                   )
        )

        # asign the files to the base list
        zipcode_list[[paste0("zipcode", i)]] <- read_data

        # remove the unzipped file
        file.remove(unzip_current_file)

        # increase the file counter for loading bar
        file_i <- file_i + 1

        # Print the loading bar
        cat("\r[",
            paste(rep("=", file_i), collapse = ""),
            paste(rep(" ", length(census_files) - file_i), collapse = ""),
            "] ",
            file_i,
            "/",
            length(census_files),
            " Files Processed ",
            sep = ""
        )

        flush.console()

      }

      results_list <- list(
        tract = tract_list,
        zipcode = zipcode_list
      )


    } else {

      stop("Specify values using function parameters")

    }

  } else {

    # tract file
    tract_list <- list()
    if(length(census_tract_files) > 0) {
    for (i in 1:length(census_tract_files)) {

      # Get the current hsp base file
      current_file <- paste0(db_directory, census_tract_files[i])

      # unzip the current file
      R.utils::gunzip(current_file,
                      remove = FALSE)

      # Get the unzipped file
      unzip_current_file <- sub("\\.gz$", "", current_file)

      # Read the data
      read_data <- suppressWarnings(suppressMessages(readr::read_fwf(unzip_current_file,
                                                                     col_positions = readr::fwf_positions(
                                                                       start = SEERdb::tract.census.file_labels$Start,
                                                                       end = SEERdb::tract.census.file_labels$Stop,
                                                                       col_names = SEERdb::tract.census.file_labels$name
                                                                     )
      )))

      # Add the labels
      read_data <- labelled::set_variable_labels(read_data,
                                                 .labels = setNames(
                                                   as.list(SEERdb::tract.census.file_labels$label),
                                                   SEERdb::tract.census.file_labels$name
                                                 )
      )

      # asign the files to the base list
      tract_list[[paste0("tract", i)]] <- read_data

      # remove the unzipped file
      file.remove(unzip_current_file)

    }
    }

    # zipcode file
    zipcode_list <- list()
    if(length(census_zipcode_files) > 0) {
    for (i in 1:length(census_zipcode_files)) {

      # Get the current hsp base file
      current_file <- paste0(db_directory, census_zipcode_files[i])

      # unzip the current file
      R.utils::gunzip(current_file,
                      remove = FALSE)

      # Get the unzipped file
      unzip_current_file <- sub("\\.gz$", "", current_file)

      # Read the data
      read_data <- suppressWarnings(suppressMessages(readr::read_fwf(unzip_current_file,
                                                                     col_positions = readr::fwf_positions(
                                                                       start = SEERdb::zipcode.census.file_labels$Start,
                                                                       end = SEERdb::zipcode.census.file_labels$Stop,
                                                                       col_names = SEERdb::zipcode.census.file_labels$name
                                                                     )
      )))

      # Add the labels
      read_data <- labelled::set_variable_labels(read_data,
                                                 .labels = setNames(
                                                   as.list(SEERdb::zipcode.census.file_labels$label),
                                                   SEERdb::zipcode.census.file_labels$name
                                                 )
      )

      # asign the files to the base list
      zipcode_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", census_zipcode_files[i])]] <- read_data

      # remove the unzipped file
      file.remove(unzip_current_file)

    }
    }

    results_list <- list(
      tract = tract_list,
      zipcode = zipcode_list
    )

  }

  return(results_list)

}
