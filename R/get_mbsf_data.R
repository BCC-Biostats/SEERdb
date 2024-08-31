#' Create mbsf dataset
#'
#' Function will identify all of the mbsf datasets in your directory and create a dataset for them
#'
#' @param db_directory path to directory where data files are held
#' @param ab.summary_files vector of names of ab summary files
#' @param abcd.summary_files vector of names of abcd summary files
#' @param cc.summary_files vector of names of cc summary files
#' @param oth.cc.summary_files vector of names of other cc summary files
#'
#' @return a list of dataframes from each given mbsf file
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

get_mbsf_data <- function(
    db_directory = ".",
    ab.summary_files = NULL,
    abcd.summary_files = NULL,
    cc.summary_files = NULL,
    oth.cc.summary_files = NULL
){

  if (all(is.null(c({{ ab.summary_files }},
                    {{ abcd.summary_files }},
                    {{ cc.summary_files }},
                    {{ oth.cc.summary_files }}
  )))) {

    # list files and prompt user

    files_in_directory <- list.files(path = db_directory)

    mbsf_files <- files_in_directory[grepl("mbsf", files_in_directory)] |>
      as.vector()

    mbsf_ab_summary_files <- grep(".ab.summary", mbsf_files, value = TRUE)
    mbsf_abcd_summary_files <- grep(".abcd.summary", mbsf_files, value = TRUE)
    mbsf_cc_summary_files <- grep("f.cc.summary", mbsf_files, value = TRUE)
    mbsf_oth_cc_summary_files <- grep("oth.cc.summary", mbsf_files, value = TRUE)

    cat(crayon::blue$bold(paste0("ℹ ",
                                 length(mbsf_files),
                                 " files found:\n"
    )))

    cat(crayon::blue("ab.summary files:\n"))

    print(mbsf_ab_summary_files)

    cat(crayon::blue("abcd.summary files:\n"))

    print(mbsf_abcd_summary_files)

    cat(crayon::blue("cc.summary files:\n"))

    print(mbsf_cc_summary_files)

    cat(crayon::blue("oth.cc.summary files:\n"))

    print(mbsf_oth_cc_summary_files)

    cat("\n")

    cat(crayon::blue$bold("Unzip files to create dataset?"))

    cat("\n")

    unzip_files_q <- readline(prompt = "yes/no: ")

    # if yes then process the files
    if(tolower(unzip_files_q) %in% c("y", "yes", "ye")) {

      file_i <- 0

      # Print the loading bar
      cat("\r[",
          paste(rep("=", 0), collapse = ""),
          paste(rep(" ", length(mbsf_files) - 0), collapse = ""),
          "] ",
          0,
          "/",
          length(mbsf_files),
          " Files Processed",
          sep = ""
      )

      flush.console()

      results_list <- list()

      # ab.summary file
      ab.summary_list <- list()

      for (i in 1:length(mbsf_ab_summary_files)) {

        # Get the current hsp base file
        current_file <- paste0(db_directory, mbsf_ab_summary_files[i])

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
                                                                         start = SEERdb::MBSF.AB_labels$Start,
                                                                         end = SEERdb::MBSF.AB_labels$Stop,
                                                                         col_names = SEERdb::MBSF.AB_labels$name
                                                                       )
        )))

        # Add the labels
        read_data <- labelled::set_variable_labels(read_data,
                                                   .labels = setNames(
                                                     as.list(SEERdb::MBSF.AB_labels$label),
                                                     SEERdb::MBSF.AB_labels$name
                                                   )
        )

        # asign the files to the base list
        ab.summary_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", mbsf_ab_summary_files[i])]] <- read_data

        # remove the unzipped file
        file.remove(unzip_current_file)

        # increase the file counter for loading bar
        file_i <- file_i + 1

        # Print the loading bar
        cat("\r[",
            paste(rep("=", file_i), collapse = ""),
            paste(rep(" ", length(mbsf_files) - file_i), collapse = ""),
            "] ",
            file_i,
            "/",
            length(mbsf_files),
            " Files Processed ",
            sep = ""
        )

        flush.console()

      }

      # ab.summary file
      abcd.summary_list <- list()

      for (i in 1:length(mbsf_abcd_summary_files)) {

        # Get the current hsp base file
        current_file <- paste0(db_directory, mbsf_abcd_summary_files[i])

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
                                                                         start = SEERdb::MBSF.ABCD_labels$Start,
                                                                         end = SEERdb::MBSF.ABCD_labels$Stop,
                                                                         col_names = SEERdb::MBSF.ABCD_labels$name
                                                                       )
        )))

        # Add the labels
        read_data <- labelled::set_variable_labels(read_data,
                                                   .labels = setNames(
                                                     as.list(SEERdb::MBSF.ABCD_labels$label),
                                                     SEERdb::MBSF.ABCD_labels$name
                                                   )
        )

        # asign the files to the base list
        abcd.summary_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", mbsf_abcd_summary_files[i])]] <- read_data

        # remove the unzipped file
        file.remove(unzip_current_file)

        # increase the file counter for loading bar
        file_i <- file_i + 1

        # Print the loading bar
        cat("\r[",
            paste(rep("=", file_i), collapse = ""),
            paste(rep(" ", length(mbsf_files) - file_i), collapse = ""),
            "] ",
            file_i,
            "/",
            length(mbsf_files),
            " Files Processed ",
            sep = ""
        )

        flush.console()

      }

      # ab.summary file
      cc.summary_list <- list()

      for (i in 1:length(mbsf_cc_summary_files)) {

        # Get the current hsp base file
        current_file <- paste0(db_directory, mbsf_cc_summary_files[i])

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
                                                                         start = SEERdb::CCflag27_labels$Start,
                                                                         end = SEERdb::CCflag27_labels$Stop,
                                                                         col_names = SEERdb::CCflag27_labels$name
                                                                       )
        )))

        # Add the labels
        read_data <- labelled::set_variable_labels(read_data,
                                                   .labels = setNames(
                                                     as.list(SEERdb::CCflag27_labels$label),
                                                     SEERdb::CCflag27_labels$name
                                                   )
        )

        # asign the files to the base list
        cc.summary_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", mbsf_cc_summary_files[i])]] <- read_data

        # remove the unzipped file
        file.remove(unzip_current_file)

        # increase the file counter for loading bar
        file_i <- file_i + 1

        # Print the loading bar
        cat("\r[",
            paste(rep("=", file_i), collapse = ""),
            paste(rep(" ", length(mbsf_files) - file_i), collapse = ""),
            "] ",
            file_i,
            "/",
            length(mbsf_files),
            " Files Processed ",
            sep = ""
        )

        flush.console()

      }

      # ab.summary file
      oth.cc.summary_list <- list()

      for (i in 1:length(mbsf_oth_cc_summary_files)) {

        # Get the current hsp base file
        current_file <- paste0(db_directory, mbsf_oth_cc_summary_files[i])

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
                                                                         start = SEERdb::Other.CCW_labels$Start,
                                                                         end = SEERdb::Other.CCW_labels$Stop,
                                                                         col_names = SEERdb::Other.CCW_labels$name
                                                                       )
        )))

        # Add the labels
        read_data <- labelled::set_variable_labels(read_data,
                                                   .labels = setNames(
                                                     as.list(SEERdb::Other.CCW_labels$label),
                                                     SEERdb::Other.CCW_labels$name
                                                   )
        )

        # asign the files to the base list
        oth.cc.summary_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", mbsf_oth_cc_summary_files[i])]] <- read_data

        # remove the unzipped file
        file.remove(unzip_current_file)

        # increase the file counter for loading bar
        file_i <- file_i + 1

        # Print the loading bar
        cat("\r[",
            paste(rep("=", file_i), collapse = ""),
            paste(rep(" ", length(mbsf_files) - file_i), collapse = ""),
            "] ",
            file_i,
            "/",
            length(mbsf_files),
            " Files Processed ",
            sep = ""
        )

        flush.console()

      }

      results_list <- list(
        ab.summary = ab.summary_list,
        abcd.summary = abcd.summary_list,
        cc.summary = cc.summary_list,
        oth.cc.summary = oth.cc.summary_list
      )

    } else {

      stop("Specify values using function parameters")

    }


  } else {

    results_list <- list()

    # ab.summary file
    ab.summary_list <- list()
    if(!is.null({{ ab.summary_files }})){
    for (i in 1:length(ab.summary_files)) {

      # Get the current hsp base file
      current_file <- paste0(db_directory, ab.summary_files[i])

      # unzip the current file
      R.utils::gunzip(current_file,
                      remove = FALSE)

      # Get the unzipped file
      unzip_current_file <- sub("\\.gz$", "", current_file)

      # Read the data
      read_data <- suppressWarnings(suppressMessages(readr::read_fwf(unzip_current_file,
                                                                     col_positions = readr::fwf_positions(
                                                                       start = SEERdb::MBSF.AB_labels$Start,
                                                                       end = SEERdb::MBSF.AB_labels$Stop,
                                                                       col_names = SEERdb::MBSF.AB_labels$name
                                                                     )
      )))

      # Add the labels
      read_data <- labelled::set_variable_labels(read_data,
                                                 .labels = setNames(
                                                   as.list(SEERdb::MBSF.AB_labels$label),
                                                   SEERdb::MBSF.AB_labels$name
                                                 )
      )

      # asign the files to the base list
      ab.summary_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", ab.summary_files[i])]] <- read_data

      # remove the unzipped file
      file.remove(unzip_current_file)

    }
    }

    # ab.summary file
    abcd.summary_list <- list()
    if(!is.null({{ abcd.summary_files }})){
    for (i in 1:length(abcd.summary_files)) {

      # Get the current hsp base file
      current_file <- paste0(db_directory, abcd.summary_files[i])

      # unzip the current file
      R.utils::gunzip(current_file,
                      remove = FALSE)

      # Get the unzipped file
      unzip_current_file <- sub("\\.gz$", "", current_file)

      # Read the data
      read_data <- suppressWarnings(suppressMessages(readr::read_fwf(unzip_current_file,
                                                                     col_positions = readr::fwf_positions(
                                                                       start = SEERdb::MBSF.ABCD_labels$Start,
                                                                       end = SEERdb::MBSF.ABCD_labels$Stop,
                                                                       col_names = SEERdb::MBSF.ABCD_labels$name
                                                                     )
      )))

      # Add the labels
      read_data <- labelled::set_variable_labels(read_data,
                                                 .labels = setNames(
                                                   as.list(SEERdb::MBSF.ABCD_labels$label),
                                                   SEERdb::MBSF.ABCD_labels$name
                                                 )
      )

      # asign the files to the base list
      abcd.summary_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", abcd.summary_files[i])]] <- read_data

      # remove the unzipped file
      file.remove(unzip_current_file)

    }
    }

    # ab.summary file
    cc.summary_list <- list()
    if(!is.null({{ cc.summary_files }})){
    for (i in 1:length(cc.summary_files)) {

      # Get the current hsp base file
      current_file <- paste0(db_directory, cc.summary_files[i])

      # unzip the current file
      R.utils::gunzip(current_file,
                      remove = FALSE)

      # Get the unzipped file
      unzip_current_file <- sub("\\.gz$", "", current_file)

      # Read the data
      read_data <- suppressWarnings(suppressMessages(readr::read_fwf(unzip_current_file,
                                                                     col_positions = readr::fwf_positions(
                                                                       start = SEERdb::CCflag27_labels$Start,
                                                                       end = SEERdb::CCflag27_labels$Stop,
                                                                       col_names = SEERdb::CCflag27_labels$name
                                                                     )
      )))

      # Add the labels
      read_data <- labelled::set_variable_labels(read_data,
                                                 .labels = setNames(
                                                   as.list(SEERdb::CCflag27_labels$label),
                                                   SEERdb::CCflag27_labels$name
                                                 )
      )

      # asign the files to the base list
      cc.summary_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", cc.summary_files[i])]] <- read_data

      # remove the unzipped file
      file.remove(unzip_current_file)

    }
    }

    # ab.summary file
    oth.cc.summary_list <- list()
    if(!is.null({{ oth.cc.summary_files }})){
    for (i in 1:length(oth.cc.summary_files)) {

      # Get the current hsp base file
      current_file <- paste0(db_directory, oth.cc.summary_files[i])

      # unzip the current file
      R.utils::gunzip(current_file,
                      remove = FALSE)

      # Get the unzipped file
      unzip_current_file <- sub("\\.gz$", "", current_file)

      # Read the data
      read_data <- suppressWarnings(suppressMessages(readr::read_fwf(unzip_current_file,
                                                                     col_positions = readr::fwf_positions(
                                                                       start = SEERdb::Other.CCW_labels$Start,
                                                                       end = SEERdb::Other.CCW_labels$Stop,
                                                                       col_names = SEERdb::Other.CCW_labels$name
                                                                     )
      )))

      # Add the labels
      read_data <- labelled::set_variable_labels(read_data,
                                                 .labels = setNames(
                                                   as.list(SEERdb::Other.CCW_labels$label),
                                                   SEERdb::Other.CCW_labels$name
                                                 )
      )

      # asign the files to the base list
      oth.cc.summary_list[[sub("^([a-zA-Z]+\\d{4})\\..*", "\\1", oth.cc.summary_files[i])]] <- read_data

      # remove the unzipped file
      file.remove(unzip_current_file)

    }
    }

    results_list <- list(
      ab.summary = ab.summary_list,
      abcd.summary = abcd.summary_list,
      cc.summary = cc.summary_list,
      oth.cc.summary = oth.cc.summary_list
    )


  }

  return(results_list)

}
