#' Build SQL database
#'
#' @param db_directory directory where files are held
#' @param database database file from RSQLite
#'
#' @return nothing
#' @export
#'
#' @examples
#' build_SQL_db()
#'

build_SQL_db <- function(db_directory = ".", database = NULL) {

  files_in_directory <- list.files(path = db_directory)

  census_files <- files_in_directory[grepl("census", files_in_directory)] |> as.vector()
  dme_files <- files_in_directory[grepl("dme", files_in_directory)] |> as.vector()
  hospital_files <- files_in_directory[grepl("hospital", files_in_directory)] |> as.vector()
  hsp_files <- files_in_directory[grepl("hsp", files_in_directory)] |> as.vector()
  mbsf_files <- files_in_directory[grepl("mbsf", files_in_directory)] |> as.vector()
  medpar_files <- files_in_directory[grepl("medpar", files_in_directory)] |> as.vector()
  nch_files <- files_in_directory[grepl("nch", files_in_directory)] |> as.vector()
  outpat_files <- files_in_directory[grepl("outpat", files_in_directory)] |> as.vector()
  SEER_files <- files_in_directory[grepl("SEER", files_in_directory)] |> as.vector()


  num_files <- sum(as.vector(unlist(lapply(list(census_files,
                                                dme_files,
                                                hospital_files,
                                                hsp_files,
                                                mbsf_files,
                                                medpar_files,
                                                nch_files,
                                                outpat_files,
                                                SEER_files),
                                           length))))

  cat(crayon::blue$bold(paste0("ℹ ",
                               num_files,
                               " files found:\n"
  )))

  cat(crayon::blue$bold(paste0(length(SEER_files), " SEER Files Found: \n")))
  print(SEER_files)

  cat(crayon::blue$bold(paste0(length(census_files), " census Files Found: \n")))
  print(census_files)

  cat(crayon::blue$bold(paste0(length(dme_files), " dme Files Found: \n")))
  print(dme_files)

  cat(crayon::blue$bold(paste0(length(hospital_files), " hospital Files Found: \n")))
  print(hospital_files)

  cat(crayon::blue$bold(paste0(length(hsp_files), " hsp Files Found: \n")))
  print(hsp_files)

  cat(crayon::blue$bold(paste0(length(mbsf_files), " mbsf Files Found: \n")))
  print(mbsf_files)

  cat(crayon::blue$bold(paste0(length(medpar_files), " medpar Files Found: \n")))
  print(medpar_files)

  cat(crayon::blue$bold(paste0(length(nch_files), " nch Files Found: \n")))
  print(nch_files)

  cat(crayon::blue$bold(paste0(length(outpat_files), " outpat Files Found: \n")))
  print(outpat_files)

  cat(crayon::blue$bold("\nBuild datbase from these files?"))

  cat("\n")

  build_db_q <- readline(prompt = "yes/no: ")

  if (tolower(build_db_q) %in% c("y", "yes", "ye")) {

    database <- RSQLite::dbConnect(RSQLite::SQLite(),
                       paste0(database, "seer-db.sqlite"))

    file_i <- 0

    # Print the loading bar
    cat("\r[",
        paste(rep("=", file_i), collapse = ""),
        paste(rep(" ", num_files - file_i), collapse = ""),
        "] ",
        file_i,
        "/",
        num_files,
        " Files Processed",
        sep = ""
    )

    flush.console()


    # SEER FILES
    for (i in 1:length(SEER_files)) {

       cat(SEER_files[i])

       SEER_file <- SEERdb::get_SEER_data(db_directory = {{ db_directory }},
                            SEER_files = SEER_files[i]
                      )

       RSQLite::dbWriteTable(database,
                             name = paste0("SEER.", names(SEER_file)[1]),
                             value = as.data.frame(SEER_file[[1]])
                             )

       rm(SEER_file)

       file_i <- file_i + 1

       # Print the loading bar
       cat("\r[",
           paste(rep("=", file_i), collapse = ""),
           paste(rep(" ", num_files - file_i), collapse = ""),
           "] ",
           file_i,
           "/",
           num_files,
           " Files Processed",
           sep = ""
       )

       flush.console()

    }

    # census FILES
    for (i in 1:length(census_files)) {

      cat(census_files[i])

      if (grepl("tract", census_files[i])) {
        census_file <- SEERdb::get_census_data(db_directory = {{ db_directory }},
                                               census_tract_files = census_files[i]
        )

        RSQLite::dbWriteTable(database,
                              name = paste0("census.", names(census_file)[1]),
                              value = as.data.frame(census_file[[1]][[1]])
        )
      } else {

        census_file <- SEERdb::get_census_data(db_directory = {{ db_directory }},
                                               census_zipcode_files = census_files[i]
        )

        RSQLite::dbWriteTable(database,
                              name = paste0("census.", names(census_file)[2]),
                              value = as.data.frame(census_file[[2]][[1]])
        )
      }



      rm(census_file)

      file_i <- file_i + 1

      # Print the loading bar
      cat("\r[",
          paste(rep("=", file_i), collapse = ""),
          paste(rep(" ", num_files - file_i), collapse = ""),
          "] ",
          file_i,
          "/",
          num_files,
          " Files Processed",
          sep = ""
      )

      flush.console()

    }

    # dme FILES
    for (i in 1:length(dme_files)) {


      if (grepl("base", dme_files[i])) {

        dme_file <- SEERdb::get_census_data(db_directory = {{ db_directory }},
                                               census_tract_files = dme_files[i]
        )

        RSQLite::dbWriteTable(database,
                              name = paste0("dme.base.", names(dme_file[[1]])[1]),
                              value = as.data.frame(dme_file[[1]][[1]])
        )

      } else if (grepl("demo", census_files[i])) {

        dme_file <- SEERdb::get_census_data(db_directory = {{ db_directory }},
                                               census_tract_files = dme_files[i]
        )

        RSQLite::dbWriteTable(database,
                              name = paste0("dme.demo.", names(dme_file[[2]])[1]),
                              value = as.data.frame(dme_file[[2]][[1]])
        )

      } else{

        dme_file <- SEERdb::get_census_data(db_directory = {{ db_directory }},
                                               census_zipcode_files = dme_files[i]
        )

        RSQLite::dbWriteTable(database,
                              name = paste0("dme.line.", names(dme_file[[3]])[1]),
                              value = as.data.frame(dme_file[[3]][[1]])
        )

      }

      rm(dme_file)

      file_i <- file_i + 1

      # Print the loading bar
      cat("\r[",
          paste(rep("=", file_i), collapse = ""),
          paste(rep(" ", num_files - file_i), collapse = ""),
          "] ",
          file_i,
          "/",
          num_files,
          " Files Processed",
          sep = ""
      )

      flush.console()


    }

    # hospital FILES
    for (i in 1:length(hospital_files)) {

    }

    # hsp FILES
    for (i in 1:length(hsp_files)) {

    }

    # mbsf FILES
    for (i in 1:length(mbsf_files)) {

    }

    # medpar FILES
    for (i in 1:length(medpar_files)) {

    }

    # nch FILES
    for (i in 1:length(nch_files)) {

    }

    # outpat FILES
    for (i in 1:length(outpat_files)) {

    }

  }

}
