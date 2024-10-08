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
    cat("\r", file_i,
        "/",
        num_files,
        " Files Processed",
        sep = ""
    )

    flush.console()


    # # SEER FILES
    # for (i in 1:length(SEER_files)) {
    #
    #   cat(crayon::yellow$bold(paste0(" | Working on: ", SEER_files[i])))
    #
    #    SEER_file <- SEERdb::get_SEER_data(db_directory = {{ db_directory }},
    #                         SEER_files = SEER_files[i]
    #                   )
    #
    #    RSQLite::dbWriteTable(database,
    #                          name = paste0("SEER.", names(SEER_file)[1]),
    #                          value = as.data.frame(SEER_file[[1]])
    #                          )
    #
    #    rm(SEER_file)
    #
    #    file_i <- file_i + 1
    #
    #    # Print the loading bar
    #    cat("\r", file_i,
    #        "/",
    #        num_files,
    #        " Files Processed",
    #        sep = ""
    #    )
    #
    #    flush.console()
    #
    # }
    #
    # # census FILES
    # for (i in 1:length(census_files)) {
    #
    #   cat(crayon::yellow$bold(paste0(" | Working on: ", census_files[i])))
    #
    #   if (grepl("tract", census_files[i])) {
    #     census_file <- SEERdb::get_census_data(db_directory = {{ db_directory }},
    #                                            census_tract_files = census_files[i]
    #     )
    #
    #     RSQLite::dbWriteTable(database,
    #                           name = paste0("census.", names(census_file)[1]),
    #                           value = as.data.frame(census_file[[1]][[1]])
    #     )
    #   } else {
    #
    #     census_file <- SEERdb::get_census_data(db_directory = {{ db_directory }},
    #                                            census_zipcode_files = census_files[i]
    #     )
    #
    #     RSQLite::dbWriteTable(database,
    #                           name = paste0("census.", names(census_file)[2]),
    #                           value = as.data.frame(census_file[[2]][[1]])
    #     )
    #   }
    #
    #
    #
    #   rm(census_file)
    #
    #   file_i <- file_i + 1
    #
    #   # Print the loading bar
    #   cat("\r", file_i,
    #       "/",
    #       num_files,
    #       " Files Processed",
    #       sep = ""
    #   )
    #
    #   flush.console()
    #
    # }
    #
    # # dme FILES
    # for (i in 1:length(dme_files)) {
    #
    #   cat(crayon::yellow$bold(paste0(" | Working on: ", dme_files[i])))
    #
    #
    #   if (grepl("base", dme_files[i])) {
    #
    #     dme_file <- SEERdb::get_dme_data(db_directory = {{ db_directory }},
    #                                            dme_base_files = dme_files[i]
    #     )
    #
    #
    #     RSQLite::dbWriteTable(database,
    #                           name = paste0("dme.base.", names(dme_file$base)[1]),
    #                           value = as.data.frame(dme_file$base[[1]])
    #     )
    #
    #   }
    #
    #   if (grepl("demo", dme_files[i])) {
    #
    #     dme_file <- SEERdb::get_dme_data(db_directory = {{ db_directory }},
    #                                            dme_demo_files = dme_files[i]
    #     )
    #
    #     RSQLite::dbWriteTable(database,
    #                           name = paste0("dme.demo.", names(dme_file$demo)[1]),
    #                           value = as.data.frame(dme_file$demo[[1]])
    #     )
    #
    #   }
    #
    #   if (grepl("line", dme_files[i])){
    #
    #     dme_file <- SEERdb::get_dme_data(db_directory = {{ db_directory }},
    #                                            dme_line_files = dme_files[i]
    #     )
    #
    #     RSQLite::dbWriteTable(database,
    #                           name = paste0("dme.line.", names(dme_file$line)[1]),
    #                           value = as.data.frame(dme_file$line[[1]])
    #     )
    #
    #   }
    #
    #   rm(dme_file)
    #
    #   file_i <- file_i + 1
    #
    #   # Print the loading bar
    #   cat("\r", file_i,
    #       "/",
    #       num_files,
    #       " Files Processed",
    #       sep = ""
    #   )
    #
    #   flush.console()
    #
    #
    # }
    #
    # # hospital FILES
    # for (i in 1:length(hospital_files)) {
    #
    #   cat(crayon::yellow$bold(paste0(" | Working on: ", hospital_files[i])))
    #
    #   hospital_file <- SEERdb::get_hospital_data(db_directory = {{ db_directory }},
    #                                              hospital_files = hospital_files[i]
    #   )
    #
    #
    #   RSQLite::dbWriteTable(database,
    #                         name = paste0("hospital.", names(hospital_file)[1]),
    #                         value = as.data.frame(hospital_file[[1]])
    #   )
    #
    #   rm(hospital_file)
    #
    #   file_i <- file_i + 1
    #
    #   # Print the loading bar
    #   cat("\r", file_i,
    #       "/",
    #       num_files,
    #       " Files Processed",
    #       sep = ""
    #   )
    #
    #   flush.console()
    #
    # }
    #
    # # hsp FILES
    # for (i in 1:length(hsp_files)) {
    #
    #   cat(crayon::yellow$bold(paste0(" | Working on: ", hsp_files[i])))
    #
    #
    #   if (grepl("base", hsp_files[i])) {
    #
    #     hsp_file <- SEERdb::get_hsp_data(db_directory = {{ db_directory }},
    #                                      hsp_base_files = hsp_files[i]
    #     )
    #
    #
    #     RSQLite::dbWriteTable(database,
    #                           name = paste0("hsp.base.", names(hsp_file$base)[1]),
    #                           value = as.data.frame(hsp_file$base[[1]])
    #     )
    #
    #   }
    #
    #   if (grepl("demo", hsp_files[i])) {
    #
    #     hsp_file <- SEERdb::get_hsp_data(db_directory = {{ db_directory }},
    #                                      hsp_demo_files = hsp_files[i]
    #     )
    #
    #     RSQLite::dbWriteTable(database,
    #                           name = paste0("hsp.demo.", names(hsp_file$demo)[1]),
    #                           value = as.data.frame(hsp_file$demo[[1]])
    #     )
    #
    #   }
    #
    #   if (grepl("condition", hsp_files[i])){
    #
    #     hsp_file <- SEERdb::get_hsp_data(db_directory = {{ db_directory }},
    #                                      hsp_condition_files = hsp_files[i]
    #     )
    #
    #     RSQLite::dbWriteTable(database,
    #                           name = paste0("hsp.condition.", names(hsp_file$condition)[1]),
    #                           value = as.data.frame(hsp_file$condition[[1]])
    #     )
    #
    #   }
    #
    #   if (grepl("occurrence", hsp_files[i])){
    #
    #     hsp_file <- SEERdb::get_hsp_data(db_directory = {{ db_directory }},
    #                                      hsp_occurence_files = hsp_files[i]
    #     )
    #
    #     RSQLite::dbWriteTable(database,
    #                           name = paste0("hsp.occurrence.", names(hsp_file$ocurrence)[1]),
    #                           value = as.data.frame(hsp_file$ocurrence[[1]])
    #     )
    #
    #   }
    #
    #   if (grepl("revenue", hsp_files[i])){
    #
    #     hsp_file <- SEERdb::get_hsp_data(db_directory = {{ db_directory }},
    #                                      hsp_revenue_files = hsp_files[i]
    #     )
    #
    #     RSQLite::dbWriteTable(database,
    #                           name = paste0("hsp.revenue.", names(hsp_file$revenue)[1]),
    #                           value = as.data.frame(hsp_file$revenue[[1]])
    #     )
    #
    #   }
    #
    #
    #   if (grepl("span", hsp_files[i])){
    #
    #     hsp_file <- SEERdb::get_hsp_data(db_directory = {{ db_directory }},
    #                                      hsp_span_files = hsp_files[i]
    #     )
    #
    #     RSQLite::dbWriteTable(database,
    #                           name = paste0("hsp.span.", names(hsp_file$span)[1]),
    #                           value = as.data.frame(hsp_file$span[[1]])
    #     )
    #
    #   }
    #
    #
    #   if (grepl("value", hsp_files[i])){
    #
    #     hsp_file <- SEERdb::get_hsp_data(db_directory = {{ db_directory }},
    #                                      hsp_value_files = hsp_files[i]
    #     )
    #
    #     RSQLite::dbWriteTable(database,
    #                           name = paste0("hsp.value.", names(hsp_file$value)[1]),
    #                           value = as.data.frame(hsp_file$value[[1]])
    #     )
    #
    #   }
    #
    #   rm(hsp_file)
    #
    #   file_i <- file_i + 1
    #
    #   # Print the loading bar
    #   cat("\r", file_i,
    #       "/",
    #       num_files,
    #       " Files Processed",
    #       sep = ""
    #   )
    #
    #   flush.console()
    #
    # }
    #
    # # mbsf FILES
    # for (i in 1:length(mbsf_files)) {
    #
    #   cat(crayon::yellow$bold(paste0(" | Working on: ", mbsf_files[i])))
    #
    #   if (grepl("ab.summary", mbsf_files[i])) {
    #
    #     mbsf_file <- SEERdb::get_mbsf_data(db_directory = {{ db_directory }},
    #                                       ab.summary_files = mbsf_files[i]
    #     )
    #
    #     RSQLite::dbWriteTable(database,
    #                           name = paste0(names(mbsf_file$ab.summary)[1]),
    #                           value = as.data.frame(mbsf_file$ab.summary[[1]])
    #     )
    #
    #   }
    #
    #   if (grepl("abcd.summary", mbsf_files[i])) {
    #
    #     mbsf_file <- SEERdb::get_mbsf_data(db_directory = {{ db_directory }},
    #                                        abcd.summary_files = mbsf_files[i]
    #     )
    #
    #     RSQLite::dbWriteTable(database,
    #                           name = paste0(names(mbsf_file$abcd.summary)[1]),
    #                           value = as.data.frame(mbsf_file$abcd.summary[[1]])
    #     )
    #
    #   }
    #
    #   if (grepl("f.cc.summary", mbsf_files[i])) {
    #
    #     mbsf_file <- SEERdb::get_mbsf_data(db_directory = {{ db_directory }},
    #                                        cc.summary_files = mbsf_files[i]
    #     )
    #
    #     RSQLite::dbWriteTable(database,
    #                           name = paste0(names(mbsf_file$cc.summary)[1]),
    #                           value = as.data.frame(mbsf_file$cc.summary[[1]])
    #     )
    #
    #   }
    #
    #   if (grepl("oth.cc.summary", mbsf_files[i])) {
    #
    #     mbsf_file <- SEERdb::get_mbsf_data(db_directory = {{ db_directory }},
    #                                        oth.cc.summary_files = mbsf_files[i]
    #     )
    #
    #     RSQLite::dbWriteTable(database,
    #                           name = paste0(names(mbsf_file$oth.cc.summary)[1]),
    #                           value = as.data.frame(mbsf_file$oth.cc.summary[[1]])
    #     )
    #
    #   }
    #
    #   rm(mbsf_file)
    #
    #   file_i <- file_i + 1
    #
    #   # Print the loading bar
    #   cat("\r", file_i,
    #       "/",
    #       num_files,
    #       " Files Processed",
    #       sep = ""
    #   )
    #
    #   flush.console()
    #
    # }
    #
    # # medpar FILES
    # for (i in 1:length(medpar_files)) {
    #
    #   cat(crayon::yellow$bold(paste0(" | Working on: ", medpar_files[i])))
    #
    #   medpar_file <- SEERdb::get_hospital_data(db_directory = {{ db_directory }},
    #                                              hospital_files = medpar_files[i]
    #   )
    #
    #   RSQLite::dbWriteTable(database,
    #                         name = paste0(names(medpar_file)[1]),
    #                         value = as.data.frame(medpar_file[[1]])
    #   )
    #
    #   rm(medpar_file)
    #
    #   file_i <- file_i + 1
    #
    #   # Print the loading bar
    #   cat("\r", file_i,
    #       "/",
    #       num_files,
    #       " Files Processed",
    #       sep = ""
    #   )
    #
    #   flush.console()
    #
    # }

    # # nch FILES
    # for (i in 1:length(nch_files)) {
    #
    #   cat(crayon::yellow$bold(paste0(" | Working on: ", nch_files[i])))
    #
    #   if (grepl("base", nch_files[i])) {
    #
    #     nch_file <- SEERdb::get_nch_data(db_directory = {{ db_directory }},
    #                                      nch_base_files = nch_files[i]
    #     )
    #
    #
    #     RSQLite::dbWriteTable(database,
    #                           name = paste0("nch.base.", names(nch_file$base)[1]),
    #                           value = as.data.frame(nch_file$base[[1]])
    #     )
    #
    #   }
    #
    #   if (grepl("demo", nch_files[i])) {
    #
    #     nch_file <- SEERdb::get_nch_data(db_directory = {{ db_directory }},
    #                                      nch_demo_files = nch_files[i]
    #     )
    #
    #     RSQLite::dbWriteTable(database,
    #                           name = paste0("nch.demo.", names(nch_file$demo)[1]),
    #                           value = as.data.frame(nch_file$demo[[1]])
    #     )
    #
    #   }
    #
    #   if (grepl("line", nch_files[i])){
    #
    #     nch_file <- SEERdb::get_nch_data(db_directory = {{ db_directory }},
    #                                      nch_line_files = nch_files[i]
    #     )
    #
    #     RSQLite::dbWriteTable(database,
    #                           name = paste0("nch.line.", names(nch_file$line)[1]),
    #                           value = as.data.frame(nch_file$line[[1]])
    #     )
    #
    #   }
    #
    #   rm(nch_file)
    #
    #   file_i <- file_i + 1
    #
    #   # Print the loading bar
    #   cat("\r", file_i,
    #       "/",
    #       num_files,
    #       " Files Processed",
    #       sep = ""
    #   )
    #
    #   flush.console()
    #
    # }

    # outpat FILES
    for (i in 1:length(outpat_files)) {


      cat(crayon::yellow$bold(paste0(" | Working on: ", outpat_files[i])))


      if (grepl("base", outpat_files[i])) {

        outpat_file <- SEERdb::get_outpat_data(db_directory = {{ db_directory }},
                                         outpat_base_files = outpat_files[i]
        )


        RSQLite::dbWriteTable(database,
                              name = paste0("outpat.base.", names(outpat_file$base)[1]),
                              value = as.data.frame(outpat_file$base[[1]])
        )

      }

      if (grepl("demo", outpat_files[i])) {

        outpat_file <- SEERdb::get_outpat_data(db_directory = {{ db_directory }},
                                         outpat_demo_files = outpat_files[i]
        )

        RSQLite::dbWriteTable(database,
                              name = paste0("outpat.demo.", names(outpat_file$demo)[1]),
                              value = as.data.frame(outpat_file$demo[[1]])
        )

      }

      if (grepl("condition", outpat_files[i])){

        outpat_file <- SEERdb::get_outpat_data(db_directory = {{ db_directory }},
                                         outpat_condition_files = outpat_files[i]
        )

        RSQLite::dbWriteTable(database,
                              name = paste0("outpat.condition.", names(outpat_file$condition)[1]),
                              value = as.data.frame(outpat_file$condition[[1]])
        )

      }

      if (grepl("occurrence", outpat_files[i])){

        outpat_file <- SEERdb::get_outpat_data(db_directory = {{ db_directory }},
                                            outpat_occurrence_files = outpat_files[i]
        )

        RSQLite::dbWriteTable(database,
                              name = paste0("outpat.occurrence.", names(outpat_file$ocurrence)[1]),
                              value = as.data.frame(outpat_file$ocurrence[[1]])
        )

      }

      if (grepl("revenue", outpat_files[i])){

        outpat_file <- SEERdb::get_outpat_data(db_directory = {{ db_directory }},
                                         outpat_revenue_files = outpat_files[i]
        )

        RSQLite::dbWriteTable(database,
                              name = paste0("outpat.revenue.", names(outpat_file$revenue)[1]),
                              value = as.data.frame(outpat_file$revenue[[1]])
        )

      }


      if (grepl("span", outpat_files[i])){

        outpat_file <- SEERdb::get_outpat_data(db_directory = {{ db_directory }},
                                         outpat_span_files = outpat_files[i]
        )

        RSQLite::dbWriteTable(database,
                              name = paste0("outpat.span.", names(outpat_file$span)[1]),
                              value = as.data.frame(outpat_file$span[[1]])
        )

      }


      if (grepl("value", outpat_files[i])){

        outpat_file <- SEERdb::get_outpat_data(db_directory = {{ db_directory }},
                                         outpat_value_files = outpat_files[i]
        )

        RSQLite::dbWriteTable(database,
                              name = paste0("outpat.value.", names(outpat_file$value)[1]),
                              value = as.data.frame(outpat_file$value[[1]])
        )

      }

      rm(outpat_file)

      file_i <- file_i + 1

      # Print the loading bar
      cat("\r", file_i,
          "/",
          num_files,
          " Files Processed",
          sep = ""
      )

      flush.console()

    }

  }

}
