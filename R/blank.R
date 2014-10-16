#' @title Make new people file
#'
#' @description
#' Copies last month's people file to "people/TODAYDATE.csv",
#' ready to be updated for this month
#'
#' @return NULL
#' @author R.J.B. Goudie
#' @export
blank_people <- function(){
  # path_most_recent is correct because last months is still
  # the most recent until this copying is done...
  ok <- file.copy(path_most_recent("people"),
                  path_today("people"),
                  overwrite = F)
  if (!isTRUE(ok)){
    message("Export not successful, does the file exist already?")
  }
}

#' @title Make new paid file
#'
#' @description
#' Makes blank paid files at "paid/TODAYDATE.csv", ready to be updated
#' with the amount people. This file will include everyone in last month's
#' people file (IN ORDER), and then people added to this month's people file
#' AT THE BOTTOM.
#'
#' @author R.J.B. Goudie
#' @export
blank_paid <- function(){
  # New people have sometimes paid before the bills are set out
  # due to misunderstanding the system, so include them at the
  # bottom of the paid csv file
  people <- load_people_prior_union_today()
  people$amount <- 0
  people <- subset(people, select = c("unique_id", "amount"))

  write.csv(people,
            file = path_today("paid"),
            row.names = F)
}

#' @title Make new used file
#'
#' @description
#' Makes blank used files at "used/TODAYDATE.csv", ready to be updated
#' with the amount people have spent in the last month. This file will include
#' everyone in last month's people file (IN ORDER), and then people added to
#' this month's people file AT THE BOTTOM.
#'
#' @author R.J.B. Goudie
#' @export
blank_used <- function(){
  people <- load_people_prior_union_today()
  people$amount <- 0
  people <- subset(people, select = c("unique_id", "amount"))

  message("Remember to add on milk money to sharon's used")
  message("Remember to add toby's used to teresa's used")
  write.csv(people,
            file = path_today("used"),
            row.names = F)
}

#' @title Make new blank sheet
#'
#' @description
#' Makes blank sheet at "sheet/TODAYDATE.pdf", ready to be printed.
#'
#' This will ONLY include people in the TODAY's people file. It will be sorted
#' by display_name
#'
#' @author R.J.B. Goudie
#' @export
blank_sheet <- function(){
  recent_used <- accounts_recent_used()
  recent_used[is.na(recent_used$amount), "amount"] <- 0
  recent_used <- sheet_cell_sizes(recent_used)
  recent_used <- sort_by_display_name(recent_used)

  sheet_df <- sheet_df(recent_used)
  sheet_df_to_latex(sheet_df)

  sheet_file <- paste0(today_date_file_string(), ".tex")
  sheet_file_path <- file.path("sheet", sheet_file)

  system(paste("pdflatex", "-output-directory", "sheet", sheet_file_path))
}
