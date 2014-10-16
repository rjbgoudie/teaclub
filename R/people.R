#' @title Load most recent 'people' data frame
#' @return A data.frame, with each person on a separate row. Rows are sorted by
#' display_name. Columns are
#' "unique_id", "display_name", "email" and "handwriting_factor"
#'
#' "unique_id" - MUST be unique across all time,  since balances match on this
#' "display_name" - What is shown on the tearoom sheet,  and in emails
#' "email" - An email address. Part after @ sign (for internal addresses) can be omitted
#' "handwriting_factor" - fudge the number of cells in tearoom sheet
#'
#' @author R.J.B. Goudie
load_people_latest <- function(){
  load_people_x_most_recent(1)
}

#' @title Load second most recent 'people' data frame
#' @return A data.frame, with each person on a separate row. Rows are sorted by
#' display_name. Columns are
#' "unique_id", "display_name", "email" and "handwriting_factor"
#'
#' "unique_id" - MUST be unique across all time,  since balances match on this
#' "display_name" - What is shown on the tearoom sheet,  and in emails
#' "email" - An email address. Part after @ sign (for internal addresses) can be omitted
#' "handwriting_factor" - fudge the number of cells in tearoom sheet
#' @author R.J.B. Goudie
load_people_penultimate <- function(){
  load_people_x_most_recent(2)
}

#' @title Load xth most recent 'people' data frame
#' @param x An integer, specifying which most recent file to get. 1 means most
#' recent, 2 second most recent etc
#' @return A data.frame, with each person on a separate row.
#' Rows are sorted by display_name. Columns are
#' "unique_id", "display_name", "email" and "handwriting_factor"
#'
#' "unique_id" - MUST be unique across all time,  since balances match on this
#' "display_name" - What is shown on the tearoom sheet,  and in emails
#' "email" - An email address. Part after @ sign (for internal addresses) can be omitted
#' "handwriting_factor" - fudge the number of cells in tearoom sheet
#' @author R.J.B. Goudie
load_people_x_most_recent <- function(x){
  people <- load_directory_x_most_recent(x, "people")
  sort_by_display_name(people)
}

#' @title Load union of new people and existing people
#'
#' @description
#' The people file changes from month to month. This gives all the
#' people from last month, plus the new people. The new people are added to the
#' top of the data.frame. Within each group (ie new and old people), the
#' rows are sorted by display_name.
#'
#' @return A data.frame, with people corresponding to rows.
#' Columns are "unique_id", "display_name", "email" and "handwriting_factor"
#'
#' "unique_id" - MUST be unique across all time,  since balances match on this
#' "display_name" - What is shown on the tearoom sheet,  and in emails
#' "email" - An email address. Part after @ sign (for internal addresses) can be omitted
#' "handwriting_factor" - fudge the number of cells in tearoom sheet
#'
#' @author R.J.B. Goudie
load_people_new_union_penultimate <- function(){
  people_penultimate <- load_people_penultimate()
  people_new <- load_people_latest_less_penultimate()
  rbind(people_new, people_penultimate)
}

#' @title Load setdiff of most recent and second most recent 'people'
#'
#' @description
#' The people file changes from month to month. This gives all the
#' people from this month, minus the people from last month. ie just
#' newbies
#'
#' \link{load_people_penultimate_less_latest} gives the opposite
#'
#' @return A data.frame, with people corresponding to rows, sorted by
#' display_name.
#' Columns are "unique_id", "display_name", "email" and "handwriting_factor"
#'
#' "unique_id" - MUST be unique across all time,  since balances match on this
#' "display_name" - What is shown on the tearoom sheet,  and in emails
#' "email" - An email address. Part after @ sign (for internal addresses) can be omitted
#' "handwriting_factor" - fudge the number of cells in tearoom sheet
#'
#' @author R.J.B. Goudie
load_people_latest_less_penultimate <- function(){
  people_penultimate <- load_people_penultimate()
  people_latest <- load_people_latest()

  new_people_rows <- !people_latest$unique_id %in% people_penultimate$unique_id
  people_latest[new_people_rows, ]
}
