#' BSU Teaclub
#' @docType package
#' @name teaclub
NULL

#' @title Load xth most recent CSV file
#'
#' @param x An integer, specifying which most recent file to get. 1 means most
#' recent, 2 second most recent etc
#' @param directory The directory ("owe", "used", "sheet", "paid", "people")
#' @return The contents of the csv
#' @author R.J.B. Goudie
load_x_most_recent <- function(x, directory){
  file_path <- path_x_most_recent(x = x, directory = directory)
  read.csv(file_path, stringsAsFactor = F)
}

#' @title Load directory new people only
#' @param directory A character,  either "used" or "paid"
#' @return A data.frame
#' @author R.J.B. Goudie
load_directory_new_people_only <- function(directory){
  accounts <- load_x_most_recent(1, directory)

  # Don't use the new people file to sort, since there may have been changes
  old_people <- load_people_prior()
  new_people <- load_people_today()

  new_people_rows <- !new_people$unique_id %in% old_people$unique_id
  new_people_only <- new_people[new_people_rows, ]
  new_unique_ids <- new_people_only$unique_id
  new_accounts_rows <- accounts$unique_id %in% new_unique_ids

  new_accounts_only <- accounts[new_accounts_rows, ]

  combine_accounts_people(new_accounts_only, new_people_only)
}

#' @title Load directory not new people
#' @param directory A character, either "used" or "paid"
#' @return A data.frame
#' @author R.J.B. Goudie
load_directory_not_new_people <- function(directory){
  accounts <- load_x_most_recent(1, directory)

  # Don't use the new people file to sort, since there may have been changes
  old_people <- load_people_prior()
  new_people <- load_people_today()

  new_people_rows <- !new_people$unique_id %in% old_people$unique_id
  new_people_only <- new_people[new_people_rows, ]
  new_unique_ids <- new_people_only$unique_id
  new_accounts_rows <- accounts$unique_id %in% new_unique_ids

  new_accounts_only <- accounts[new_accounts_rows, ]
  old_accounts_only <- accounts[!new_accounts_rows, ]

  combine_accounts_people(old_accounts_only, old_people)
}
