#' @title Load latest CSV file
#'
#' @param directory The directory ("owe", "used", "sheet", "paid", "people")
#' @return The contents of the csv as a data.frame
#' @author R.J.B. Goudie
load_directory_latest <- function(directory){
  load_directory_x_most_recent(1, directory)
}

#' @title Load penultimate CSV file
#'
#' @param directory The directory ("owe", "used", "sheet", "paid", "people")
#' @return The contents of the csv as a data.frame
#' @author R.J.B. Goudie
load_directory_penultimate <- function(directory){
  load_directory_x_most_recent(2, directory)
}

#' @title Load xth most recent CSV file
#'
#' @param x An integer, specifying which most recent file to get. 1 means most
#' recent, 2 second most recent etc
#' @param directory The directory ("owe", "used", "sheet", "paid", "people")
#' @return The contents of the csv as a data.frame
#' @author R.J.B. Goudie
load_directory_x_most_recent <- function(x, directory){
  file_path <- path_x_most_recent(x = x, directory = directory)
  read.csv(file_path, stringsAsFactor = F)
}
