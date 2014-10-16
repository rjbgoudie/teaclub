#' @title Get path to csv for today
#' @param directory The directory ("owe", "used", "sheet", "paid", "people")
#' @param extension The extension to append to the path
#' @return A file path of the form e.g. "owe/2014-09-09.csv"
#' @author R.J.B. Goudie
path_today <- function(directory, extension = "csv"){
  base <- today_date_file_string()
  today_filename <- paste0(base, ".", extension)
  file.path(directory, today_filename)
}

#' @title Get path to csv for the most recent file
#' @param directory The directory ("owe", "used", "sheet", "paid", "people")
#' @param extension The extension to append to the path
#' @return A file path of the form e.g. "owe/2014-09-09.csv"
#' @author R.J.B. Goudie
path_most_recent <- function(directory, extension = "csv"){
  path_x_most_recent(1, directory, extension)
}

#' @title Get path to csv for second most recent file
#' @param directory The directory ("owe", "used", "sheet", "paid", "people")
#' @param extension The extension to append to the path
#' @return A file path of the form e.g. "owe/2014-09-09.csv"
#' @author R.J.B. Goudie
path_second_most_recent <- function(directory, extension = "csv"){
  path_x_most_recent(2, directory)
}

#' @title Get path to csv for xth most recent month
#' @param directory The directory ("owe", "used", "sheet", "paid", "people")
#' @param extension The extension to append to the path
#' @return A file path of the form e.g. "owe/2014-09-09.csv"
#' @author R.J.B. Goudie
path_x_most_recent <- function(x, directory, extension = "csv"){
  base <- find_x_most_recent_file(x, directory)
  file <- paste0(base, ".", extension)
  file.path(directory, file)
}

#' @title Get most recent file in a directory
#'
#' @description
#' This just judges 'recent' by the name of the file, not the 'last modified'
#' date etc
#'
#' @param directory The directory ("owe", "used", "sheet", "paid", "people")
#' @return A data string of the form "2014-09-09"
#' @author R.J.B. Goudie
find_most_recent_file <- function(directory){
  find_x_most_recent_file(1, directory)
}

#' @title Get second most recent file in a directory
#'
#' @description
#' This just judges 'recent' by the name of the file, not the 'last modified'
#' date etc
#'
#' @param directory The directory ("owe", "used", "sheet", "paid", "people")
#' @return A data string of the form "2014-09-09"
#' @author R.J.B. Goudie
find_second_most_recent_file <- function(directory){
  find_x_most_recent_file(2, directory)
}

#' @title Get xth most recent file in a directory
#'
#' @description
#' This just judges 'recent' by the name of the file, not the 'last modified'
#' date etc
#'
#' @param x An integer, specifying which most recent file to get. 1 means most
#' recent, 2 second most recent etc
#' @param directory The directory ("owe", "used", "sheet", "paid", "people")
#' @return A data string of the form "2014-09-09"
#' @author R.J.B. Goudie
find_x_most_recent_file <- function(x, directory){
  files <- dir(directory)
  files_dates <- as.Date(sub("\\.[^.]*$", "", files))
  files_dates_order <- order(files_dates, decreasing = T)
  most_recent_date <- files_dates[files_dates_order[x]]
  as.character(most_recent_date)
}
