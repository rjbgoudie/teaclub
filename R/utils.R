#' @title Combine account and people data.frames
#'
#' @description
#' Just a wrapper around merge,
#'
#' @param accounts An accounts data.frame
#' @param people A people data.frame
#' @return A merged data.frame. People who are not on the supplied people list
#' are DROPPED.
#' @author R.J.B. Goudie
merge_for_these_people_only <- function(accounts, people){
  merge(accounts, people, all.x = F, all.y = T)
}

#' @title Flip sign of 'amount' column in data.frame
#' @param df A data.frame, with an 'amount' column
#' @return The data.frame df,  with sign of amount column flipped
#' @author R.J.B. Goudie
make_negative <- function(df){
  df$amount <- -df$amount
  df
}

#' @title Extract rows with non-zero 'amount'
#' @param df A data.frame, with an 'amount' column
#' @return The rows of df with non-zero amount
#' @author R.J.B. Goudie
filter_zeros <- function(df){
  df[df$amount != 0, ]
}

#' @title Move row with specified unique_id to end
#'
#' @description
#' The row of the supplied data.frame with df$unique_id == unique_id
#' is moved to the end of the data.frame in the returned data.frame
#'
#' @param df A data.frame with a 'unique_id' column
#' @param unique_id A character. This is matched to the unique_id of each
#' row,  to identify which to move to the end
#' @return The data.frame df, with relevant row moved to the end
#' @author R.J.B. Goudie
move_row_to_end <- function(df, unique_id){
  row_indicator <- df$unique_id == unique_id
  row <- df[row_indicator, ]
  df <- df[!row_indicator, ]
  rbind(df, row)
}

#' @title Sort a data.frame by 'display_name' row,  except 'zzz-visitors'
#'
#' @description
#' The returned data.frame is sorted by 'display_name', but the
#' 'zzz-visitors' row is moved to the end
#'
#' @param df A data.frame
#' @return The sorted data.frame
#' @author R.J.B. Goudie
sort_by_display_name <- function(df){
  out <- df[order(df$display_name), ]
  move_row_to_end(out, "zzz-visitors")
}

#' @title Get today's date, formatted YYYY-MM-DD
#' @return A character string, with today's date formatted YYYY-MM-DD
#' @author R.J.B. Goudie
today_date_file_string <- function(){
  format(Sys.time(), "%Y-%m-%d")
}

#' @title Capture printing of a data.frame to a string
#' @param x A data.frame
#' @return A character string, representing the result of print(x)
#' @author R.J.B. Goudie
print_data_frame <- function(x){
  x <- capture.output(x)
  paste0(x, "\n")
}
