#' @title Load all 'paid' data
#'
#' @description
#' Loads all data in paid/*.csv and joins them together into one data.frame
#'
#' NOTE people NOT on latest people file are DROPPED.
#'
#' @return A data.frame with used or paid for all current people. The result is
#' a all of the csv files in the directory joined together as a data.frame,
#' combined with extra information from the current people file.
#'
#' The columns of the data.frame are "unique_id", "date", "amount",
#' "display_name", "email", "handwriting_factor"
#'
#' NOTE people NOT on the latest people file are DROPPED.
#' @author R.J.B. Goudie
load_all_paid <- function(){
  load_accounts("paid")
}

#' @title Load all 'used' data
#'
#' @description
#' Loads all data in used/*.csv and joins them together into one data.frame
#'
#' NOTE people NOT on latest people file are DROPPED.
#'
#' @return A data.frame with used or paid for all current people. The result is
#' a all of the csv files in the directory joined together as a data.frame,
#' combined with extra information from the current people file.
#'
#' The columns of the data.frame are "unique_id", "date", "amount",
#' "display_name", "email", "handwriting_factor"
#'
#' NOTE people NOT on the latest people file are DROPPED.
#' @author R.J.B. Goudie
load_all_used <- function(){
  load_accounts("used")
}

#' @title Load accounts (either used or paid)
#'
#' @description
#' Loads all data in the directory and joins them together into one data.frame
#'
#' NOTE people NOT on latest people file are DROPPED.
#'
#' @param directory A character string, either "used" or "paid"
#' @return A data.frame with used or paid for all current people. The result is
#' a all of the csv files in the directory joined together as a data.frame,
#' combined with extra information from the current people file.
#'
#' The columns of the data.frame are "unique_id", "date", "amount",
#' "display_name", "email", "handwriting_factor"
#'
#' NOTE people NOT on the latest people file are DROPPED.
#' @author R.J.B. Goudie
load_accounts <- function(directory){
  files <- dir(directory)
  files_paths <- file.path(directory, files)
  files_dates <- sub("\\.[^.]*$", "", files)
  names(files_paths) <- files_dates
  accounts <- plyr::ldply(files_paths, read.csv, stringsAsFactor = F)
  colnames(accounts)[colnames(accounts) == ".id"] <- "date"

  people_latest <- load_people_latest()

  # Note that people NOT on the most recent people file are dropped!
  # ie don't include accounts for people not in the people data.frame
  merge_accounts_people_dropping_leavers(accounts, people_latest)
}

#' @title How much have people used the teaclub recently?
#'
#' @description
#' Load the last 3 months of "used" data, and total up how much everyone spent.
#' ONLY people in the current people file are included, because this is used
#' for the new sheet for the tearoom.
#'
#' @return A data.frame of the amount spent in the last 3 months. Each person's
#' spending for each of the last three months is a row.
#' The columns of the data.frame are "unique_id", "amount",
#' "display_name", "email", "handwriting_factor". "amount" is the amount spent.
#' @author R.J.B. Goudie
accounts_recent_used <- function(){
  # load accounts of the amount used
  people_latest <- load_people_latest()

  # Don't actually want most recent, since already made this month file
  used2 <- load_directory_x_most_recent(2, "used")
  used3 <- load_directory_x_most_recent(3, "used")
  used4 <- load_directory_x_most_recent(4, "used")
  recent_used <- rbind(used2, used3, used4)

  merge_accounts_people_dropping_leavers(recent_used, people_latest)
}

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
merge_accounts_people_dropping_leavers <- function(accounts, people){
  merge(accounts, people, all.x = F, all.y = T)
}

#' @title Compute balances
#'
#' @description
#' Add up the total amount used (over all months), subtract the total amount
#' (over all months) to get each person's balance
#'
#' Note that people NOT on the most recent people file are dropped!
#'
#' Negative numbers indicate the person owes the teaclub money. Positive
#' numbers indicate the person is in credit.
#'
#' Amounts are returned in POUNDS
#'
#' @return A data.frame of balances. Each row corresponds to a person.
#' Columns are "display_name", "unique_id", "email", "balance"
#'
#' NOTE the balance is in POUNDS
#' @author R.J.B. Goudie
account_balances <- function(){
  # load accounts of the amount used
  used_df <- load_all_used()
  used_df <- make_negative(used_df)

  # load accounts of the amount paid
  paid_df <- load_all_paid()

  # combine the data frames
  all_df <- rbind(used_df, paid_df)

  # IMPORTANT
  # Note that the sign is flipped here
  balance_df <- plyr::ddply(all_df,
                            c("display_name", "unique_id", "email"),
                            plyr::summarise,
                            balance = -sum(amount)/100)
  balance_df
}
