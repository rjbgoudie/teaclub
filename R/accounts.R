#' @title Load accounts (either used or paid)
#'
#' @description
#' Note that people NOT on the most recent people file are dropped.
#'
#' @param directory A character string, either "used" or "paid"
#' @return A data.frame with used or paid for all current people
#' @author R.J.B. Goudie
load_accounts <- function(directory){
  files <- dir(directory)
  files_paths <- file.path(directory, files)
  files_dates <- sub("\\.[^.]*$", "", files)
  names(files_paths) <- files_dates
  accounts <- plyr::ldply(files_paths, read.csv, stringsAsFactor = F)
  colnames(accounts)[colnames(accounts) == ".id"] <- "date"

  people <- load_people_today()

  # Note that people NOT on the most recent people file are dropped!
  combine_accounts_people(accounts, people, all.accounts = F)
}

#' @title How much have people used the teaclub recently?
#' @return A data.frame of the amount spent in the last 3 months,
#' by person
#' @author R.J.B. Goudie
accounts_recent_used <- function(){
  # load accounts of the amount used
  people <- load_people_today()

  # Don't actually want most recent, since already made this month file
  used2 <- load_x_most_recent(2, "used")
  used3 <- load_x_most_recent(3, "used")
  used4 <- load_x_most_recent(4, "used")
  recent_used <- rbind(used2, used3, used4)

  combine_accounts_people(recent_used,
                          people,
                          all.accounts = F,
                          all.people = T)
}

#' @title Combine account and people data.frames
#' @param accounts An accounts data.frame
#' @param people A people data.frame
#' @param sort A logical, should the data.frame be sorted (by display name)
#' @param all.accounts Should all accounts be included in the merge?
#' @param all.people Should all people be included in the merge?
#' @return A data.frame
#' @author R.J.B. Goudie
combine_accounts_people <- function(accounts,
                                    people,
                                    sort = F,
                                    all.accounts = T,
                                    all.people = T){
  df <- merge(accounts, people, all.x = all.accounts, all.y = all.people)
  if (sort){
    sort_by_display_name(df)
  } else {
    df
  }
}

#' @title Compute balances
#'
#' @description
#' Note that people NOT on the most recent people file are dropped!
#'
#' @return A data.frame of balances
#' @author R.J.B. Goudie
compute_balances <- function(){
  # load accounts of the amount used
  used_df <- load_accounts("used")
  used_df <- make_negative(used_df)

  # load accounts of the amount paid
  paid_df <- load_accounts("paid")

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
