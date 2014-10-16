#' @title Display the most recent used/paid file
#' @param directory A character,  either "used" or "paid"
#' @return Prints the contents to the console
#' @author R.J.B. Goudie
#' @export
display_check <- function(directory){
  new <- load_directory_new_people_only(directory)
  existing <- load_directory_penultimate_people(directory)

  existing <- subset(existing,
                     select = c("unique_id", "display_name", "amount"))
  existing <- sort_by_display_name(filter_zeros(existing))

  new <- subset(new, select = c("unique_id", "display_name", "amount"))
  new <- sort_by_display_name(filter_zeros(new))

  existing_print <- print_data_frame(existing)
  new_print <- print_data_frame(new)

  message("NEW PEOPLE:")
  cat(new_print)
  message("EXISTING PEOPLE")
  cat(existing_print)
}

#' @title Display summary of used and paid over the last month
#' @return A summary of used and paid over the last month
#' @author R.J.B. Goudie
#' @export
display_summary <- function(){
  last_used <- load_x_most_recent(1, "used")
  last_paid <- load_x_most_recent(1, "paid")
  used_last_month <- sum(last_used$amount)/100
  paid_last_month <- sum(last_paid$amount)/100
  message("Summary:")
  message("Used last month: £", used_last_month)
  message("Paid last month: £", paid_last_month)
  message("")
}

#' @title Display balances
#' @return A data.frame of balances
#' @author R.J.B. Goudie
#' @export
display_balances <- function(){
  # Note that people NOT on the most recent people file are dropped!
  balance_df <- account_balances()

  out <- subset(balance_df, select = c("unique_id", "display_name", "balance"))
  sort_by_display_name(out)
}
