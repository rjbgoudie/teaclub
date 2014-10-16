#' @title Publish the current balances
#'
#' @description
#' Display a summary of the teaclub finances, compute the amount everyone
#' owes, email everyone, and make a pdf with people's balances
#'
#' @return NULL
#' @author R.J.B. Goudie
#' @param owe_function A function that adds a message column to balance_df for
#' people who owe money to the teaclub
#' @param credit_function A function that adds a message column to balance_df
#' for people who are in credit
#' @param dry_run A logical. If true, emails are NOT sent, instead a data.frame
#' including the messages that will be sent is returned.
#' @export
publish_balances <- function(owe_function, credit_function, dry_run = T){
  display_summary()

  # Note that people NOT on the most recent people file are dropped!
  balance_df <- account_balances()
  publish_owe_sheet(balance_df)

  # export_email_tab(balance_df)
  email_df <- draft_all_emails(balance_df, owe_function, credit_function)
  if (dry_run){
    email_df
  } else {
    send_all_emails(email_df)
  }
}

#' @title Send emails with balances
#'
#' @description
#' People who have balance zero are NOT emailed, to avoid bothering people who
#' do not use the teaclub.
#'
#' People who have no email address are excluded.
#'
#' @param balance_df A data.frame containing balances
#' @param owe_function A function that adds a message column to balance_df for
#' people who owe money to the teaclub
#' @param credit_function A function that adds a message column to balance_df
#' for people who are in credit
#' @return NULL
#' @author R.J.B. Goudie
draft_all_emails <- function(balance_df, owe_function, credit_function){
  email_df <- filter_people_lacking_email(balance_df)
  email_df <- email_df[, c("display_name", "email", "balance")]

  total_in_league <- nrow(balance_df)

  email_df_owing <- subset(email_df, balance > 0)
  email_df_owing$position <- rank(-email_df_owing$balance, ties.method = "first")
  email_df_owing <- owe_function(email_df_owing)

  email_df_incredit <- subset(email_df, balance < 0)
  email_df_incredit$position <- rank(-email_df_incredit$balance, ties.method = "first")
  email_df_incredit <- credit_function(email_df_incredit)

  rbind(email_df_owing, email_df_incredit)
}

send_all_emails <- function(x){
  plyr::a_ply(x, 1, email_row)
  message("Messages sent")
}

email_row <- function(row){
  message("Emailing ", row$display_name, " (", row$email, ")")

  email_subject <- "[tea-club] Statement"
  # Pipe the message into mailx
  # echo message | mailx -s 'Subject' email@address.com
  call <- paste0("echo ",
                 shQuote(row$message),
                 " | mailx -s '",
                 email_subject,
                 "' ",
                 row$email)
  system(call)
}

#' @title Filter out rows with blank email address
#' @param x A data.frame
#' @return The data.frame x, but with rows with email == "" removed
#' @author R.J.B. Goudie
filter_people_lacking_email <- function(x){
  has_email_address <- x$email != ""
  no_email_display_names <- x[!has_email_address, "display_name"]
  no_email_string <- paste(no_email_display_names, collapse = " ")
  message("Excluding people with no email address: ", no_email_string)
  x[has_email_address, ]
}

#' @title Make PDF of what people owe
#' @param balance_df A data.frame
#' @return NULL
#' @author R.J.B. Goudie
#' @export
publish_owe_sheet <- function(balance_df){
  owe_df <- subset(balance_df, select = c("display_name", "balance"))
  owe_df$paid <- "      "
  row_half_way <- floor(nrow(owe_df)/2)

  top_owe_df <- owe_df[seq_len(row_half_way),]
  bottom_owe_df <- owe_df[seq_len(row_half_way) + row_half_way, ]

  owe_df <- cbind(top_owe_df, bottom_owe_df)

  latex_table <- as.latex.data.frame(owe_df,
                                     tabular.environment= "longtable",
                                     floating = F)
  latex_top <- paste("\\documentclass[a4paper]{article}
\\usepackage[top=0.25in, bottom=0.25in, left=0.25in, right=0.25in]{geometry}
\\usepackage{longtable}
\\renewcommand{\\topfraction}{0.999}	% max fraction of floats at top
\\renewcommand{\\bottomfraction}{0.999}
\\renewcommand{\\textfraction}{0}
\\begin{document}
\\begin{center}
\\textbf{BSU Tea Club balances at ",
today_date_file_string(),
"}\\end{center}
\\linespread{1.5}")
  latex_bottom <- "\\end{document}"

  owe_file_path <- path_today("owe", extension = "tex")
  cat(latex_top,
      latex_table,
      latex_bottom,
      file = owe_file_path,
      sep = "\n")

  system(paste("pdflatex", "-output-directory", "owe", owe_file_path))
}
