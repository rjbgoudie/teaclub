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
#' @export
publish_balances <- function(owe_function, credit_function){
  display_summary()

  # Note that people NOT on the most recent people file are dropped!
  balance_df <- compute_balances()

  # export_email_tab(balance_df)
  send_emails(balance_df, owe_function, credit_function)
  publish_owe_pdf(balance_df)
}

# toby_teresa <- function(df){
#   toby_row <- df$unique_id == "toby"
#   message("adding toby's amount to ")
# }

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
send_emails <- function(balance_df, owe_function, credit_function){
  no_email <- balance_df$email == ""
  no_email_string <- paste(balance_df[no_email, "display_name"], collapse = " ")
  message("Excluding people with no email address: ", no_email_string)
  email_df <- subset(balance_df, email != "")
  email_df <- email_df[, c("display_name", "email", "balance")]

  total_in_league <- nrow(balance_df)

  email_df_owing <- subset(email_df, balance > 0)
  email_df_owing$position <- rank(-email_df_owing$balance, ties.method = "first")
  email_df_owing <- owe_function(email_df_owing)

  email_df_incredit <- subset(email_df, balance < 0)
  email_df_incredit$position <- rank(-email_df_incredit$balance, ties.method = "first")
  email_df_incredit <- credit_function(email_df_incredit)

  email_df <- rbind(email_df_owing, email_df_incredit)

  plyr::a_ply(email_df, 1, function(row){
    message("Emailing ", row$display_name, " (", row$email, ")")
    call <- paste0("echo ",
                   shQuote(row$message),
                   " | mailx -s '[tea-club] Statement' ",
                   row$email)
    system(call)
  })
  message("Messages sent")
}

#' @title Make PDF of what people owe
#' @param balance_df A data.frame
#' @return NULL
#' @author R.J.B. Goudie
#' @export
publish_owe_pdf <- function(balance_df){
  owe_df <- subset(balance_df, select = c("display_name", "balance"))
  owe_df$paid <- "      "
  row_half_way <- floor(nrow(owe_df)/2)

  top_owe_df <- owe_df[seq_len(row_half_way),]
  bottom_owe_df <- owe_df[seq_len(row_half_way) + row_half_way, ]

  owe_df <- cbind(top_owe_df, bottom_owe_df)

  latex_table <- data.frame.as.latex(owe_df, tabular.environment= "longtable")
  latex_top <- paste("\\documentclass[a4paper]{article}\n\\usepackage[top=0.25in, bottom=0.25in, left=0.25in, right=0.25in]{geometry}\n\\usepackage{longtable}\n\\renewcommand{\\topfraction}{0.999}	% max fraction of floats at top\n\\renewcommand{\\bottomfraction}{0.999}\n\\renewcommand{\\textfraction}{0}\n\\begin{document}\n  \n\\begin{center}\n\\textbf{BSU Tea Club balances at ", today_date_file_string(), "}\\end{center}\n\\linespread{1.5}")
  latex_bottom <- "\\end{document}"

  owe_file <- paste0(today_date_file_string(), ".tex")
  owe_file_path <- file.path("owe", owe_file)
  cat(latex_top,
      latex_table,
      latex_bottom,
      file = owe_file_path,
      sep = "\n")

  system(paste("pdflatex", "-output-directory", "owe", owe_file_path))
}
