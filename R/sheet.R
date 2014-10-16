#' @title Calculate cell sizes
#' @param df A data.frame
#' @return A data.frame
#' @author R.J.B. Goudie
sheet_cell_sizes <- function(df){
  df <- plyr::ddply(df,
                    c("display_name", "unique_id"),
                    plyr::summarise,
                    size = max(min(sum(amount), 1000), 350) +
                      100 * mean(handwriting_factor))
  total <- sum(df$size)
  transform(df, size = floor(size/total*(22*40-60)))
}

#' @title Data frame for sheet
#' @param df A data.frame
#' @return A data.frame
#' @author R.J.B. Goudie
sheet_df <- function(df){
  # each entry in the following string vector represents a box on the grid
  str <- c()

  for (i in seq_along(df$display_name)){
    # if any of the next 8 cells is a new column, then jump forward so that
    # we get a name at the top of the column
    l <- length(str)
    range <- l:(l + 8)
    div <- range %% 40 == 0
    if (any(div)){
      str <- c(str, rep("", length = which(div) - 1))
    }
    new_cells <- rep("", length = df$size[i])
    str <- c(str, df$display_name[i], new_cells)
  }

  extra <- max(0, 960 - length(str))
  str <- c(str, rep("", length = extra))

  m <- matrix(str, nrow = 40)
  as.data.frame(m)
}

#' @title Convert a data.frame to latex
#' @param df A data.frame
#' @param tabular.environment A tabular.environment from xtable
#' @return A character string of latex code
#' @author R.J.B. Goudie
as.latex.data.frame <- function(df,
                                tabular.environment = "tabular",
                                floating = T){
  r <- rep("p{2cm}|", length = ncol(df) + 1)
  r[2] <- paste("|", r[1], collapse = "")
  df_xtable <- xtable::xtable(df, align = r)

  capture.output(xtable::print.xtable(
    df_xtable,
    include.colnames = F,
    floating = floating,
    include.rownames = F,
    tabular.environment = tabular.environment,
    hline.after = 0:min(nrow(df_xtable), 40)))
}

#' @title Convert sheet_df to latex
#' @param sheet_df A data.frame
#' @return NULL
#' @author R.J.B. Goudie
sheet_df_to_latex <- function(sheet_df){
  sheet_df_list <- list(sheet_df[, 1:8], sheet_df[, 9:16], sheet_df[, 17:24])
  sheet_latex_list <- lapply(sheet_df_list, as.latex.data.frame)

  latex_top <- "\\documentclass[a4paper]{article}
\\usepackage[top=0.25in, bottom=0.25in, left=0.25in, right=0.25in]{geometry}
\\renewcommand{\\topfraction}{0.999}% max fraction of floats at top
\\renewcommand{\\bottomfraction}{0.999}
\\renewcommand{\\textfraction}{0}
\\begin{document}
\\begin{center}
\\textbf{BSU TEA CLUB}

New staff/students and long-term visitors: please add your name in a free space.

Short-term visitors: please enter purchases in `visitors' section
\\end{center}
\\linespread{1.5}"
  latex_bottom <- "\\end{document}"

  sheet_file_path <- path_today("sheet", extension = "tex")
  cat(latex_top,
      sheet_latex_list[[1]],
      sheet_latex_list[[2]],
      sheet_latex_list[[3]],
      latex_bottom,
      file = sheet_file_path,
      sep = "\n")
}
