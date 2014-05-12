#' @title Load most recent 'people' data frame
#' @param ... Passed to load_people_x_most_recent. Currently
#' includes sort logical
#' @return A data.frame, with people corresponding to rows
#' @author R.J.B. Goudie
load_people_today <- function(...){
  load_people_x_most_recent(1, ...)
}

#' @title Load second most recent 'people' data frame
#' @param ... Passed to load_people_x_most_recent. Currently
#' includes sort logical
#' @return A data.frame, with people corresponding to rows
#' @author R.J.B. Goudie
load_people_prior <- function(...){
  load_people_x_most_recent(2, ...)
}

#' @title Load xth most recent 'people' data frame
#' @param x An integer, specifying which most recent file to get. 1 means most
#' recent, 2 second most recent etc
#' @param sort A logical, should the people be sorted by display
#' name?
#' @return A data.frame, with people corresponding to rows
#' @author R.J.B. Goudie
load_people_x_most_recent <- function(x, sort = T){
  people <- load_x_most_recent(x, "people")

  if (sort){
    sort_by_display_name(people)
  } else {
    people
  }
}

#' @title Load union of second most recent and most recent 'people'
#'
#' @description
#' The people file changes from month to month. This gives all the
#' people from last month, plus the new people.
#'
#' If sort = F, new people appear at the bottom.
#' \link{load_people_today_union_prior} gives the opposite
#'
#' @param sort A logical,  should the people be sorted by display
#' name?
#' @return A data.frame, with people corresponding to rows. If sort = F
#' then the new people will be appended to the end of the data.frame
#' @author R.J.B. Goudie
load_people_prior_union_today <- function(sort = F){
  people_prior <- load_people_prior()
  people_today <- load_people_today_less_prior()
  people <- rbind(people_prior, people_today)

  if (sort){
    sort_by_display_name(people)
  } else {
    people
  }
}

#' @title Load union of most recent and second most recent 'people'
#'
#' @description
#' The people file changes from month to month. This gives all the
#' people from last month, plus the new people.
#'
#' If sort = F, people who left in the last month appear at the bottom.
#' \link{load_people_prior_union_today} gives the opposite
#'
#' @param sort A logical,  should the people be sorted by display
#' name?
#' @return A data.frame, with people corresponding to rows. If sort = F
#' then the people who have left in the last month will be appended to the end
#' of the data.frame
#' @author R.J.B. Goudie
load_people_today_union_prior <- function(sort = F){
  people_prior <- load_people_prior_less_today()
  people_today <- load_people_today()
  people <- rbind(people_today, people_prior)

  if (sort){
    sort_by_display_name(people)
  } else {
    people
  }
}

#' @title Load setdiff of most recent and second most recent 'people'
#'
#' @description
#' The people file changes from month to month. This gives all the
#' people from this month, minus the people from last month. ie just
#' newbies
#'
#' \link{load_people_prior_less_today} gives the opposite
#'
#' @param sort A logical, should the people be sorted by display
#' name?
#' @return A data.frame, with people corresponding to rows.
#' @author R.J.B. Goudie
load_people_today_less_prior <- function(sort = F){
  people_prior <- load_people_prior()
  people_today <- load_people_today()

  new_people_rows <- !people_today$unique_id %in% people_prior$unique_id
  people <- people_today[new_people_rows, ]

  if (sort){
    sort_by_display_name(people)
  } else {
    people
  }
}

#' @title Load setdiff of second most recent and most recent 'people'
#'
#' @description
#' The people file changes from month to month. This gives all the
#' people from last month, minus the new people.
#'
#' \link{load_people_today_less_prior} gives the opposite
#'
#' @param sort A logical,  should the people be sorted by display
#' name?
#' @return A data.frame, with people corresponding to rows.
#' @author R.J.B. Goudie
load_people_prior_less_today <- function(sort = F){
  people_prior <- load_people_prior()
  people_today <- load_people_today()

  removed_people_rows <- !people_prior$unique_id %in% people_today$unique_id
  people <- people_prior[removed_people_rows, ]

  if (sort){
    sort_by_display_name(people)
  } else {
    people
  }
}
