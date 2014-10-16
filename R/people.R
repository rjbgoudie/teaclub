#' @title Load most recent 'people' data frame
#' @param ... Passed to load_people_x_most_recent. Currently
#' includes sort logical
#' @return A data.frame, with people corresponding to rows
#' @author R.J.B. Goudie
load_people_latest <- function(...){
  load_people_x_most_recent(1, ...)
}

#' @title Load second most recent 'people' data frame
#' @param ... Passed to load_people_x_most_recent. Currently
#' includes sort logical
#' @return A data.frame, with people corresponding to rows
#' @author R.J.B. Goudie
load_people_penultimate <- function(...){
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
#' \link{load_people_latest_union_penultimate} gives the opposite
#'
#' @param sort A logical,  should the people be sorted by display
#' name?
#' @return A data.frame, with people corresponding to rows. If sort = F
#' then the new people will be appended to the end of the data.frame
#' @author R.J.B. Goudie
load_people_penultimate_union_latest <- function(sort = F){
  people_penultimate <- load_people_penultimate()
  people_new <- load_people_latest_less_penultimate()
  people <- rbind(people_penultimate, people_new)

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
#' \link{load_people_penultimate_less_latest} gives the opposite
#'
#' @param sort A logical, should the people be sorted by display
#' name?
#' @return A data.frame, with people corresponding to rows.
#' @author R.J.B. Goudie
load_people_latest_less_penultimate <- function(sort = F){
  people_penultimate <- load_people_penultimate()
  people_latest <- load_people_latest()

  new_people_rows <- !people_latest$unique_id %in% people_penultimate$unique_id
  people <- people_latest[new_people_rows, ]

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
#' \link{load_people_latest_less_penultimate} gives the opposite
#'
#' @param sort A logical,  should the people be sorted by display
#' name?
#' @return A data.frame, with people corresponding to rows.
#' @author R.J.B. Goudie
load_people_penultimate_less_latest <- function(sort = F){
  people_penultimate <- load_people_penultimate()
  people_latest <- load_people_latest()

  removed_people_rows <-
    !people_penultimate$unique_id %in% people_latest$unique_id
  people <- people_penultimate[removed_people_rows, ]

  if (sort){
    sort_by_display_name(people)
  } else {
    people
  }
}
