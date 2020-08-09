
add_family_event <- function(gedcom,
                             family_xref = character(),
                             event_type = character(),
                             event_subtype = character(),
                             event_descriptor = character(),
                             husband_age_at_event = character(),
                             wife_age_at_event = character(),
                             event_date = date_value(),
                             event_cause = character(),
                             event_place = PLACE_STRUCTURE(character()),
                             event_address = ADDRESS_STRUCTURE(character())) {
  
  check_active_record_valid(gedcom, record_string_fam(), is_family)
  
  
}

#' @export
#' @rdname add_family_event
add_family_event_annulment <- purrr::partial(add_family_event, event_type = "ANUL")
#' @export
#' @rdname add_family_event
add_family_event_census <- purrr::partial(add_family_event, event_type = "CENS")
#' @export
#' @rdname add_family_event
add_family_event_divorce <- purrr::partial(add_family_event, event_type = "DIV")
#' @export
#' @rdname add_family_event
add_family_event_divorce_filed <- purrr::partial(add_family_event, event_type = "DIVF")
#' @export
#' @rdname add_family_event
add_family_event_engagement <- purrr::partial(add_family_event, event_type = "ENGA")
#' @export
#' @rdname add_family_event
add_family_event_marriage_banns <- purrr::partial(add_family_event, event_type = "MARB")
#' @export
#' @rdname add_family_event
add_family_event_marriage_contract <- purrr::partial(add_family_event, event_type = "MARC")
#' @export
#' @rdname add_family_event
add_family_event_marriage <- purrr::partial(add_family_event, event_type = "MARR")
#' @export
#' @rdname add_family_event
add_family_event_marriage_license <- purrr::partial(add_family_event, event_type = "MARL")
#' @export
#' @rdname add_family_event
add_family_event_marriage_settlement <- purrr::partial(add_family_event, event_type = "MARS")
#' @export
#' @rdname add_family_event
add_family_event_residence <- purrr::partial(add_family_event, event_type = "RESI")
#' @export
#' @rdname add_family_event
add_family_event_other <- purrr::partial(add_family_event, event_type = "EVEN")

