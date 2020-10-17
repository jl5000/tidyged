

#' Add a family event to a family record
#'
#' @param gedcom 
#' @param family_xref 
#' @param event_type 
#' @param event_subtype 
#' @param event_descriptor 
#' @param husband_age_at_event 
#' @param wife_age_at_event 
#' @param event_notes 
#' @param event_date 
#' @param event_cause 
#' @param place_name 
#' @param place_hierarchy 
#' @param place_phonetic_variation 
#' @param phonetic_type 
#' @param place_romanized_variation 
#' @param romanized_type 
#' @param place_latitude 
#' @param place_longitude 
#' @param place_notes 
#' @param address_first_line 
#' @param city 
#' @param state 
#' @param postal_code 
#' @param country 
#' @param phone_number 
#' @param email 
#' @param fax 
#' @param web_page 
#' @param responsible_agency 
#' @param religious_affiliation 
#' @param cause_of_event 
#' @param restriction_notice 
#'
#' @return
add_family_event <- function(gedcom,
                             family_xref = character(),
                             event_type = character(),
                             event_subtype = character(),
                             event_descriptor = character(),
                             husband_age_at_event = character(),
                             wife_age_at_event = character(),
                             event_notes = character(),
                             event_date = date_value(),
                             event_cause = character(),
                             place_name = character(),
                             place_hierarchy = character(),
                             place_phonetic_variation = character(),
                             phonetic_type = character(),
                             place_romanized_variation = character(),
                             romanized_type = character(),
                             place_latitude = character(),
                             place_longitude = character(),
                             place_notes = character(),
                             address_first_line = character(),
                             city = character(),
                             state = character(),
                             postal_code = character(),
                             country = character(),
                             phone_number = character(),
                             email = character(),
                             fax = character(),
                             web_page = character(),
                             responsible_agency = character(),
                             religious_affiliation = character(),
                             cause_of_event = character(),
                             restriction_notice = character()) {
  
  check_active_record_valid(gedcom, record_string_fam(), is_family)
  
}


#' @rdname add_family_event
#' @export
add_family_event_annulment <- purrr::partial(add_family_event, event_type = "ANUL")
rlang::fn_fmls(add_family_event_annulment) <- purrr::list_modify(rlang::fn_fmls(add_family_event), 
                                                              event_type = "ANUL")
#' @rdname add_family_event
#' @export
add_family_event_census <- purrr::partial(add_family_event, event_type = "CENS")
rlang::fn_fmls(add_family_event_census) <- purrr::list_modify(rlang::fn_fmls(add_family_event), 
                                                              event_type = "CENS")
#' @export
#' @rdname add_family_event
add_family_event_divorce <- purrr::partial(add_family_event, event_type = "DIV")
rlang::fn_fmls(add_family_event_divorce) <- purrr::list_modify(rlang::fn_fmls(add_family_event), 
                                                              event_type = "DIV")
#' @export
#' @rdname add_family_event
add_family_event_divorce_filed <- purrr::partial(add_family_event, event_type = "DIVF")
rlang::fn_fmls(add_family_event_divorce_filed) <- purrr::list_modify(rlang::fn_fmls(add_family_event), 
                                                              event_type = "DIVF")
#' @export
#' @rdname add_family_event
add_family_event_engagement <- purrr::partial(add_family_event, event_type = "ENGA")
rlang::fn_fmls(add_family_event_engagement) <- purrr::list_modify(rlang::fn_fmls(add_family_event), 
                                                              event_type = "ENGA")
#' @export
#' @rdname add_family_event
add_family_event_marriage_banns <- purrr::partial(add_family_event, event_type = "MARB")
rlang::fn_fmls(add_family_event_marriage_banns) <- purrr::list_modify(rlang::fn_fmls(add_family_event), 
                                                              event_type = "MARB")
#' @export
#' @rdname add_family_event
add_family_event_marriage_contract <- purrr::partial(add_family_event, event_type = "MARC")
rlang::fn_fmls(add_family_event_marriage_contract) <- purrr::list_modify(rlang::fn_fmls(add_family_event), 
                                                              event_type = "MARC")
#' @export
#' @rdname add_family_event
add_family_event_marriage <- purrr::partial(add_family_event, event_type = "MARR")
rlang::fn_fmls(add_family_event_marriage) <- purrr::list_modify(rlang::fn_fmls(add_family_event), 
                                                              event_type = "MARR")
#' @export
#' @rdname add_family_event
add_family_event_marriage_license <- purrr::partial(add_family_event, event_type = "MARL")
rlang::fn_fmls(add_family_event_marriage_license) <- purrr::list_modify(rlang::fn_fmls(add_family_event), 
                                                              event_type = "MARL")
#' @export
#' @rdname add_family_event
add_family_event_marriage_settlement <- purrr::partial(add_family_event, event_type = "MARS")
rlang::fn_fmls(add_family_event_marriage_settlement) <- purrr::list_modify(rlang::fn_fmls(add_family_event), 
                                                              event_type = "MARS")
#' @export
#' @rdname add_family_event
add_family_event_residence <- purrr::partial(add_family_event, event_type = "RESI")
rlang::fn_fmls(add_family_event_residence) <- purrr::list_modify(rlang::fn_fmls(add_family_event), 
                                                              event_type = "RESI")
#' @export
#' @rdname add_family_event
add_family_event_other <- purrr::partial(add_family_event, event_type = "EVEN")
rlang::fn_fmls(add_family_event_other) <- purrr::list_modify(rlang::fn_fmls(add_family_event), 
                                                              event_type = "EVEN")

