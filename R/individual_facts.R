
#' Add a fact associated with an individual
#' 
#' @details
#' The three-letter codes used for the type parameter are:
#' 
#' Attributes:
#' res(idence), occ(upation), edu(cation), pos(sessions), cas(te), phy(sical description), 
#' rel(igion), cit(izenship or nationality), nob(ility title), nid (national ID number),
#' nur (number of relationships), nuc (number of children),
#' 
#' Events:
#' bir(th), dea(th), cen(sus), ado(ption), bap(tism), chr(istening), bur(ial), 
#' adu(lt christening), wil(l), gra(duation), pro(bate), 
#' ret(irement), cre(mation), bar(-mitvah), bas(-mitzvah), 
#' emi(gration), imm(igration), con(firmation), fir(st communion), nat(uralization).
#' 
#' Alternatively eve or att (for any other event or attribute).
#' 
#' If attributes (except residence) or 'other' events/attributes are used then the descriptor 
#' argument must be provided.
#'
#' @param gedcom A tidyged object.
#' @param type A (case-insensitive) three-letter code giving the type of event or attribute. See Details.
#' @param descriptor A short description of the attribute (which is not a residence) or 'other' event.
#' @param classification A descriptive word or phrase used to further classify this 
#' fact. This should be used whenever the 'other' event/attribute is used (but can also be used
#' with others).
#' @param date A date_calendar(), date_approximated(), date_period(), or date_range() 
#' object giving the timing of the fact.
#' @param age A character string that indicates the age in years, months, and days 
#' that the individual was at the time of the fact. Any combination of these is permitted. 
#' Any labels must come after their corresponding number, for example; "4y 8m 10d".
#' @param cause Used in special cases to record the reasons which precipitated an event. 
#' Normally this will be used for a death event to show cause of death, such as might be listed 
#' on a death certificate.
#' @param user_reference_type A user-defined type to associate with an attribute. This argument
#' is mandatory for type = nid and type = att.
#' @param fact_place A place() object giving the place associated with this fact.
#' @param fact_address An address() object giving the address associated with this fact.
#' @param notes A character vector of notes accompanying the fact. These could be xrefs to 
#' existing Note records.
#' @param responsible_agency The organisation, institution, corporation, person, or other 
#' entity that has responsibility for the fact.
#' @param religious_affiliation A name of the religion with which this fact was affiliated.
#' @param family_xref The xref of the family associated of which this individual is a child.
#' Only used for birth, christening, or adoption events.
#' @param adopting_parent A code which shows which parent in the associated family adopted this 
#' individual. Either "HUSB", "WIFE", or "BOTH".
#' @param multimedia_links A character vector of multimedia file references accompanying this
#' fact. These could be xrefs to existing Multimedia records.
#' @param xref The xref of a record to act on if one is not activated (will override active record).
#' @param update_date_changed Whether to add/update the change date for the record.
#' @return An updated tidyged object with an expanded Individual record including
#' this fact.
#' @export
#' @tests
#' expect_snapshot_value(gedcom(subm("Me")) %>% 
#'                        add_indi(sex = "M") %>% 
#'                        add_indi_fact(type = "chr",
#'                                       date = date_calendar(year = 1956),
#'                                       age = "1y",
#'                                       fact_address = address(c("line1","line2","line3","line4")),
#'                                       fact_place = place("There",
#'                                                     notes = "Place note")) %>% 
#'                        tidyged.internals::remove_dates_for_tests(), "json2")
#' expect_snapshot_value(gedcom(subm("Me")) %>% 
#'                        add_indi(sex = "M") %>% 
#'                        add_indi_fact(type = "occ",
#'                                      descriptor = "Jedi",
#'                                      fact_address = address(c("line1","line2","line3","line4")),
#'                                      fact_place = place("There",
#'                                                    notes = "Place note")) %>% 
#'                        tidyged.internals::remove_dates_for_tests(), "json2")
add_indi_fact <- function(gedcom,
                          type,
                          descriptor = "",
                          classification = character(),
                          date = character(),
                          age = character(),
                          cause = character(),
                          user_reference_type = character(),
                          fact_place = place(),
                          fact_address = address(),
                          notes = character(),
                          responsible_agency = character(),
                          religious_affiliation = character(),
                          family_xref = character(),
                          adopting_parent = character(),
                          multimedia_links = character(),
                          xref = character(),
                          update_date_changed = TRUE) {
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_indi, is_indi)
  
  type <- tolower(stringr::str_sub(type, 1, 3))
  type <- dplyr::case_when(type == "bir" ~ "BIRT",
                           type == "dea" ~ "DEAT",
                           type == "cen" ~ "CENS",
                           type == "res" ~ "RESI",
                           type == "occ" ~ "OCCU",
                           type == "rel" ~ "RELI",
                           type == "ado" ~ "ADOP",
                           type == "bap" ~ "BAPM",
                           type == "bar" ~ "BARM",
                           type == "bas" ~ "BASM",
                           type == "adu" ~ "CHRA",
                           type == "chr" ~ "CHR",
                           type == "bur" ~ "BURI",
                           type == "cre" ~ "CREM",
                           type == "con" ~ "CONF",
                           type == "fir" ~ "FCOM",
                           type == "emi" ~ "EMIG",
                           type == "imm" ~ "IMMI",
                           type == "nat" ~ "NATU",
                           type == "pro" ~ "PROB",
                           type == "wil" ~ "WILL",
                           type == "gra" ~ "GRAD",
                           type == "ret" ~ "RETI",
                           type == "cas" ~ "CAST",
                           type == "phy" ~ "DSCR",
                           type == "nid" ~ "IDNO",
                           type == "edu" ~ "EDUC",
                           type == "cit" ~ "NATI",
                           type == "nur" ~ "NMR",
                           type == "nuc" ~ "NCHI",
                           type == "pos" ~ "PROP",
                           type == "nob" ~ "TITL",
                           type == "att" ~ "FACT",
                           type == "eve" ~ "EVEN",
                           TRUE ~ "error")
  
  if(type == "error") stop("type not recognised")
  
  if(type %in% c("FACT","EVEN","CAST","DSCR","EDUC","IDNO","NATI",
                 "NMR","OCCU","PROP","RELI","TITL","NCHI")) {
    if(descriptor == "") stop("A descriptor must be given with this type")
  } else {
    descriptor == ""
  }
    
  
  if(length(user_reference_type) == 0 & type %in% c("IDNO","FACT"))
    stop("A user_reference_type must be defined for national ID numbers and other attributes")
  
  
  even_notes <- purrr::map(notes, tidyged.internals::NOTE_STRUCTURE)
  
  media_links <- purrr::map_chr(multimedia_links, find_xref, 
                                gedcom = gedcom, record_xrefs = xrefs_media(gedcom), tags = "FILE") %>% 
    purrr::map(tidyged.internals::MULTIMEDIA_LINK)
  
  details1 <- tidyged.internals::EVENT_DETAIL(event_or_fact_classification = classification,
                                              date = date,
                                              place = fact_place,
                                              address = fact_address,
                                              responsible_agency = responsible_agency,
                                              religious_affiliation = religious_affiliation,
                                              cause_of_event = cause,
                                              notes = even_notes,
                                              multimedia_links = media_links)
  
  details2 <- tidyged.internals::INDIVIDUAL_EVENT_DETAIL(event_details = details1,
                                                         age_at_event = age)
  
  
  if(type %in% c("CAST","DSCR","EDUC","IDNO","NATI","NCHI","NMR","OCCU","PROP","RELI","RESI","TITL","FACT")) {
    
    fact_str <- tidyged.internals::INDIVIDUAL_ATTRIBUTE_STRUCTURE(attribute_type = type,
                                                                  attribute_descriptor = descriptor,
                                                                  individual_event_details = details2,
                                                                  user_reference_type = user_reference_type) %>% 
      tidyged.internals::add_levels(1)
    
  } else {
    
    fact_str <- tidyged.internals::INDIVIDUAL_EVENT_STRUCTURE(event_type_individual = type,
                                                              event_descriptor = descriptor,
                                                              individual_event_details = details2,
                                                              xref_fam = family_xref,
                                                              adopted_by_which_parent = adopting_parent) %>% 
      tidyged.internals::add_levels(1)
    
  }
  
  if(update_date_changed) {
    gedcom <-  tidyged.internals::remove_section(gedcom, 1, "CHAN", "", xrefs = xref)
    fact_str <- dplyr::bind_rows(fact_str, tidyged.internals::CHANGE_DATE() %>% 
                                    tidyged.internals::add_levels(1))
  }
  
  next_row <- tidyged.internals::find_insertion_point(gedcom, xref, 0, "INDI")
  
  gedcom %>%
    tibble::add_row(fact_str, .before = next_row) %>% 
    tidyged.internals::finalise() %>% 
    activate_indi(xref)
  
}


