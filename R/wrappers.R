

#' Add parent records for an individual
#' 
#' This function adds placeholder records for an individual's parents.
#' 
#' @details This function may also create a Family Group record and will 
#' not modify existing parents.
#'
#' @param gedcom A tidyged object.
#' @param xref The xref of an Individual record to act on if one is not activated (will override active record).
#' @param inc_sex Whether to populate the sex of the parents. This will ensure
#' that there is one male and one female parent. Otherwise the sex will be
#' assigned as "U" (undetermined).
#'
#' @return A tidyged object with additional parent records.
#' @export
add_parents <- function(gedcom, xref = character(), inc_sex = TRUE){
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_indi, is_indi)
  
  xref_par <- get_indi_parents(gedcom, xref, birth_only = TRUE)
  
  if(length(xref_par) >= 2) return(gedcom)
  
  # check if family exists
  xref_famc <- get_families_as_child(gedcom, xref, birth_only = TRUE)
  
  if(length(xref_famc) == 0){
    xref_famc <- tidyged.internals::assign_xref_famg(gedcom)
    gedcom <- add_famg(gedcom) |>
      activate_indi(xref) |>
      add_indi_links_to_families(famg_xref_chil = xref_famc)
  }
  
  if(length(xref_par) == 1){
    
    par_sex_new <- "U"
    
    if(inc_sex){
      par_sex_cur <- gedcom$value[gedcom$record == xref_par & gedcom$tag == "SEX"]
      
      if(length(par_sex_cur) == 1){
        par_sex_new <- dplyr::case_when(par_sex_cur == "M" ~ "F",
                                        par_sex_cur == "F" ~ "M",
                                        TRUE ~ "U")
      }
    }
    
    gedcom <- add_indi(gedcom, sex = dplyr::if_else(inc_sex, par_sex_new, "U")) |>
      add_indi_links_to_families(famg_xref_spou = xref_famc) 
    
  } else {
    # No parents - add them both
    gedcom <- add_indi(gedcom, sex = dplyr::if_else(inc_sex, "M", "U")) |>
      add_indi_links_to_families(famg_xref_spou = xref_famc) |>
      add_indi(sex = dplyr::if_else(inc_sex, "F", "U")) |>
      add_indi_links_to_families(famg_xref_spou = xref_famc)
  }
  
  activate_indi(gedcom, xref)
}


#' Create multiple siblings for an Individual
#'
#' @param gedcom A tidyged object.
#' @param xref The xref of an Individual record to act on if one is not activated (will override active record).
#' @param sexes A character string giving the sexes of each sibling. For example,
#' "FFM" to add two sisters and one brother. See the help for `add_indi()`
#' for possible codes.
#'
#' @return A tidyged object with additional sibling records.
#' @export
add_siblings <- function(gedcom, xref = character(), sexes = NULL){
  
  if(is.null(sexes)) return(gedcom)
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_indi, is_indi)
  
  xref_famc <- get_families_as_child(gedcom, xref, birth_only = TRUE)
  
  if(length(xref_famc) == 0){
    xref_famc <- tidyged.internals::assign_xref_famg(gedcom)
    gedcom <- add_famg(gedcom) |>
      activate_indi(xref) |>
      add_indi_links_to_families(famg_xref_chil = xref_famc)
  }
  
  sexes_vec <- unlist(strsplit(sexes, split = NULL))
  
  for(sib in sexes_vec){
    if(!sib %in% tidyged.internals::val_sexes()){
      warning("Skipping sibling with unknown sex: ", sib)
      next
    }
    
    gedcom <- add_indi(gedcom, sex = sib) |>
      add_indi_links_to_families(famg_xref_chil = xref_famc)
    
  }
  
  activate_indi(gedcom, xref)
}

#' Create multiple children for a Family Group
#'
#' @param gedcom A tidyged object.
#' @param xref The xref of a Family Group record to act on if one is not activated (will override active record).
#' @param sexes A character string giving the sexes of each child. For example,
#' "FFM" to add two daughters and one son. See the help for `add_indi()`
#' for possible codes.
#'
#' @return A tidyged object with additional child records.
#' @export
add_children <- function(gedcom, xref = character(), sexes = NULL){
  
  if(is.null(sexes)) return(gedcom)
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_famg, is_famg)
  
  sexes_vec <- unlist(strsplit(sexes, split = NULL))
  
  for(chil in sexes_vec){
    if(!chil %in% tidyged.internals::val_sexes()){
      warning("Skipping child with unknown sex: ", chil)
      next
    }
    
    gedcom <- add_indi(gedcom, sex = chil) |>
      add_indi_links_to_families(famg_xref_chil = xref)
    
  }
  
  activate_famg(gedcom, xref)
}

#' Add a spouse for an individual
#' 
#' This creates a record for a spouse and their Family Group record.
#'
#' @param gedcom A tidyged object.
#' @param xref The xref of an Individual record to act on if one is not activated (will override active record).
#' @param sex The sex of the spouse.
#'
#' @return A tidyged object with additional spouse and Family Group records.
#' @export
add_spouse <- function(gedcom, xref = character(), sex = "U"){
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_indi, is_indi)
  
  xref_fams <- tidyged.internals::assign_xref_famg(gedcom)
  
  add_famg(gedcom) |>
    add_indi(sex = sex) |>
    add_indi_links_to_families(famg_xref_spou = xref_fams) |>
    activate_indi(xref) |>
    add_indi_links_to_families(famg_xref_spou = xref_fams) |>
    activate_indi(xref)
  
  
}