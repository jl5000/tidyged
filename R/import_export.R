

import_gedcom <- function(filepath) {
  
  ged <- readr::read_lines(filepath) %>% 
    stringr::str_trim() %>% 
    tibble::tibble(value = .) %>%
    tidyr::extract(value, into = c("level", "id", "tag", "value"), 
                   regex = "^(.) (@.+@)? ?(\\w{3,5}) ?(.*)$") %>%
    dplyr::mutate(id = dplyr::if_else(tag == "HEAD", "HD", id),
                  id = dplyr::if_else(tag == "TRLR", "TR", id)) %>%
    tidyr::fill(id) %>% 
    dplyr::mutate(level = as.numeric(level)) %>% 
    set_class_to_tidygedcom()

  validate_gedcom(ged)
  ged
  
}



export_gedcom <- function(gedcom_df, filepath) {
  
  gedcom_df %>%
    dplyr::mutate(id = dplyr::if_else(dplyr::lag(id) == id, "", id)) %>% 
    tidyr::replace_na(list(id = "")) %>% 
    dplyr::transmute(value = paste(level, id, tag, value)) %>% 
    dplyr::mutate(value = stringr::str_replace_all(value, "  ", " ")) %>%
    utils::write.table(filepath, na = "", col.names = FALSE, quote = FALSE, row.names = FALSE)
  
}






