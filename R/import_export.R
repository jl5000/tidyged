

import_gedcom <- function(filepath) {
  
  readr::read_lines(filepath) %>% 
    stringr::str_trim() %>% 
    tibble::tibble(text = .) %>%
    tidyr::extract(text, into = c("level", "id", "tag", "value"), 
                   regex = "^(.) (@.+@)? ?(\\w{3,5}) ?(.*)$") %>%
    dplyr::mutate(id = dplyr::if_else(tag == "HEAD", "HD", id),
                  id = dplyr::if_else(tag == "TRLR", "TR", id)) %>%
    tidyr::fill(id) %>% 
    dplyr::mutate(level = as.numeric(level))

}



export_gedcom <- function(gedcom_df, filepath) {
  
  gedcom_df %>%
    dplyr::mutate(id = if_else(lag(id) == id, "", id)) %>% 
    tidyr::replace_na(list(id = "")) %>% 
    dplyr::transmute(text = paste(level, id, tag, value)) %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "  ", " ")) %>%
    write.table(filepath, na = "", col.names = FALSE, quote = FALSE, row.names = FALSE)
  
}






