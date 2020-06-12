

import_gedcom <- function(filepath) {

read_lines(filepath) %>% 
str_trim() %>% 
tibble(text = .) %>%
  extract(text, into = c("level", "id", "tag", "value"), 
          regex = "^(.) (@.+@)? ?(\\w{3,5}) ?(.*)$") %>%
  mutate(id = if_else(tag == "HEAD", "HD", id),
         id = if_else(tag == "TRLR", "TR", id)) %>%
  fill(id) %>% 
  mutate(level = as.numeric(level))

}



export_gedcom <- function(gedcom_df, filepath) {
  
  gedcom_df %>%
    mutate(id = if_else(lag(id) == id, "", id)) %>% 
    replace_na(list(id = "")) %>% 
    transmute(text = paste(level, id, tag, value)) %>% 
    mutate(text = stringr::str_replace_all(text, "  ", " ")) %>%
    write.table(filepath, na = "", col.names = FALSE, quote = FALSE, row.names = FALSE)
  
}






