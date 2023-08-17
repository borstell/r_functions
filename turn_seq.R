# Define function for strictly sequential turn segments (chronological by start time)
turn_seq <- function(data, file, start, end, participant, annotation, simplify = TRUE, long = FALSE) {
  df <- data %>% 
    arrange({{ file }}, {{ start }}) %>% 
    mutate(turn = consecutive_id({{ participant }}), .by = {{ file }})
  if (simplify) {
    df <- df %>% 
      group_by({{ file }}, {{ participant}}, turn) %>% 
      mutate(annotations = paste0({{ annotation }}, collapse = " ")) %>% 
      mutate(start = min({{ start }}),
             end = max({{ end }})) %>% 
      mutate(duration = end-start) %>% 
      slice(1) %>% 
      select(-{{ annotation }}) %>% 
      ungroup()
    if (long) {
      df <- df %>% 
        arrange({{ file }}, {{ participant }}, {{ start }}) %>% 
        mutate(turn = if_else(row_number()!=1 & start<=lag(end), lag(turn), turn), .by = c({{ file }}, {{ participant }})) %>% 
        group_by({{ file }}, {{ participant}}, turn) %>%
        mutate(annotations = paste0(annotations, collapse = " ")) %>% 
        mutate(start = min({{ start }}),
               end = max({{ end }})) %>%
        slice(1) %>% 
        ungroup()
    }
  }
  return(df)
}
