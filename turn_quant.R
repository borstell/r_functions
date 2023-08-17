# Define function for turn segments by most signs per 3-signs moving time window
turn_quant <- function(data,  file, start, end, participant, annotation, simplify = TRUE, long = FALSE) {
  
  # Create windows of "whose turn" by max signing (number of signs)
  df <- data %>% 
    arrange({{ file }}, {{ start }}) %>% 
    mutate(whose_turn = slider::slide_chr(signer, ~ names(which.max(table(.x))), .before = 1, .after = 1)) %>% 
    mutate(current_turn = if_else({{ participant }}==whose_turn, "same", "other")) %>% 
    mutate(turn = consecutive_id(whose_turn))
  if (simplify) {
    df <- df %>% 
      group_by({{ file }}, turn) %>% 
      mutate(annotations = paste0({{ annotation }}, collapse = " ")) %>% 
      mutate(start = min({{ start }}),
             end = max({{ end }})) %>%
      slice(1) %>% 
      select(-c({{ annotation }})) %>% 
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
