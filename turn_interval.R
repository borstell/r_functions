# Define function for turn segments within intervals (chronological by start time per participant, but merge segments within interval)
turn_interval <- function(data,  file, start, end, participant, annotation, interval = 300, simplify = TRUE) {
  df <- data %>% 
    arrange({{ file }}, {{ participant }}, {{ start }}) %>% 
    mutate(longest_span = cummax({{ end }}), .by = c({{ file }}, {{ participant }})) %>% 
    mutate(gap = if_else(is.na(lag({{ end }})), TRUE, {{ start }}-lag(longest_span)>=interval), .by = c({{ file }}, {{ participant }})) %>% 
    mutate(turn = cumsum(gap), .by = c({{ file }}, {{ participant }}))
  if (simplify) {
    df <- df %>% 
      group_by({{ file }}, {{ participant }}, turn) %>% 
      mutate(annotations = paste0({{ annotation }}, collapse = " ")) %>% 
      mutate(start = min({{ start }}),
             end = max({{ end }})) %>%
      slice(1) %>% 
      select(-c({{ annotation }}, gap, longest_span)) %>% 
      ungroup()
  }
  return(df)
}
