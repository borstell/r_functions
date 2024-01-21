####
#### R functions for segmenting ELAN annotation data frames into turns
####

# Define function for turn segments within intervals (chronological by 
# start time per participant, but merge segments within interval)
turn_interval <- function(data, 
                          file, 
                          start, 
                          end, 
                          participant, 
                          annotation, 
                          interval = 300, 
                          simplify = TRUE) {
  df <- data |>  
    dplyr::arrange({{ file }}, {{ participant }}, {{ start }}) |>
    
    # Check what the highest end time seen is at each incremental point
    dplyr::mutate(longest_span = cummax({{ end }}), 
           .by = c({{ file }}, {{ participant }})) |>  
    
    # Compare highest end time seen with defined interval to identify pauses
    dplyr::mutate(gap = if_else(
      is.na(dplyr::lag({{ end }})), 
      TRUE, 
      {{ start }}-dplyr::lag(longest_span)>=interval), 
      .by = c({{ file }}, {{ participant }})) |>
    
    # Create turns based on pauses
    dplyr::mutate(turn = cumsum(gap), 
           .by = c({{ file }}, {{ participant }}))
  
  # This step pivots annotations into turns
  if (simplify) {
    df <- df |> 
      dplyr::group_by({{ file }}, {{ participant }}, turn) |>  
      dplyr::mutate(annotations = paste0({{ annotation }}, collapse = " ")) |> 
      dplyr::mutate(start = min({{ start }}),
             end = max({{ end }})) |> 
      dplyr::slice(1) |>  
      dplyr::select(-c({{ annotation }}, gap, longest_span)) |> 
      dplyr::ungroup()
  }
  return(df)
}

# Define function for turn segments by most signs per 3-signs moving time window
turn_quant <- function(data,
                       file, 
                       start, 
                       end, 
                       participant, 
                       annotation, 
                       simplify = TRUE, 
                       long = FALSE) {
  
  # Create windows of "whose turn" by max signing (number of signs)
  df <- data |>  
    dplyr::arrange({{ file }}, {{ start }}) |>   
    
    # Find who signs the most at any given 3-sign window
    dplyr::mutate(whose_turn = slider::slide_chr({{ participant }}, ~ names(which.max(table(.x))), 
                                          .before = 1, .after = 1)) |>  
    dplyr::mutate(current_turn = dplyr::if_else(
      {{ participant }}==whose_turn, 
      "same", 
      "other")) |>  
    dplyr::mutate(turn = consecutive_id(whose_turn))
  if (simplify) {
    df <- df |>  
      dplyr::group_by({{ file }}, turn) |>  
      dplyr::mutate(annotations = paste0({{ annotation }}, collapse = " ")) |>  
      dplyr::mutate(start = min({{ start }}),
             end = max({{ end }})) |> 
      dplyr::slice(1) |> 
      dplyr::select(-c({{ annotation }})) |>  
      dplyr::ungroup()
    if (long) {
      df <- df |> 
        dplyr::arrange({{ file }}, {{ participant }}, {{ start }}) |> 
        dplyr::mutate(turn = if_else(
          dplyr::row_number()!=1 & start<=dplyr::lag(end), 
          dplyr::lag(turn), turn), 
          .by = c({{ file }}, {{ participant }})) |> 
        dplyr::group_by({{ file }}, {{ participant}}, turn) |>
        dplyr::mutate(annotations = paste0(annotations, collapse = " ")) |> 
        dplyr::mutate(start = min({{ start }}),
               end = max({{ end }})) |>
        dplyr::slice(1) |>  
        dplyr::ungroup()
    }
  }
  return(df)
}

# Define function for strictly sequential turn segments 
# (chronological by start time)
turn_seq <- function(data, 
                     file, 
                     start, 
                     end, 
                     participant, 
                     annotation, 
                     simplify = TRUE, 
                     long = FALSE) {
  df <- data |>  
    dplyr::arrange({{ file }}, {{ start }}) |>  
    dplyr::mutate(turn = dplyr::consecutive_id({{ participant }}), 
           .by = {{ file }})
  if (simplify) {
    df <- df |>  
      dplyr::group_by({{ file }}, {{ participant}}, turn) |> 
      dplyr::mutate(annotations = paste0({{ annotation }}, collapse = " ")) |> 
      dplyr::mutate(start = min({{ start }}),
             end = max({{ end }})) |>  
      dplyr::mutate(duration = end-start) |>  
      dplyr::slice(1) |> 
      dplyr::select(-{{ annotation }}) |>  
      dplyr::ungroup()
    if (long) {
      df <- df |>  
        dplyr::arrange({{ file }}, {{ participant }}, {{ start }}) |>  
        dplyr::mutate(turn = dplyr::if_else(
          dplyr::row_number()!=1 & start<=dplyr::lag(end), 
          dplyr::lag(turn), 
          turn), 
          .by = c({{ file }}, {{ participant }})) |>  
        dplyr::group_by({{ file }}, {{ participant}}, turn) |> 
        dplyr::mutate(annotations = paste0(annotations, collapse = " ")) |>  
        dplyr::mutate(start = min({{ start }}),
               end = max({{ end }})) |> 
        dplyr::slice(1) |>  
        dplyr::ungroup()
    }
  }
  return(df)
}
