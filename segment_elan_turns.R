# Load libraries
library(tidyverse)
library(slider)
library(glue)

# Set themes
theme_set(theme_classic(base_size=15))
options(ggplot2.discrete.colour = c("dodgerblue", "orange")) 

# Make example data
df <- tibble(file=paste0("file_00", c(rep(1, 14), rep(2, 6))),
             signer=paste0("S",c(rep(1, 6), 2, 1, 2, 2, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1)),
             hand=c("rh", "rh", "rh", "lh", "rh", "rh", "rh", "rh", "rh", "lh", "lh", "rh", "rh", "rh", "rh", "lh", "rh", "rh", "rh", "lh"),
             start=c(1000, 1200, 1400, 1600, 1650, 1850, 2000, 2500, 2550, 2600, 2650, 2800, 2900, 3400, 1000, 1100, 2000, 2050, 2500, 2550),
             end=c(1150, 1350, 1550, 2550, 1800, 2000, 2100, 2650, 2600, 2650, 2700, 2900, 3100, 3500, 1200, 1900, 2100, 2150, 2600, 2800),
             annotation=paste0("GLOSS-", LETTERS[1:20])) 

t <- df %>% 
  mutate(ci = consecutive_id(signer), .by = file) %>% 
  mutate(rn = row_number(), .by = c(file, signer))

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


# Plot sequential (gives overlapping turns within participants in this example)
ggplot() +
  geom_segment(data=turn_seq(df, file = file, start = start, end = end, participant = signer, annotation = annotation, simplify = T, long = T),
               aes(x=start, xend=end, y=glue::glue("{signer}_(seq)"), yend=glue::glue("{signer}_(seq)"), color=signer),
               linewidth=5, show.legend = F, alpha=.5) +
  geom_segment(data=df, aes(x=start, xend=end, y=glue::glue("{signer}_{hand}"), yend=glue::glue("{signer}_{hand}"), color=signer),
               linewidth=5, show.legend = F, alpha=1) +
  labs(y="", x="Time (ms)") +
  facet_wrap(~file, ncol=1)
#ggsave(width = 7, height = 4, units = "in", dpi = 600)


# Plot with different intervals

# Set intervals in ms
#ms <- 100
#ms <- 200
ms <- 300
#ms <- 800 

ggplot() +
  geom_segment(data=turn_interval(df, file = file, start = start, end = end, participant = signer, annotation = annotation, interval = ms, simplify = T),
               aes(x=start, xend=end, y=glue::glue("{signer}_(interval: {ms})"), yend=glue::glue("{signer}_(interval: {ms})"), color=signer),
               linewidth=5, show.legend = F, alpha=.5) +
  geom_segment(data=df, aes(x=start, xend=end, y=glue::glue("{signer}_{hand}"), yend=glue::glue("{signer}_{hand}"), color=signer),
               linewidth=5, show.legend = F, alpha=1) +
  labs(y="", x="Time (ms)") +
  facet_wrap(~file, ncol=1)
#ggsave(width = 7, height = 4, units = "in", dpi = 600)


# Plot quantified
ggplot() +
  geom_segment(data=turn_quant(df, file = file, start = start, end = end, participant = signer, annotation = annotation, simplify = T, long = T),
               aes(x=start, xend=end, y=glue::glue("{signer}_(quant)"), yend=glue::glue("{signer}_(quant)"), color=signer),
               linewidth=5, show.legend = F, alpha=.5) +
  geom_segment(data=df, aes(x=start, xend=end, y=glue::glue("{signer}_{hand}"), yend=glue::glue("{signer}_{hand}"), color=signer),
               linewidth=5, show.legend = F, alpha=1) +
  labs(y="", x="Time (ms)") +
  facet_wrap(~file, ncol=1)
#ggsave(width = 7, height = 4, units = "in", dpi = 600)
