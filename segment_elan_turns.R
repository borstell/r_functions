# Load libraries
library(tidyverse)

# Set themes
theme_set(theme_classic(base_size=15))
options(ggplot2.discrete.colour = c("dodgerblue", "orange")) 

# Make example data
df <- tibble(file=paste0("file_00", c(rep(1, 14), rep(2, 6))),
             signer=paste0("S",c(rep(1, 6), 2, 1, 2, 2, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1)),
             hand=c("right", "right", "right", "left", "right", "right", "right", "right", "right", "left", "left", "right", "right", "right", "right", "left", "right", "right", "right", "left"),
             start=c(1000, 1200, 1400, 1600, 1650, 1850, 2000, 2500, 2550, 2600, 2650, 2800, 2900, 3400, 1000, 1100, 2000, 2050, 2500, 2550),
             end=c(1150, 1350, 1550, 2550, 1800, 2000, 2100, 2650, 2600, 2650, 2700, 2900, 3100, 3500, 1200, 1900, 2100, 2150, 2600, 2800),
             annotation=paste0("GLOSS-", LETTERS[1:20])) 

# Define function for strictly sequential turn segments (chronological by start time)
turn_seq <- function(data, file, start, end, participant, annotation, simplify = TRUE) {
  df <- data %>% 
    arrange({{ file }}, {{ start }}) %>% 
    mutate(turn = consecutive_id({{ file }}, {{ participant }}))
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

# Plot sequential (gives overlapping turns within participants in this example)
ggplot() +
  geom_segment(data=turn_seq(df, file = file, start = start, end = end, participant = signer, annotation = annotation, simplify = T),
               aes(x=start, xend=end, y=paste0(signer,"_seq"), yend=paste0(signer, "_seq"), color=signer),
               linewidth=5, show.legend = F, alpha=.5) +
  geom_segment(data=turn_df, aes(x=start, xend=end, y=paste0(signer, "_", hand), yend=paste0(signer, "_", hand), color=signer),
               linewidth=5, show.legend = F, alpha=1) +
  labs(y="", x="Time (ms)") +
  facet_wrap(~file, ncol=1)

# Plot with different intervals
ggplot() +
  geom_segment(data=turn_interval(df, file = file, start = start, end = end, participant = signer, annotation = annotation, interval = 100, simplify = T),
               aes(x=start, xend=end, y=paste0(signer,"_100"), yend=paste0(signer, "_100"), color=signer),
               linewidth=5, show.legend = F, alpha=.5) +
  geom_segment(data=turn_interval(df, file = file, start = start, end = end, participant = signer, annotation = annotation, interval = 200, simplify = T),
               aes(x=start, xend=end, y=paste0(signer,"_200"), yend=paste0(signer, "_200"), color=signer),
               linewidth=5, show.legend = F, alpha=.5) +
  geom_segment(data=turn_interval(df, file = file, start = start, end = end, participant = signer, annotation = annotation, interval = 400, simplify = T),
               aes(x=start, xend=end, y=paste0(signer,"_400"), yend=paste0(signer, "_400"), color=signer),
               linewidth=5, show.legend = F, alpha=.5) +
  geom_segment(data=turn_interval(df, file = file, start = start, end = end, participant = signer, annotation = annotation, interval = 800, simplify = T),
               aes(x=start, xend=end, y=paste0(signer,"_800"), yend=paste0(signer, "_800"), color=signer),
               linewidth=5, show.legend = F, alpha=.5) +
  geom_segment(data=turn_df, aes(x=start, xend=end, y=paste0(signer, "_", hand), yend=paste0(signer, "_", hand), color=signer),
               linewidth=5, show.legend = F, alpha=1) +
  labs(y="", x="Time (ms)") +
  facet_wrap(~file, ncol=1)
