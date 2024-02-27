
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(tidylo)
library(scales)
library(patchwork)



# Make simulated data -----------------------------------------------------

set.seed(1234)

df <- tibble(Gloss=paste0("SIGN-", sample(1:10000, 10000))) |> 
  mutate(n = case_when(
    row_number()==1 ~ 3201,
    .default = round((1/row_number())*3201)
  )) |> 
  uncount(n) |> 
  arrange(Gloss) |> 
  mutate(
    Gender=c(sample(c("Man" ,"Woman"), n()/2, replace = T, prob = c(.3, .7)), sample(c("Man" ,"Woman"), n()/2, replace = T, prob = c(.65, .35))),
    Region=c(sample(c("North" ,"South"), n()/4, replace = T, prob = c(.4, .6)), 
             sample(c("North" ,"South"), n()/4, replace = T, prob = c(.35, .65)),
             sample(c("North" ,"South"), n()/4, replace = T, prob = c(.5, .5)),
             sample(c("North" ,"South"), n()/4, replace = T, prob = c(.65, .35))),
    AgeGroup=c(sample(c("20-39" ,"40-59", "60+"), n()/2, replace = T, prob = c(.35, .25, .4)), 
               sample(c("20-39" ,"40-59", "60+"), n()/2, replace = T, prob = c(.3, .4, .3))),
    Text=c(sample(c("Narrative" ,"Conversation"), n()/2, replace = T, prob = c(.35, .65)),
           sample(c("Narrative" ,"Conversation"), n()/2, replace = T, prob = c(.55, .45)))
  )



# Calculate log odds for each variable ------------------------------------

# Age
a <- 
  df |> 
  count(AgeGroup, Gloss) |> 
  bind_log_odds(AgeGroup, Gloss, n) |> 
  mutate(LogOdds = log_odds_weighted) |> 
  mutate(Variable = "Age") |> 
  mutate(RelFreq = n/sum(n), .by = AgeGroup) |> 
  mutate(RelProp = RelFreq/sum(RelFreq), .by = Gloss) |> 
  mutate(Group = AgeGroup) |> 
  select(Variable, Group, Gloss, RelFreq, RelProp, LogOdds, n)

# Gender
g <- 
  df |> 
  count(Gender, Gloss) |> 
  bind_log_odds(Gender, Gloss, n) |> 
  mutate(LogOdds = log_odds_weighted) |> 
  mutate(Variable = "Gender") |> 
  mutate(RelFreq = n/sum(n), .by = Gender) |> 
  mutate(RelProp = RelFreq/sum(RelFreq), .by = Gloss) |> 
  mutate(Group = Gender) |> 
  select(Variable, Group, Gloss, RelFreq, RelProp, LogOdds, n)

# Region
r <- 
  df |> 
  count(Region, Gloss) |> 
  bind_log_odds(Region, Gloss, n) |> 
  mutate(LogOdds = log_odds_weighted) |> 
  mutate(Variable = "Region") |> 
  mutate(RelFreq = n/sum(n), .by = Region) |> 
  mutate(RelProp = RelFreq/sum(RelFreq), .by = Gloss) |> 
  mutate(Group = Region) |> 
  select(Variable, Group, Gloss, RelFreq, RelProp, LogOdds, n)

# Text type
t <- 
  df |> 
  count(Text, Gloss) |> 
  bind_log_odds(Text, Gloss, n) |> 
  mutate(LogOdds = log_odds_weighted) |> 
  mutate(Variable = "Text") |> 
  mutate(RelFreq = n/sum(n), .by = Text) |> 
  mutate(RelProp = RelFreq/sum(RelFreq), .by = Gloss) |> 
  mutate(Group = Text) |> 
  select(Variable, Group, Gloss, RelFreq, RelProp, LogOdds, n)

# Bind all together
agrt <- bind_rows(a, g, r, t)



# Function for calculating and plotting metrics ---------------------------

plot_frequencies <- function(data,             # The data frame of corpus data
                             signs,            # A vector of signs to plot
                             var,              # The column of the variable
                             grp,              # The value the group member
                             lex,              # The column of the sign glosses
                             metric="logodds", # The metric to be used
                             scales="free_x"   # Scaling the facets
                             ) {
  
  if (metric %in% c("relfreq", "logodds", "both")) {
    
    df <- 
      data |> 
      group_by({{ var }}) |> 
      complete({{ grp }}, {{ lex }}) |> 
      filter({{ lex }} %in% signs)
    
    lo <- 
      df |> 
      ggplot() +
      geom_hline(yintercept = 0) +
      geom_col(aes(x={{ grp }}, y=LogOdds, fill=LogOdds), color="grey50", show.legend = F) +
      geom_label(data=filter(df, is.na(LogOdds)), aes(x={{ grp }}, y=0, label="NA")) +
      scale_fill_gradient2(low="salmon", mid="white", high="dodgerblue") +
      labs(x=NULL, y="Weighted log odds", title=paste("Weighted log odds for", paste(sort(signs), collapse = " and "))) +
      facet_grid(rows=vars({{ lex }}), cols=vars({{ var }}), scales = "free_x", space = "free_x") +
      theme(strip.text = element_text(size=18))
    
    lf <- 
      df |> 
      ggplot() +
      geom_hline(yintercept = 0) +
      geom_col(aes(x={{ grp }}, y=RelFreq*100000, fill=RelFreq), color="grey50", show.legend = F) +
      geom_label(data=filter(df, is.na(RelFreq)), aes(x={{ grp }}, y=0, label="NA")) +
      scale_fill_gradient(low="lightblue1", high="dodgerblue") +
      labs(x=NULL, y="Tokens/100 000 signs", title=paste("Relative token frequencies for", paste(sort(signs), collapse = " and "))) +
      facet_grid(rows=vars({{ lex }}), cols=vars({{ var }}), scales = scales, space = "free_x") +
      theme(strip.text = element_text(size=18))
    
    if (metric == "relfreq") {
      lf
    } else if (metric == "logodds") {
      lo
    } else {
      lf / lo
    }
  } else {
    data |> 
      filter({{ lex }} %in% signs) |> 
      uncount(n) |> 
      ggplot() +
      geom_hline(yintercept = 0) +
      geom_bar(aes(x={{ grp }}, fill={{ lex }}), position="fill", color="grey50", show.legend = T) +
      scale_fill_manual(values=c(names(palette.colors(length(signs), palette = "Okabe-Ito")))) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(x=NULL, y="% of total tokens", title=paste("Relative token frequencies for", paste(sort(signs), collapse = " and "))) +
      facet_grid(cols=vars({{ var }}), scales = scales, space = "free_x") +
      theme(strip.text = element_text(size=18))
    
  }
  
}


# Example plotting --------------------------------------------------------

# Plot relative frequency
plot_frequencies(agrt, c("SIGN-7452"), var = Variable, grp = Group, lex = Gloss, metric = "relfreq", scales="free")

# Plot log odds 
plot_frequencies(agrt, c("SIGN-7452"), var = Variable, grp = Group, lex = Gloss, metric = "logodds", scales="free")

# Plot relative frequency and log odds
plot_frequencies(agrt, c("SIGN-7452"), var = Variable, grp = Group, lex = Gloss, metric = "both", scales="free")

# Plot relative proportion comparisons between different signs/variants
plot_frequencies(agrt, c("SIGN-479", "SIGN-996"), var = Variable, grp = Group, lex = Gloss, metric = "variants", scales="free")
plot_frequencies(agrt, c("SIGN-5722", "SIGN-578"), var = Variable, grp = Group, lex = Gloss, metric = "variants", scales="free")

