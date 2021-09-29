# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

setwd("/home/prakki/Documents/LeaRn/TidyTuesday/2021_week40")
library(tidyverse)
library(tidytuesdayR)
library(dplyr)

# Read in the data manually

papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/papers.csv')
authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/authors.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/programs.csv')
paper_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/paper_authors.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/paper_programs.csv')

papers
authors
programs
paper_authors
paper_programs

library(nberwp)

papers %>% 
  write_csv("papers.csv")

authors %>% 
  write_csv("authors.csv")

programs %>% 
  write_csv("programs.csv")

paper_authors %>% 
  write_csv('paper_authors.csv')

paper_programs %>% 
  write_csv("paper_programs.csv")

joined_df <- left_join(papers, paper_authors) %>% 
  left_join(authors) %>% 
  left_join(paper_programs) %>% 
  left_join(programs)%>% 
  mutate(
    catalogue_group = str_sub(paper, 1, 1),
    catalogue_group = case_when(
      catalogue_group == "h" ~ "Historical",
      catalogue_group == "t" ~ "Technical",
      catalogue_group == "w" ~ "General"
    ),
    .after = paper
  ) 

joined_df
tail(joined_df)

levels(as.factor(joined_df$year))
levels(as.factor(joined_df$catalogue_group))
levels(as.factor(joined_df$month))
levels(as.factor(joined_df$program))
levels(as.factor(joined_df$program_desc))
levels(as.factor(joined_df$program_category))
levels(as.factor(joined_df$author))
levels(as.factor(joined_df$user_nber))
levels(as.factor(joined_df$user_repec))

# How many publications per year?

## Table
joined_df %>% count(year, sort=TRUE)

## Plot
joined_df %>% count(year, sort=TRUE) %>% 
  ggplot(aes(x=factor(year),y=n)) +
  geom_bar(stat='identity') + 
 # ggthemes::theme_excel_new() +
  labs(x="Year",
       y="Count",
       title = "Number of publications per year")

ggsave("publication_per_year.png",height=10,width=20, unit="in")

# How many publications per year by catalogue group

## Table
joined_df %>% count(year,catalogue_group, sort=TRUE) 

## Plot
joined_df %>% count(year,catalogue_group, sort=TRUE) %>% 
  ggplot(aes(x=factor(year),y=n)) +
  geom_bar(stat='identity') +
  geom_col(aes(fill = catalogue_group))+
  labs(x="Year",
       y="Count",
       title = "Number of publications per year by catalogue group")

ggsave("publication_per_year_by_catalogue_group.png",height=10,width=20, unit="in")

# How many publications per year by program

## Table
joined_df %>% count(year,program, sort=TRUE) 

## Plot
joined_df %>% count(year,program, sort=TRUE) %>% 
  ggplot(aes(x=factor(year),y=n)) +
  geom_bar(stat='identity') +
  geom_col(aes(fill = program))+
  ggthemes::theme_excel_new() +
  labs(x="Year",
       y="Count",
       title = "Number of publications per year by program")

ggsave("publication_per_year_by_program_stacked.png",height=10,width=20, unit="in")

# Above plot, cannot clearly say which year has number of publications. Let's try facet_wrap

joined_df %>% count(year,program, sort=TRUE) %>% 
  ggplot(aes(x=factor(year),y=n)) +
  geom_bar(stat='identity') +
  geom_col(aes(fill = program))+
  facet_wrap(.~program,scales = "free_x") +
  ggthemes::theme_solarized()+
  labs(x="Year",
       y="Count",
       title = "Number of publications per year by program") +
  theme(
  #  panel.grid.major = element_blank(),
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 6
    ),
    axis.text.y = element_text(
      angle = 0,
      hjust = 1,
      size = 7
    ),
    legend.position = "none",
    strip.text.y.right = element_text(angle = 0),
    strip.text.x.top = element_text(angle = 0)
  )

ggsave("publication_per_year_by_program_facet.png",height=10,width=18, unit="in")

# How many authors have X number of publications?

## Table
joined_df %>% count(user_nber, sort=TRUE) %>% group_by(n) %>% summarise(author_count = n())

# joined_df %>% count(user_nber, sort=TRUE) %>% filter(n==103)

# Plot
joined_df %>% count(user_nber, sort=TRUE) %>% group_by(n) %>% summarise(author_count = n()) %>% 
  ggplot(aes(x=factor(n),y=author_count)) +
  geom_bar(stat='identity') +
  ggthemes::theme_solarized() +
  labs(x="Number of Publications",
       y="Count of authors",
       title = "Count of authors with \"X\" number of publications") +
  theme(
      panel.grid.major = element_blank(),
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 6
    ),
    axis.text.y = element_text(
      angle = 0,
      hjust = 1,
      size = 7
    ),
    legend.position = "none",
    strip.text.y.right = element_text(angle = 0),
    strip.text.x.top = element_text(angle = 0)
  )

ggsave("publication_counts.png",height=6,width=14, unit="in")


# Which author had most number of publications

## Table
joined_df %>% count(user_nber, sort=TRUE) %>%  head(n = 50) %>%  tail()

# Plot 2

joined_df %>% count(user_nber, sort=TRUE) %>% head(n = 50) %>% filter(user_nber!="NA") %>% 
ggplot(aes(x=reorder(factor(user_nber),n),y=n, fill = user_nber, label = n)) +
  geom_bar(stat="identity", show.legend = FALSE) +
  coord_flip() +
  labs(x="Author", y="Count of Publications",
       title = "Top 50 authors with highest count of publications") +
  geom_label(aes(fill = user_nber),colour = "white", fontface = "bold", show.legend = FALSE) 

ggsave("author_with_mostpubs.png",height=10,width=9, unit="in")


# Which months had more number of publications per year

## Table
joined_df %>% count(year,month, sort=TRUE) 

# Plot 2

joined_df %>% count(year, month, sort=TRUE) %>% 
  ggplot(aes(x=factor(month),y=n)) +
  geom_bar(stat='identity') +
  geom_col(aes(fill = factor(year)))+
  facet_wrap(.~year,scales = "free_x") +
  ggthemes::theme_solarized()+
  labs(x="Year",
       y="Count",
       title = "Number of publications per month for each year") +
  theme(
   # panel.grid.major = element_blank(),
    axis.text.x = element_text(
      angle = 0,
      hjust = 1,
      size = 6
    ),
    axis.text.y = element_text(
      angle = 0,
      hjust = 1,
      size = 7
    ),
    legend.position = "none",
    strip.text.y.right = element_text(angle = 0),
    strip.text.x.top = element_text(angle = 0)
  )

ggsave("months_mostpubs_facet.png",height=10,width=18, unit="in")

# The above  does not obviously tell us which month had more publications

## Table
joined_df %>% count(month, sort=TRUE) 

# Plot 2
joined_df %>% count(month, sort=TRUE) %>%  
  ggplot(aes(x=reorder(factor(month),n),y=n, fill = factor(month), label = n)) +
  geom_bar(stat="identity", show.legend = FALSE) +
  #coord_flip() +
  labs(x="Month", y="Count of Publications",
       title = "Number of publications per month for each year") +
  geom_label(aes(fill = factor(month)),colour = "white", fontface = "bold", show.legend = FALSE) 

ggsave("months_mostpubs_bar.png",height=3,width=9, unit="in")
