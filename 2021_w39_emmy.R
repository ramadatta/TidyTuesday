library(tidyverse)
library(ggtext)
library(extrafont)
library(Cairo)
library(gridExtra)
library(grid)
library(gridtext)
library(patchwork)
library(RColorBrewer)

#extrafont::loadfonts(device = "pdf")
theme_set(theme_minimal())

sysfonts::font_add_google('Fira Sans')
# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!


# Getting the data --------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2021-09-21')
head(tuesdata)

#tuesdata <- tidytuesdayR::tt_load(2021, week = 39)

nominees <- tuesdata$nominees
head(nominees)


nominees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-21/nominees.csv')
view(nominees)

# Plot winner to nominee ratio

unique(nominees$distributor)
unique(nominees$type)
unique(nominees$producer)
unique(nominees$production)
unique(nominees$title)
unique(nominees$category)

df_nw <- nominees %>% group_by(year, type) %>% 
  summarise(Count = n()) #%>% pivot_wider(names_from = type, values_from = n)

df_nw  

ggplot(df_nw, aes(x=Count, y=factor(year))) + 
  geom_line(aes(group = year))+
  geom_point(aes(color=type), size=1) +
  theme_light()+
  theme(legend.position="top", axis.text.x = element_text(angle = 90, hjust = 1, size = 6), axis.text.y = element_text(angle = 0, hjust = 1, size = 5)) +
  scale_color_brewer(palette="Dark2", direction=-1) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  labs(x="Count",
       y="Year",
       title = "Trend of nominees vs winner from 1957 to 2021", 
       caption="Data from emmys.com | Graphic by: Prakki Sai Rama Sridatta for #TidyTuesday")

# Let's reorder the year by higher counts

ggplot(df_nw, aes(x=Count, y=reorder(year,Count))) + 
  geom_line(aes(group = year))+
  geom_point(aes(color=type), size=1) +
  theme_light()+
  theme(legend.position="top", axis.text.x = element_text(angle = 90, hjust = 1, size = 6), axis.text.y = element_text(angle = 0, hjust = 1, size = 5)) +
  scale_color_brewer(palette="Dark2", direction=-1) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  labs(x="Count",
       y="Year",
       title = "Trend of nominees vs winner from 1957 to 2021", 
       caption="Data from emmys.com | Graphic by: Prakki Sai Rama Sridatta for #TidyTuesday")


df_gt2010 <- nominees %>% filter(year <= 2001)

nb.cols <- length(unique(df_gt2010$distributor))
nb.cols
customPal <- colorRampPalette(brewer.pal(8, "Accent"))(nb.cols) # Set2, Accent, Paired


ggplot(df_gt2010,
       aes(year, distributor, fill = distributor)) +
  geom_tile(colour = "white", size = 0.1) +
  #facet_grid(category ~ Species_ST, scales = "free", space = "free") +
  labs(
    x = "Sample",
    y = "Gene nomenclature",
    title = "Resistance Gene Heatmap (from Resfinder)",
    subtitle = "",
    fill = "Gene Classification"
  ) +
  scale_fill_manual(values = customPal) + #Paired and Set3
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
    strip.text.x.top = element_text(angle = 90)
  )

# Too much information. Lets clean the data a bit

df_gt2010 <- df_gt2010 %>% mutate(distributor2 = case_when(grepl("HBO", distributor) ~ "HBO",
                                       grepl("NBC", distributor) ~ "NBC",
                                       grepl("ABC", distributor) ~ "ABC",
                                       grepl("CBS", distributor) ~ "CBS",
                                       grepl("Apple", distributor) ~ "Apple",
                                       TRUE ~  as.character(distributor))
                     )

df_gt2010 

#Let's Replot
ggplot(df_gt2010,
       aes(year, distributor2, fill = distributor)) +
  geom_tile(colour = "white", size = 0.1) +
  facet_grid(category ~ type, scales = "free", space = "free") +
  labs(
    x = "Sample",
    y = "Gene nomenclature",
    title = "Resistance Gene Heatmap (from Resfinder)",
    subtitle = "",
    fill = "Gene Classification"
  ) +
  scale_fill_manual(values = customPal) + #Paired and Set3
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
    strip.text.x.top = element_text(angle = 90)
  )

view(df_gt2010)
# 
# #Data for plotting
# distrib <- nominees %>% count(distributor, type, year)
# distrib <- spread(distrib, type, n)
# 
# #Data for analyzing
# total_ct <- nominees %>% count(distributor)
# type_ct <- nominees %>% count(distributor, type)
# 
# #Plot theme
# plottheme <- theme(
#   panel.grid.minor=element_blank(),
#   plot.margin=margin(0.5,0.5,0.5,0.5,unit="cm"),
#   text=element_text(family="Helvetica"),
#   plot.title=element_markdown(size=22, lineheight=1.2),
#   plot.subtitle=element_text(size=20, face="bold"),
#   axis.title=element_blank(),
#   axis.text=element_text(size=16, color="black"))
# 
# #HBO
# hbo <- distrib %>% filter(distributor=="HBO") %>%
#   ggplot(aes(x=year)) + 
#   geom_bar(aes(y=Winner), stat="identity", fill="#820B8A") +
#   geom_bar(aes(y=-Nominee), stat="identity", fill="#ADFFE8") +
#   scale_y_continuous(limits=c(-300,100), breaks=c(-300, -200, -100, 0, 100), labels=c(300,200,100,0,100)) +
#   scale_x_continuous(limits=c(1952,2026), breaks=c(1960,1970,1980,1990,2000,2010,2020), labels=c(1960,1970,1980,1990,2000,2010,2020)) +
#   labs(
#     title="<b>Networks & Platforms' Emmy <span style='color:#820B8A;'>Wins</span> & <span style='color:#ADFFE8;'>Nominations</span></b> <br>
#     Distributors with the most Emmy recognitions <br>",
#     subtitle="HBO - 4,558 total (nominations & wins combined)"
#   ) +
#   theme_minimal() +
#   plottheme
# 
# print(hbo)
# 
# #NBC
# nbc <- distrib %>% filter(distributor=="NBC") %>%
#   ggplot(aes(x=year)) + 
#   geom_bar(aes(y=Winner), stat="identity", fill="#820B8A") +
#   geom_bar(aes(y=-Nominee), stat="identity", fill="#ADFFE8") +
#   scale_y_continuous(limits=c(-300,100), breaks=c(-300, -200, -100, 0, 100), labels=c(300,200,100,0,100)) +
#   scale_x_continuous(limits=c(1952,2026), breaks=c(1960,1970,1980,1990,2000,2010,2020), labels=c(1960,1970,1980,1990,2000,2010,2020)) +
#   labs(
#     subtitle="NBC - 3,683 total"
#   ) +
#   theme_minimal() +
#   plottheme
# 
# print(nbc)
# 
# #CBS
# cbs <- distrib %>% filter(distributor=="CBS") %>%
#   ggplot(aes(x=year)) + 
#   geom_bar(aes(y=Winner), stat="identity", fill="#820B8A") +
#   geom_bar(aes(y=-Nominee), stat="identity", fill="#ADFFE8") +
#   scale_y_continuous(limits=c(-300,100), breaks=c(-300, -200, -100, 0, 100), labels=c(300,200,100,0,100)) +
#   scale_x_continuous(limits=c(1952,2026), breaks=c(1960,1970,1980,1990,2000,2010,2020), labels=c(1960,1970,1980,1990,2000,2010,2020)) +
#   labs(
#     subtitle="CBS - 3,002 total"
#   ) +
#   theme_minimal() +
#   plottheme
# 
# print(cbs)
# 
# #ABC
# abc <- distrib %>% filter(distributor=="ABC") %>%
#   ggplot(aes(x=year)) + 
#   geom_bar(aes(y=Winner), stat="identity", fill="#820B8A") +
#   geom_bar(aes(y=-Nominee), stat="identity", fill="#ADFFE8") +
#   scale_y_continuous(limits=c(-300,100), breaks=c(-300, -200, -100, 0, 100), labels=c(300,200,100,0,100)) +
#   scale_x_continuous(limits=c(1952,2026), breaks=c(1960,1970,1980,1990,2000,2010,2020), labels=c(1960,1970,1980,1990,2000,2010,2020)) +
#   labs(
#     subtitle="ABC - 2,962 total"
#   ) +
#   theme_minimal() +
#   plottheme
# 
# print(abc)
# 
# #Netflix
# net <- distrib %>% filter(distributor=="Netflix") %>%
#   ggplot(aes(x=year)) + 
#   geom_bar(aes(y=Winner), stat="identity", fill="#820B8A") +
#   geom_bar(aes(y=-Nominee), stat="identity", fill="#ADFFE8") +
#   scale_y_continuous(limits=c(-300,100), breaks=c(-300, -200, -100, 0, 100), labels=c(300,200,100,0,100)) +
#   scale_x_continuous(limits=c(1952,2026), breaks=c(1960,1970,1980,1990,2000,2010,2020), labels=c(1960,1970,1980,1990,2000,2010,2020)) +
#   labs(
#     subtitle="Netflix - 2,144 total",
#     caption="Data from emmys.com | Graphic by Ilena Peng for #TidyTuesday"
#   ) +
#   theme_minimal() +
#   plottheme
# 
# print(net)
# 
# hbo / nbc / cbs / abc / net
# ggsave("w39_emmys.png",height=15,width=9, unit="in")
