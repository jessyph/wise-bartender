library(tidyverse)
library(showtext)

d <- read.csv("~/Downloads/wise-bartender-six.csv")

font_add_google(name ="Open Sans", family = "open")
showtext_auto()

#Get list of products sold
product <- d %>%
  select(ProductName)

#Select unique products sold
unique <- product %>%
  distinct(ProductName)

#clean <- unique %>%
#  mutate(ProductName = gsub("\\(..*", "", ProductName),  #Remove anything after a bracket
#         ProductName = gsub("Alcohol Free*", "", ProductName), #Remove anything after 'Alcohol Free'
 #        ProductName = gsub("Pre Order", "", ProductName), #Remove 'Pre Order'
#         ProductName = gsub("(Big Drop).*", "\\1", ProductName),
#         ProductName = gsub("(Drop Bear).*", "\\1", ProductName),
#         ProductName = gsub("(Lyre's).*", "\\1", ProductName),
#         ProductName = gsub("(Seedlip).*", "\\1", ProductName),
#         ProductName = gsub("(Harviestoun).*", "\\1", ProductName),
#         ProductName = gsub("(Stryyk).*", "\\1", ProductName),
#         ProductName = gsub("(Infinite Session).*", "\\1", ProductName),
#         ProductName = gsub("(Coast).*", "\\1", ProductName),
#         ProductName = gsub("(Mikeller).*", "\\1", ProductName),
#         ProductName = gsub("(Square Root).*", "\\1", ProductName),
#         ProductName = gsub("(St Peter's).*", "\\1", ProductName),
#         ProductName = gsub("(Nirvana).*", "\\1", ProductName),
#         ProductName = gsub("(Clausthaler).*", "\\1", ProductName),
#         ProductName = gsub("(Real Kombucha).*", "\\1", ProductName),
#         ProductName = gsub("(Left Field Kombucha).*", "\\1", ProductName),
#         ProductName = gsub("(Wild Life).*", "\\1", ProductName),
#         ProductName = gsub("(Highball).*", "\\1", ProductName),
#         ProductName = gsub("(Smashed).*", "\\1", ProductName),
#         ProductName = gsub("(Sipling).*", "\\1", ProductName),
#         ProductName = gsub("(Aecorn).*", "\\1", ProductName),
#         ProductName = gsub("(Thornbridge).*", "\\1", ProductName),
#         ProductName = gsub("(Adnams).*", "\\1", ProductName),
#         ProductName = gsub("(Double Dutch).*", "\\1", ProductName),
#         ProductName = gsub("(Kopparberg).*", "\\1", ProductName),
#         ProductName = gsub("(Twelve Below).*", "\\1", ProductName),
#         ProductName = gsub("(Chillio).*", "\\1", ProductName),
#         ProductName = gsub("(Jeffrey's).*", "\\1", ProductName),
#         ProductName = gsub("(Clausthaler).*", "\\1", ProductName),) %>%
#  filter(!str_detect(ProductName, "Wise Pack"),
#         !str_detect(ProductName, "Book"),
#         !str_detect(ProductName, "Loyalty Points"),
#         !str_detect(ProductName, "Refunded"))

#Apply the cleaning to the full list 
brandProduct <- product %>%
  mutate(ProductName = gsub("\\(..*", "", ProductName),  #Remove anything after a bracket
         ProductName = gsub("Alcohol Free*", "", ProductName), #Remove anything after 'Alcohol Free'
         ProductName = gsub("Pre Order", "", ProductName), #Remove 'Pre Order'
         ProductName = gsub("(Big Drop).*", "\\1", ProductName),
         ProductName = gsub("(Drop Bear).*", "\\1", ProductName),
         ProductName = gsub("(Lyre's).*", "\\1", ProductName),
         ProductName = gsub("(Seedlip).*", "\\1", ProductName),
         ProductName = gsub("(Harviestoun).*", "\\1", ProductName),
         ProductName = gsub("(Stryyk).*", "\\1", ProductName),
         ProductName = gsub("(Infinite Session).*", "\\1", ProductName),
         ProductName = gsub("(Coast).*", "\\1", ProductName),
         ProductName = gsub("(Mikeller).*", "\\1", ProductName),
         ProductName = gsub("(Square Root).*", "\\1", ProductName),
         ProductName = gsub("(St Peter's).*", "\\1", ProductName),
         ProductName = gsub("(Nirvana).*", "\\1", ProductName),
         ProductName = gsub("(Clausthaler).*", "\\1", ProductName),
         ProductName = gsub("(Real Kombucha).*", "\\1", ProductName),
         ProductName = gsub("(Left Field).*", "\\1", ProductName),
         ProductName = gsub("(Wild Life).*", "\\1", ProductName),
         ProductName = gsub("(Highball).*", "\\1", ProductName),
         ProductName = gsub("(Smashed).*", "\\1", ProductName),
         ProductName = gsub("(Sipling).*", "\\1", ProductName),
         ProductName = gsub("(Aecorn).*", "\\1", ProductName),
         ProductName = gsub("(Thornbridge).*", "\\1", ProductName),
         ProductName = gsub("(Adnams).*", "\\1", ProductName),
         ProductName = gsub("(Double Dutch).*", "\\1", ProductName),
         ProductName = gsub("(Kopparberg).*", "\\1", ProductName),
         ProductName = gsub("(Twelve Below).*", "\\1", ProductName),
         ProductName = gsub("(Chillio).*", "\\1", ProductName),
         ProductName = gsub("(Jeffrey's).*", "\\1", ProductName),
         ProductName = gsub("(Clausthaler).*", "\\1", ProductName),
         ProductName = gsub("(Ambar 0.0).*", "\\1", ProductName),
         ProductName = gsub("(Darling Cellars).*", "\\1", ProductName),
         ProductName = gsub("(Lucky Saint).*", "\\1", ProductName),
         ProductName = gsub("(Beavertown).*", "\\1", ProductName),
         ProductName = gsub("(Brutal Brewing).*", "\\1", ProductName),
         ProductName = gsub("(Ariel).*", "\\1", ProductName),
         ProductName = gsub("(Twisst).*", "\\1", ProductName)) %>%
  filter(!str_detect(ProductName, "Wise Pack"),
         !str_detect(ProductName, "Book"),
         !str_detect(ProductName, "Loyalty Points"),
         !str_detect(ProductName, "Refunded"),
         !str_detect(ProductName, "Gift Pack"),
         !str_detect(ProductName, "Advent Calendar"),
         str_detect(ProductName, ""))

popularBrand <- brandProduct %>% 
  group_by(ProductName) %>% 
  count() %>% 
  summarise(ProductName, n) %>%
  arrange(desc(n)) %>%
  top_n(20)

ggplot(data = popularBrand, aes(x = n, y = reorder(ProductName, n), fill = reorder(ProductName,n))) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(
    axis.text = element_text(family = "open"),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(family = "open"),
    legend.position = "none",
    plot.subtitle = element_text(family = "open",
                              hjust = 0.5),
    plot.title = element_text(family = "open",
                              hjust = 0.5),
  ) +
  scale_y_discrete(name = "Brand",
                     expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0, 2250)) +
  labs(title = "Most popular brands ordered from Wise Bartender",
       subtitle = "Over past 6 months")

ggsave("mostpopularwisebartender.png", dpi = 300, height = 4, width = 8, units = "in")  

