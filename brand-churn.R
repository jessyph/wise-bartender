library(tidyverse)
library(zoo)
library(grid)
library(gridExtra)

d <- read.csv("~/Downloads/wise-bartender-six.csv")

#Create dataset of just beers
beer <- d %>%
  #mutate(ProductName = gsub("\\(..*", "", ProductName)) %>%
  filter(!str_detect(ProductName, "Wise Pack"),
         !str_detect(ProductName, "Book"),
         !str_detect(ProductName, "Loyalty Points"),
         !str_detect(ProductName, "Refunded"),
         !str_detect(ProductName, "Gift Pack"),
         !str_detect(ProductName, "Advent Calendar"),
         !str_detect(ProductName, "Spirit"),
         !str_detect(ProductName, "Wine"),
         !str_detect(ProductName, "Kombucha"),
         !str_detect(ProductName, "Cider"),
         !str_detect(ProductName, "Sparkling"),
         !str_detect(ProductName, "Seedlip"),
         !str_detect(ProductName, "Tonic"),
         !str_detect(ProductName, "Chocolate"),
         !str_detect(ProductName, "Cocktail"),
         !str_detect(ProductName, "Everleaf"),
         !str_detect(ProductName, "Caleno"),
         !str_detect(ProductName, "Square Root"),
         !str_detect(ProductName, "Aecorn"),
         !str_detect(ProductName, "Punchy"),
         !str_detect(ProductName, "Chillio"),
         !str_detect(ProductName, "Olives"),
         !str_detect(ProductName, "Nocktail"),
         !str_detect(ProductName, "Savyll"),
         !str_detect(ProductName, "Westcombe"),
         !str_detect(ProductName, "Popcorn"),
         !str_detect(ProductName, "Twisst"),
         !str_detect(ProductName, "Bag"),
         !str_detect(ProductName, "Gift Cards"),
         !str_detect(ProductName, "Sea & T"),
         !str_detect(ProductName, "The Bitter Note"),
         !str_detect(ProductName, "Salcombe"),
         !str_detect(ProductName, "Franklin"),
         !str_detect(ProductName, "Wilfreds"),
         !str_detect(ProductName, "Lyre's"),
         !str_detect(ProductName, "Highball"),
         !str_detect(ProductName, "Kolibri"),
         !str_detect(ProductName, "Kopparberg"),
         !str_detect(ProductName, "DRGN"),
         !str_detect(ProductName, "Ish"),
         !str_detect(ProductName, "Borrago"),
         !str_detect(ProductName, "GIMBER"),
         !str_detect(ProductName, "Sea Arch"),
         !str_detect(ProductName, "Botonique"),
         !str_detect(ProductName, "Silk Tree"),
         !str_detect(ProductName, "spirit"),
         !str_detect(ProductName, "Torres"),
         !str_detect(ProductName, "Aperitif"),
         !str_detect(ProductName, "Prosecco"),
         !str_detect(ProductName, "Hardys"),
         !str_detect(ProductName, "Westons"),
         !str_detect(ProductName, "Stryyk"),
         !str_detect(ProductName, "Xachoh"),
         !str_detect(ProductName, "Pimento"),
         !str_detect(ProductName, "Belle"),
         str_detect(ProductName, ""))

beer$ProductName <- gsub("[^[:alnum:]///' ]", "", beer$ProductName) #Remove unknown characters

#Create new column with PackSize and clean remaining product names
beer <- beer %>%
  separate(ProductName, c("ProductName", "PackSize"), sep = "Pack Size", fill = "right") %>%
  mutate(ProductName = gsub("0..*", "", ProductName))

#Clean PackSize
beer <- beer %>%
  mutate(PackSize = gsub("Single..*", "1", PackSize)) %>%
  mutate(PackSize = gsub("6..*", "6", PackSize)) %>%
  mutate(PackSize = gsub("12..*", "12", PackSize)) %>%
  mutate(PackSize = gsub("24..*", "24", PackSize)) %>%
  mutate(PackSize = gsub("Case of 6", "6", PackSize))

#Turn characters into numbers 
beer$PackSize <- as.numeric(as.character(beer$PackSize))

#Find total amount ordered
beer <- beer %>%
  mutate(total = PackSize * ProductQuantity)

#Find most popular beers
popularbeer <- beer %>% 
  count(ProductName, wt = total) %>% 
  summarise(ProductName, n) %>%
  arrange(desc(n)) %>%
  top_n(20)

#Filter the most popular beers from the main df
fullpopularbeer <- beer %>%
  filter(ProductName %in% popularbeer$ProductName)

#Extract date from order number
fullpopularbeer <- fullpopularbeer %>%
  mutate(date = str_extract(OrderNumber, "/[:digit:]+/")) %>% 
  mutate(date = gsub("/","", date)) %>%
  mutate(date = as.Date(date, "%d%m%y")) %>%
  summarise(ProductName, total, date)

#Perday
fullpopularbeer <- fullpopularbeer %>%
  count(ProductName, date, wt = total) %>%
  arrange(date)

#Calculate ten day rolling average of products
rolling <- fullpopularbeer %>%
  group_by(ProductName) %>%
  mutate(average = rollmean(n, k = 10, fill = NA))

rolling$ProductName <- str_trim(rolling$ProductName, side = c("right"))

#Create list of products
products = unique(rolling$ProductName)

beer.plots = list()
for(i in 1:length(products)) {
  
  plot.data = rolling %>% filter(ProductName == products[i])
  
  beer.plots[[i]] = ggplot(data = plot.data, aes(x = date, y = average)) +
    geom_bar(stat = "identity",
             width = 0.1,
             colour = "red") +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5)
    ) +
    scale_x_date(name = "Date", 
                 expand = c(0, 0)) +
    scale_y_continuous(name = "Average sales",
                       expand = c(0, 0)) +
    labs(title = products[i])
  
  beer.plots[[i]] = ggplotGrob(beer.plots[[i]])
  
}

plot1 <- beer.plots[[1]]
plot2 <- beer.plots[[2]]
plot3 <- beer.plots[[3]]
plot4 <- beer.plots[[4]]
plot5 <- beer.plots[[5]]
plot6 <- beer.plots[[6]]
plot7 <- beer.plots[[7]]
plot8 <- beer.plots[[8]]
plot9 <- beer.plots[[9]]
plot10 <- beer.plots[[10]]
plot11 <- beer.plots[[11]]
plot12 <- beer.plots[[12]]
plot13 <- beer.plots[[13]]
plot14 <- beer.plots[[14]]
plot15 <- beer.plots[[15]]
plot16 <- beer.plots[[16]]
plot17 <- beer.plots[[17]]
plot18 <- beer.plots[[18]]
plot19 <- beer.plots[[19]]
plot20 <- beer.plots[[20]]

grid.arrange(plot1, plot2, plot3, plot4, plot5, 
             plot6, plot7, plot8, plot9, plot10,
             plot11, plot12, plot13, plot14, plot15,
             plot16, plot17, plot18, plot19, plot20, ncol = 4)
