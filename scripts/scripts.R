library(tidyverse)
library(ggthemes)
library(corrplot)
library(lubridate)
#library(plotly)

financials <- read_csv("data/raw/Financials.csv")

#1-Changer le noms de certaines variables 
#2-Remove '\$' sign and '-' from all columns where they are present
financials_detail <- financials %>% 
  rename(Discount_Band = `Discount Band`,
         Units_sold =`Units Sold`,
         Manufacturing_Price = `Manufacturing Price`,
         Sale_price = `Sale Price`,
         Gross_sales = `Gross Sales`,
         Month_Number = `Month Number`,
         Month_Name = `Month Name`
         ) %>% 
  mutate(across(c(Units_sold,Manufacturing_Price,Discounts,Sale_price,Gross_sales,
        Sales,COGS,Profit),parse_number))


#la matrice de corrélation affiche la relation entre les variables

matrice_cor <- cor(financials_detail[, sapply(financials_detail, is.numeric)],
                   use = "complete.obs")
corrplot(matrice_cor, type = "upper", method = "square", 
         addCoef.col = "black", tl.col = "black", tl.srt = 45)


#columns(Units Sold,Manufacuturing price,sale price, gross sales)

#A- Etudes des variables qualitatives
#Grouper des donnés financiers par segment
segments <- financials_detail %>% group_by(Segment) %>% summarise(n=n())
attach(segments)
#visualiser en diagramme bar la distribution des transactions

ggplot(segments, aes(x=factor(Segment),y=n,fill = Segment)) + 
  geom_col() +
  geom_text(aes(label = n), hjust = 1, colour = "black") +
  labs(title = "Nombre de transactions par secteur de marché",
       x = "Secteurs",
       y = "Total",
       caption = "Data source: Financials Dataset")+
  coord_flip()+
  theme_fivethirtyeight()

ggsave("plot/image1.png")
#Ce graphique nous permet de comprendre que c'est le gouvernement qui 
#effectué le plus de transaction
    

# Grouper les donnés financiers par produits 
# visualiser en diagramme bar la distribution des produits
produits <- financials_detail %>% group_by(Product) %>% summarise(n=n())
    
ggplot(produits,aes(Product,n,fill = Product))+
  geom_col(linewidth = 2) +
  geom_text(aes(label = n),vjust = 1,color = "black")+
  scale_fill_manual(values = c("#7575F5", "#F57575","#F57575", "#41B7C4","#41B7D8","#41B7D8"))+
  annotate("text",x=5.5,y=190,label = "Total : 700")+
  labs(title = "Quantité des produits vendues",
       x = "Produits",
       y = "quantité")+
  theme_fivethirtyeight()

ggsave("plot/image2.png")
#Ce graphique nous fait comprendre que le produit paseo est le plus commandé

#B- Etudes des variables quantitatives

#Visualisation des produits de fabrication en fonct

financials_detail %>%
  filter(Product == "Paseo") %>%
  group_by(Date) %>%
  summarize(Units_sold = sum(Units_sold, na.rm = TRUE)) %>%
  ggplot(aes(x = dmy(Date), y = Units_sold)) +
  geom_line(color = "black") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) +
  labs(x = "Date",
       y = "Unit Sold",
       title = "Evolution du Chiffre d'affaire du produit Paseo") +
  theme_fivethirtyeight()

ggsave("plot/image3.png")

financials_detail %>%
  filter(Product == "Carretera") %>%
  group_by(Date) %>%
  summarize(Units_sold = sum(Units_sold, na.rm = TRUE)) %>%
  ggplot(aes(x = dmy(Date), y = Units_sold)) +
  geom_line(color = "black") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) +
  labs(x = "Date",
       y = "Unit Sold",
       title = "Evolution du Chiffre d'affaire du produit Carretera") +
  theme_fivethirtyeight()
  
ggsave("plot/image4.png")

