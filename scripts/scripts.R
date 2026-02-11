library(tidyverse)

financials <- read_csv("data/raw/Financials.csv")

#1-Changer le noms de certaines variables 
financials %>% 
  rename(Discount_Band = `Discount Band`,
         Units_sold =`Units Sold`,
         Manufacturing_Price = `Manufacturing Price`,
         Sale_price = `Sale Price`,
         Gross_sales = `Gross Sales`,
         Month_Number = `Month Number`,
         Month_Name = `Month Name`
         )
#2-Remove '\$' sign and '-' from all columns where they are present

#columns(Units Sold,Manufacuturing price,sale price, gross sales)

#A- Etudes des variables qualitatives
# Grouper des donnés financiers par segment
segments <- financials %>% group_by(Segment) %>% summarise(n=n())
attach(segments)
#visualiser en diagramme bar la distribution des transactions

ggplot(segments, aes(x=factor(Segment,levels = c("Small Business","Midmarket",
  "Enterprise","Channel Partners",
  "Government")),y=n,fill = Segment)) + 
  geom_col() +
  geom_text(aes(label = n), hjust = 1, colour = "black") +
  labs(title = "Nombre de transactions par segment",
       x = "Segment",
       y = "count")+
  coord_flip()+
  theme_classic()

ggsave("plot/image1.png")
#Ce graphique nous permet de comprendre que c'est le gouvernement qui 
#effectué le plus de transaction
    

# Grouper les donnés financiers par produits 
# visualiser en diagramme bar la distribution des produits
produits <- financials %>% group_by(Product) %>% summarise(n=n())
    
ggplot(produits,aes(Product,n,fill = Product))+
  geom_col() + 
  labs(title = "Fréquence des produits",
       x = "Produit",
       y = "Fréquence")

ggsave("plot/image2.png")
#Ce graphique nous fait comprendre que le produit paseo est le plus commandé