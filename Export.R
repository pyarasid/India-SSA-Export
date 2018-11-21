#load relevant libraries
library(tidyverse)
library(reshape2)
library(readxl)
library(ggplot2)
library(dplyr)
library(scales)

#load excel dataset
india_export <- read_excel("Export-Import.xlsx", sheet= "Export")
View(india_export)

#rename columns
india_export <- india_export %>% rename(country=`Reporter Name`) %>% 
              rename(partner= `Partner Name`) %>% 
              rename(group= `Product Group`) %>% 
              rename(Export= `Trade Flow`)

#converting to tidy data
india_export <- melt(india_export, id.vars =c("country", "Export", "group", "Indicator"),
                     measure.vars = c("1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016"),
                     variable.name = "Year", value.name = "trade_value")

#creating plot of export value
p1 <- ggplot(data = india_export %>% filter(Indicator=="Export (US$ Million)"), mapping = aes(x=Year, y= trade_value)) 
     p1 + geom_line(aes(group=group, color= group), size=1) +geom_point(aes(color=group))+ 
          scale_y_continuous(label=comma) +
         theme(axis.text.x = element_text(angle = 55, hjust=1), axis.text.y = element_text(angle = 55)) +
        labs(x="Years", y="Trade Value (in Million Dollars)", color="Product Group")
     
#creating plots of export share
p2 <- ggplot(data = india_export %>% filter(Indicator=="Export-Share"), mapping = aes(x=Year, y=trade_value))
      p2 + geom_line(aes(group=group, colour=group), size=1)+geom_point(aes(color=group))+
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        theme(axis.text.x = element_text(angle = 55, hjust=1), axis.text.y = element_text(angle=55))+
        labs(x="Years", y="Export Share (in percentage)", color="Product Group")
        
     
     
