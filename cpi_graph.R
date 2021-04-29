library(tidyverse)
library(cansim)
library(lubridate)
library(cowplot)
library(ggrepel)


cpi_data_raw<-get_cansim("18-10-0006-01") %>%
  normalize_cansim_values

cpi_data<-cpi_data_raw %>%
  rename(category='Products and product groups',
         cpi=VALUE) %>%
  mutate(date=paste(REF_DATE,"01",sep="-"))%>%
  select(date,category,cpi) %>%
  mutate(date=ymd(date)) %>%
  filter(date>"2001-02-01") %>%
  filter(!category %in% c("All-items excluding food","All-items excluding food and energy"))


cpi_data_base<- cpi_data %>%
  filter(date=="2001-03-01") %>%
  rename(base=cpi) %>%
  select(category, base)

cpi_data<-cpi_data %>%
  mutate(label=if_else(date==max(date),category,NA_character_))%>%
  left_join(cpi_data_base) %>%
  group_by(category) %>%
  mutate(cpi=100*cpi/base) %>%
  ungroup() %>%
  select(-base)

cpi_plot<-cpi_data %>%
  mutate(is_cpi=category=="All-items") %>%
  ggplot(aes(date,cpi,color=category)) +
  geom_line(aes(size=is_cpi))+
  scale_size_manual(values=c(1.3,2))+
  scale_x_date(expand=expansion())+
  theme_minimal_hgrid(14)+ 
    labs(title="Consumer Price Index Categories: 2001-2021",
       caption="Data: Statistics Canada Table 18-10-0006-01",
       x=NULL,
       y="Index (March 2001=100)")+
  theme(legend.position="none",
        plot.title = element_text(size=28))

direct_labels<-cpi_data %>%
  filter(date==max(date)) %>%
  mutate(y=cpi,
         category=ifelse(category=="Household operations, furnishings and equipment","Household stuff",category),
         category=ifelse(category=="Health and personal care","Health",category),
         category=ifelse(category=="Alcoholic beverages, tobacco products and recreational cannabis","Alcohol, tobacco\n cannabis",category),
         category=ifelse(category=="Recreation, education and reading","Recreation\n & education",category),
         category=ifelse(category=="Clothing and footwear","Clothing",category),
         category=ifelse(category=="All-items","All-items CPI",category),
         y=ifelse(category=="Transportation",155,y),
         y=ifelse(category=="Shelter",150,y),
         y=ifelse(category=="Food",163,y)) %>%
  select(category,y)

direct_label_axis<-axis_canvas(cpi_plot,axis="y")+
  geom_text(
    data=direct_labels,
    aes(y=y, label=category, color=category),
    x=0.05,
    size=4.5,
    hjust=0
  )

p_direct_labels<-insert_yaxis_grob(cpi_plot, direct_label_axis)

ggdraw(p_direct_labels)


  
 


