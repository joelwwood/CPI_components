library(tidyverse)
library(cansim)
library(lubridate)
library(cowplot)



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


#### Download seasonally unadjusted CPI data to get sub-component info

cpi_data_na<-get_cansim("18-10-0004-01") %>%
  normalize_cansim_values %>%
  rename(category='Products and product groups',
         cpi=VALUE) %>%
  mutate(date=paste(REF_DATE,"01",sep="-")) %>%
  filter(GEO=="Canada")%>%
  select(date,category,cpi) %>%
  mutate(date=ymd(date)) %>%
  filter(date>"2001-02-01")


### Shelter subcomponents
shelter_cpi<-cpi_data_na %>%
  filter(category %in% c("All-items","Shelter","Rented accommodation","Owned accommodation","Water, fuel and electricity")) %>%
  mutate(         category=ifelse(category=="All-items","All-items CPI",category),
                  category=ifelse(category=="Owned accommodation","Owned housing",category),
                  category=ifelse(category=="Rented accommodation","Rented housing",category),
                  category=ifelse(category=="Water, fuel and electricity","Utilities",category))


#adjust index base to be Mar 2001
shelter_data_base<- shelter_cpi %>%
  filter(date=="2001-03-01") %>%
  rename(base=cpi) %>%
  select(category, base)

shelter_cpi<-shelter_cpi %>%
  mutate(label=if_else(date==max(date),category,NA_character_))%>%
  left_join(shelter_data_base) %>%
  group_by(category) %>%
  mutate(cpi=100*cpi/base) %>%
  ungroup() %>%
  select(-base)

#Shelter Sub-component plot
shelter_plot<-shelter_cpi %>%
  mutate(is_cpi=category=="All-items") %>%
  ggplot(aes(date,cpi,color=category)) +
  geom_line(aes(size=is_cpi))+
  scale_size_manual(values=c(1.3,2))+
  scale_x_date(expand=expansion())+
  theme_minimal_hgrid(14)+ 
  labs(title="Shelter: 2001-2021",
       caption="Data: Statistics Canada Table 18-10-0004-01",
       x=NULL,
       y="Index (March 2001=100)")+
  theme(legend.position="none",
        plot.title = element_text(size=28))

shelter_labels<-shelter_cpi %>%
  filter(date==max(date)) %>%
  mutate(y=cpi) %>%
  select(category,y)

shelter_label_axis<-axis_canvas(shelter_plot,axis="y")+
  geom_text(
    data=shelter_labels,
    aes(y=y, label=category, color=category),
    x=0.05,
    size=4.5,
    hjust=0
  )

p_shelter_labels<-insert_yaxis_grob(shelter_plot, shelter_label_axis)

ggdraw(p_shelter_labels)


## Owned accommodation

owned_cpi<-cpi_data_na %>%
  filter(category %in% c("Owned accommodation","Mortgage interest cost","Homeowners' replacement cost",
                         "Property taxes and other special charges","Homeowners' home and mortgage insurance",
                         "Homeowners' maintenance and repairs","Other owned accommodation expenses")) %>%
  mutate(         category=ifelse(category=="Mortgage interest cost","Mortgage\ninterest cost",category),
                  category=ifelse(category=="Owned accommodation","Owned housing",category),
                  category=ifelse(category=="Homeowners' replacement cost","Home\nreplacement cost",category),
                  category=ifelse(category=="Property taxes and other special charges","Property taxes",category),
                  category=ifelse(category=="Homeowners' home and mortgage insurance","Insurance",category),
                  category=ifelse(category=="Homeowners' maintenance and repairs","Maintenance",category),
                  category=ifelse(category=="Other owned accommodation expenses","Other",category)
                  )


#adjust index base to be Mar 2001
owned_data_base<- owned_cpi %>%
  filter(date=="2001-03-01") %>%
  rename(base=cpi) %>%
  select(category, base)

owned_cpi<-owned_cpi %>%
    left_join(owned_data_base) %>%
  group_by(category) %>%
  mutate(cpi=100*cpi/base) %>%
  ungroup() %>%
  select(-base)

#Owned accommodation Sub-component plot
owned_plot<-owned_cpi %>%
  mutate(is_owned=category=="Owned housing") %>%
  ggplot(aes(date,cpi,color=category)) +
  geom_line(aes(size=is_owned))+
  scale_size_manual(values=c(1.3,2))+
  scale_x_date(expand=expansion())+
  theme_minimal_hgrid(14)+ 
  labs(title="Owned accommodation: 2001-2021",
       caption="Data: Statistics Canada Table 18-10-0004-01",
       x=NULL,
       y="Index (March 2001=100)")+
  theme(legend.position="none",
        plot.title = element_text(size=28))

owned_labels<-owned_cpi %>%
  filter(date==max(date)) %>%
  mutate(y=cpi,
         y=ifelse(category=="Owned housing",162,y),
         y=ifelse(category=="Maintenance",151,y),
         y=ifelse(category=="Home\nreplacement cost",200,y),
         y=ifelse(category=="Other",188,y)) %>%
  select(category,y)

owned_label_axis<-axis_canvas(owned_plot,axis="y")+
  geom_text(
    data=owned_labels,
    aes(y=y, label=category, color=category),
    x=0.05,
    size=4.5,
    hjust=0
  )

p_owned_labels<-insert_yaxis_grob(owned_plot, owned_label_axis)

ggdraw(p_owned_labels)




