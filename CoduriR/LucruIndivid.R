install.packages("readxl")
install.packages("writexl")

library(readxl)
library(openintro)
library(tidyverse)
setwd("U:/Univer/Anul 3/Analiza Datelor")

client <- read_excel("E Commerce Dataset.xlsx")

glimpse(client)
summary(client)

# numarul de clienti pierduti/ramasi
client %>%
count(Churn)

client |> count(PreferredLoginDevice)

client<- client |> mutate(PreferredLoginDevice=ifelse(PreferredLoginDevice=='Phone','Mobile Phone',PreferredLoginDevice))

client |> count(PreferredLoginDevice)
client |> count(PreferedOrderCat)
client<- client |> mutate(PreferedOrderCat=ifelse(PreferedOrderCat=='Mobile','Mobile Phone',PreferedOrderCat))
client |> count(PreferedOrderCat)

client |> count(PreferredPaymentMode)

client<- client |> mutate(PreferredPaymentMode=case_when(PreferredPaymentMode=="CC"~'Credit Card',
                                                         PreferredPaymentMode=="COD"~'Cash on Delivery',
                                                         .default = PreferredPaymentMode))

client |> count(PreferredPaymentMode)
client<-client |> mutate(Churn=if_else(Churn==1,'Pierdut','Loial'))
glimpse(client)

client |> count(Churn)
densT <- ggplot(client, aes(x=Tenure, color=Churn)) +
  geom_density()

densW <- ggplot(client, aes(x=WarehouseToHome, color=Churn)) +
  geom_density()
install.packages("patchwork")
library(patchwork)

d3<-ggplot(client, aes(x=HourSpendOnApp, color=Churn)) +
  geom_density()

d4<-ggplot(client, aes(x=OrderAmountHikeFromlastYear, color=Churn)) +
  geom_density()

d5<-ggplot(client, aes(x=CouponUsed, color=Churn)) +
  geom_density()

d6<-ggplot(client, aes(x=OrderCount, color=Churn)) +
  geom_density()
d7<-ggplot(client, aes(x=DaySinceLastOrder, color=Churn)) +
  geom_density()
d8<-ggplot(client, aes(x=CashbackAmount, color=Churn)) +
  geom_density()
d9<-ggplot(client, aes(x=Complain, color=Churn)) +
  geom_density()
denstot <- densT+densW+d3+d4+d5+d6+d7+d8+d9+plot_annotation(title = "Distributia variabilelor numerice pe categoria de churn")





denstot





p1<-ggplot(client, aes(x=PreferredLoginDevice, fill=Churn)) +
  geom_bar()
p2<-ggplot(client, aes(x=CityTier, fill=Churn)) +
  geom_bar()
p3<-ggplot(client, aes(x=PreferredPaymentMode, fill=Churn)) +
  geom_bar()
p4<-ggplot(client, aes(x=Gender, fill=Churn)) +
  geom_bar()
p5<-ggplot(client, aes(x=NumberOfDeviceRegistered, fill=Churn)) +
  geom_bar()
p6<-ggplot(client, aes(x=PreferedOrderCat, fill=Churn)) +
  geom_bar()
p7<-ggplot(client, aes(x=SatisfactionScore, fill=Churn)) +
  geom_bar()
p8<-ggplot(client, aes(x=MaritalStatus, fill=Churn)) +
  geom_bar()

boxplots <-p1+p2+p3+p4+p5+p6+p7+p8+plot_annotation(title = "Densitatea pe variabile categoriale in baza de churn")+
  plot_layout(ncol = 2, guides = "collect")

boxplots
# Add mean lines
p<-ggplot(df, aes(x=weight, color=sex)) +
  geom_density()+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=sex),
             linetype="dashed")

# Verificam daca influențează genul la rata de pierdere
client %>%
  group_by(Churn)|>
  count(Gender)

DeviceCount<-client %>%
  group_by(PreferredLoginDevice)|>
  count(OrderCount)

OrdClient<- client|>
  filter(!is.na(OrderCount))

OrdClient %>%
  group_by(Gender)|>
  summarise(Mean=mean(OrderCount))|>
  ggplot(aes(x=Gender, y=Mean, fill=Gender))+
  geom_col()+
  labs(title = "Numarul mediu de cumparaturi in raport cu genul")

ChClient <- client|>
  filter(Churn=="Pierdut")

ChClient|>
  group_by(Gender)|>
  summarise(percent=100*n()/nrow(ChClient))|>
ggplot( aes(x="", y=percent, fill=Gender)) +
  geom_bar(stat="identity", width=1) +
  coord_polar(theta = "y")+
  geom_text(aes(label = paste0(round(percent, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Gender Distribution of Churned Persons")+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank())



#Influenta plingerii la churn.
comp_churn <- client %>%
  group_by(Complain,Churn) %>%
  summarize(Count = n())


print(comp_churn)

# Rename columns and reset index
comp_churn <- comp_churn %>%
  mutate(Complain = ifelse(Complain == 0, 'No Complain', 'Complain'))

# Print the resulting data frame
print(comp_churn)

# Plot using ggplot2
ggplot(comp_churn, aes(x = Complain, y = Count, fill = Churn)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Complain Vs Churn", 
       x = "Complain", 
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "top") +
  coord_flip()


#Care MartialStatus are cea mai mare rată de dezabonare?

MaerChurn<-client|>
  group_by(Churn)|>
  count(MaritalStatus)
MaerChurn<-MaerChurn|>
  rename("Nr_Clienti"=n)
ggplot(MaerChurn, aes(x=MaritalStatus,y=Nr_Clienti, fill=Churn)) +
  geom_bar(stat = "identity", position = position_dodge())+
  labs(title = "Rata de pierdere a clientilor pe baza statutului social")



#Care CityTier are un nivel mai ridicat de Tenure și OrderCount?

ClTenute<-client |>
  filter(Tenure <35 & !is.na(WarehouseToHome))

ClTenute$CityTier <- as.character(ClTenute$CityTier)

glimpse(ClTenute)
ClTenute|>
  group_by(CityTier)|>
  summarise(Mean=mean(Tenure),max(Tenure))|>
  ggplot(aes(x=CityTier,y=Mean,fill=CityTier)) +
  geom_bar(stat = "identity", position = position_dodge())+
  labs(title = "Raportul vechimii medii a clientului pe baza tirului oraselor")

OrdClient$CityTier <- as.character(OrdClient$CityTier)

OrdClient|>
  group_by(CityTier)|>
  summarise(Mean=mean(OrderCount),max(OrderCount))|>
ggplot(aes(x=CityTier,y=Mean,fill=CityTier)) +
  geom_bar(stat = "identity", position = position_dodge())+
  labs(title = "Raportul numarului mediu de comenzi pe baza tirului oraselor")


#Clientul cu un scor de satisfacție ridicat are un HourSpendOnApp ridicat

ClHour<-client|>
  filter(!is.na(HourSpendOnApp))

ClHour|>
  group_by(SatisfactionScore,Churn)|>
  summarise(SumHours=sum(HourSpendOnApp))|>
  ggplot(aes(x=SatisfactionScore, y=SumHours, fill=Churn))+
  geom_col()+
  coord_flip()+
  geom_text(aes(label = SumHours), angle=90, vjust=-1.1)+
  labs(title = "Scorul de satisfacere VS Nr.ore in aplicatie")

#Care este relația dintre Complain și DaySinceLinceLastOrder?
install.packages("ggpubr")
library("ggpubr")
ggscatter(client, x = "DaySinceLastOrder", y = "Complain", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Nr de zile de la ultima comanda", ylab = "Plingere")+
  labs(title = "Determinarea corelatiei dintre variabile")

res <- cor.test(client$Complain, client$DaySinceLastOrder, 
                method = "pearson")

res

ggplot(client, aes(x=DaySinceLastOrder, y=Complain)) + geom_point()+
  facet_grid(~Churn)

Comp<-client
Comp$Complain<-as.character(Comp$Complain)

c1<-ggplot(Comp, aes(x=DaySinceLastOrder,fill=Complain))+
  geom_histogram(binwidth = 1)+
  geom_boxplot(position = )
c1
c2<-ggplot(Comp, aes(x=DaySinceLastOrder,fill=Complain))+
  geom_boxplot()


boxplots2 <-c2+c1+plot_annotation(title = "Nr de zile de la ultima comanda vs Plingere")+
  plot_layout(ncol = 1)

boxplots2

#Există o relație între PreferredLoginDevice și churn?

myplot3<- ggplot(client, aes(PreferredLoginDevice, group = Churn))+
  geom_bar(aes(y= ..prop.., fill= factor(..x..)), stat="count")+
  scale_y_continuous(labels = scales::percent) +
  ylab("frecventa relativa")+
  facet_grid(~Churn)+
  theme(legend.position="none")+
  labs(title = "Raportul dintre Device-urile preferate si rata de pierdere")
myplot3

#Există o relație între categoria de comandă preferată și rata de dezabonare?

Test<-client|>
  group_by(PreferedOrderCat,Gender)|>
  summarise(Count=n())|>
  group_by(PreferedOrderCat)|>
  mutate(Procent=(Count/ sum(Count))*100)

Test2<-client|>
  group_by(PreferedOrderCat,Churn)|>
  summarise(Count=n())|>
  group_by(PreferedOrderCat)|>
  mutate(Procent=(Count/ sum(Count))*100)

  ggplot(Test,aes(x=PreferedOrderCat, y=Procent, fill=Gender))+
  geom_col(position = position_dodge())+
    labs(title = "Categoria preferata VS Gen")
  
  ggplot(Test,aes(x=PreferedOrderCat, y=Procent, fill=Churn))+
    geom_col(position = position_dodge())+
    labs(title = "Categoria preferata VS Churn")

#Există o legătură între scorul de satisfacție și numărul de comenzi din ultima lună?

  glimpse(client)
  SatClient<-client
  SatClient$SatisfactionScore<-as.character(SatClient$SatisfactionScore)
  
  ggplot(SatClient, aes(x = SatisfactionScore, y = OrderCount, fill=SatisfactionScore)) + 
    geom_boxplot() +
    stat_summary(fun = "mean", geom = "point", shape = 8,
                 size = 2, color = "white")+
    geom_dotplot(binaxis = "y", stackdir = 'center', dotsize = 0.1)+
    labs(title = "Satisfaction Score Vs Order Count")
  
  #Creșterea procentuală a valorii comenzilor față de anul trecut afectează rata de dezabonare?
  
  ChurnAmount<-client|>
    group_by(Churn,OrderAmountHikeFromlastYear)|>
    summarise(Count=n())

  ggplot(ChurnAmount, aes(x=OrderAmountHikeFromlastYear, y=Count, size=Count, color=Churn))+
    geom_point()+
    labs(title = "Creștere a valorii comenzii față de anul trecut vs Churn")

  #Clienții care au folosit mai multe cupoane au rate de renunțare mai mici?
  
  CuponFol<-client|>
    group_by(Churn,CouponUsed)|>
    count(CouponUsed)
  
  CuponFol<-CuponFol|>
    rename(Count=n)
  
  ggplot(CuponFol, aes(x=CouponUsed,y=Count,fill=Churn))+
    geom_col(position = position_dodge())+
    labs(title = "Coupon used vs Count")
  
  #Clienții care s-au plâns sunt mai predispuși la dezabonare?
  
  
  Test3<-client|>
    group_by(Complain,Churn)|>
    summarise(Count=n())|>
    group_by(Complain)|>
    mutate(Procent=(Count/ sum(Count))*100)
  
  ggplot(Test3, aes(area = Procent, fill = Churn,
                 label = paste(group, value, sep = "\n"))) +
    geom_map() +
    geom_treemap_text(colour = "white",
                      place = "centre",
                      size = 15) +
    theme(legend.position = "none")
  
  
  
#eliminarea valorilor lipsă

ggplot(client, aes(x=Tenure))+
  geom_density()
  
clValid<-client |>
  fill(Tenure)

ggplot(clValid, aes(x=Tenure))+
  geom_density()

result<-print(clValid|>
  filter(is.na(Tenure))|>
  count(Tenure))


ggplot(client, aes(x=Tenure))+
  geom_density()

clValid<-client |>
  fill(Tenure)

ggplot(clValid, aes(x=Tenure))+
  geom_density()

result<-print(clValid|>
                filter(is.na(Tenure))|>
                count(Tenure))



glimpse(clValid)

ggplot(clValid, aes(x=WarehouseToHome))+
  geom_density()


clValid<-clValid |>
  fill(WarehouseToHome)

clValid|>
  count(is.na(WarehouseToHome))

ggplot(clValid, aes(x=HourSpendOnApp))+
  geom_density()

vals <- seq(1, 4,1)

clValid$HourSpendOnApp[is.na(clValid$HourSpendOnApp)] <- sample(vals, sum(is.na(clValid$HourSpendOnApp)), replace = TRUE)

clValid<-clValid|>
  fill(HourSpendOnApp)

ggplot(clValid, aes(x=HourSpendOnApp))+
  geom_density()


ggplot(clValid, aes(x=OrderAmountHikeFromlastYear))+
  geom_density()

clValid<-clValid|>
  fill(HourSpendOnApp, .direction = 'up')

ggplot(clValid, aes(x=OrderAmountHikeFromlastYear))+
  geom_density()


clValid<-clValid|>
  fill(OrderAmountHikeFromlastYear, .direction = 'up')

ggplot(test, aes(x=OrderAmountHikeFromlastYear))+
  geom_density()

ggplot(clValid, aes(x=CouponUsed))+
  geom_density()

valCup<- median(clValid$CouponUsed, na.rm = TRUE )


install.packages("mice")

library(mice)

test<-clValid|>
  fill(CouponUsed, .direction = 'up')

ggplot(test, aes(x=CouponUsed))+
  geom_density()

test|>
  count(is.na(CouponUsed))

clValid<-clValid|>
  fill(CouponUsed, .direction = 'up')

#OrderCount
ggplot(clValid, aes(x=OrderCount))+
  geom_density()
clValid|>
  count(is.na(OrderCount))

test<-clValid|>
  fill(OrderCount, .direction = 'up')

ggplot(test, aes(x=OrderCount))+
  geom_density()

clValid<-clValid|>
  fill(OrderCount, .direction = 'up')
library(ggplot2)

#DaySinceLastOrder
ggplot(clValid, aes(x=DaySinceLastOrder))+
  geom_density()
clValid|>
  count(is.na(DaySinceLastOrder))

test<-clValid|>
  fill(DaySinceLastOrder, .direction = 'up')

ggplot(test, aes(x=DaySinceLastOrder))+
  geom_density()

clValid<-clValid|>
  fill(DaySinceLastOrder, .direction = 'up')

clValid<- clValid |> mutate(PreferredLoginDevice=ifelse(PreferredLoginDevice=='Phone','Mobile Phone',PreferredLoginDevice))

client |> count(PreferredLoginDevice)
client |> count(PreferedOrderCat)
clValid<- clValid |> mutate(PreferedOrderCat=ifelse(PreferedOrderCat=='Mobile','Mobile Phone',PreferedOrderCat))
client |> count(PreferedOrderCat)

client |> count(PreferredPaymentMode)

clValid<- clValid |> mutate(PreferredPaymentMode=case_when(PreferredPaymentMode=="CC"~'Credit Card',
                                                         PreferredPaymentMode=="COD"~'Cash on Delivery',
                                                         .default = PreferredPaymentMode))


test<-clValid|>
  count(DaySinceLastOrder)
clValid<-clValid|>
  filter(DaySinceLastOrder<30)

tcheck<-test|>
  count(DaySinceLastOrder)

install.packages("xlsx")
library(xlsx)

library(writexl)

write_xlsx(clValid,"U:\\Univer\\Anul 3\\Analiza Datelor\\clients.xlsx")



#Vizualizarea legaturii dintre Creșterea numaruli de comeniz și numarului de cashback

ggplot(data = client, aes(x=OrderAmountHikeFromlastYear, y=CashbackAmount))+
  geom_point()

#Vizualizarea raportului dinte vechimea clientului și timpului cheltuit in aplicatie

ggplot(data = client, aes(x=HourSpendOnApp, y=Tenure))+
  geom_point()

#Analiza churnului clientilor pe baza tirului de oras
client |>
  group_by(Churn) |>
  count(CityTier)

#Analiza churnului clientilor pe baza modului de plata
client |>
  group_by(Churn) |>
  count(PreferredPaymentMode)

#Analiza proportiilor dintre scorul de satisfacere si churn-ul clientilor

tab1 <- client |>
  count(Churn, SatisfactionScore) |>
  group_by(Churn) |>
  mutate(prop = n/sum(n)) |>
  pivot_wider(id_cols = SatisfactionScore, names_from = Churn, values_from = prop)
tab1


ggplot(tab1, aes(x= SatisfactionScore))+
  geom_bar()

# Analiza numarului de clienti si modului de plata preferat
client |>
  count (Churn, PreferredPaymentMode) |>
  pivot_wider(names_from = Churn, values_from = n) 

ggplot(client, aes(x=PreferredPaymentMode))+
  geom_bar()

myplot<- ggplot(client, aes(PreferredPaymentMode, group = Churn))+
  geom_bar(aes(y= ..prop.., fill= factor(..x..)), stat="count")+
  scale_y_continuous(labels = scales::percent) +
  ylab("frecventa relativa")+
  facet_grid(~Churn)
myplot


# Analiza variabilei de distanta in timp de la depozit

# Gasirea unor inscrieri anpmalice ???
ggplot(data=client, aes(x=WarehouseToHome))+
  geom_histogram(binwidth = 3)

#ELiminara inscrieirilor

client %>% 
  mutate(WarehouseCheck=if_else(WarehouseToHome>=50,"big then 50","low then 50")) %>% 
  count(WarehouseCheck)

client<-client |>
  filter(WarehouseToHome <50 & !is.na(WarehouseToHome))


ggplot(data=client, aes(x=WarehouseToHome))+
  geom_histogram(binwidth = 1)

#Analiza influenteii distantei la churn
client_Warehouse <-client %>% 
  mutate(WarehouseDist=if_else(WarehouseToHome>=20,"more or = 20 ore","less then 20"))
  
myplot2<- ggplot(client_Warehouse, aes(WarehouseDist, group = Churn))+
  geom_bar(aes(y= ..prop.., fill= factor(..x..)), stat="count")+
  scale_y_continuous(labels = scales::percent) +
  ylab("frecventa relativa")+
  facet_grid(~Churn)
myplot2

# Analiza variabilei de vechime a clientului

client |>
  filter(is.na(Tenure))

ggplot(data=client, aes(x=Tenure))+
  geom_histogram(binwidth = 1)

client %>% 
  mutate(TenureNew=if_else(Tenure>=35,"big then 35","low then 35")) %>% 
  count(TenureNew)
  
#Eliminara inscrieirilor
client<-client |>
  filter(Tenure <35 & !is.na(WarehouseToHome))

myplot3<- ggplot(client, aes(Tenure, group = Churn))+
  geom_bar(aes(y= ..prop.., fill= factor(..x..)), stat="count")+
  scale_y_continuous(labels = scales::percent) +
  ylab("frecventa relativa")+
  facet_grid(~Churn)
myplot3

#Analiza variabilei preferedLoginDevice

myplot3<- ggplot(client, aes(PreferredLoginDevice, group = Churn))+
  geom_bar(aes(y= ..prop.., fill= factor(..x..)), stat="count")+
  scale_y_continuous(labels = scales::percent) +
  ylab("frecventa relativa")+
  facet_grid(~Churn)
myplot3

#Analiza variabilei NUmberOfAddress

ggplot(data=client, aes(x=NumberOfAddress))+
  geom_boxplot()

ggplot(data=client, aes(x=NumberOfAddress))+
  geom_dotplot(binwidth = 1/150)

client<-client |>
  filter(NumberOfAddress <15 & !is.na(WarehouseToHome))

ggplot(data=client, aes(x=NumberOfAddress))+
  geom_boxplot()

#Raportul dinre nr de adresări și churn
myplot4<- ggplot(client, aes(NumberOfAddress, group = Churn))+
  geom_bar(aes(y= ..prop.., fill= factor(..x..)), stat="count")+
  scale_y_continuous(labels = scales::percent) +
  ylab("frecventa relativa")+
  facet_grid(~Churn)
myplot4

# Analiza variabilei de citiyTier

client %>%
  count(CityTier)

myplot5<- ggplot(client, aes(CityTier, group = Churn))+
  geom_bar(aes(y= ..prop.., fill= factor(..x..)), stat="count")+
  scale_y_continuous(labels = scales::percent) +
  ylab("frecventa relativa")+
  facet_grid(~Churn)
myplot5

#Analiza variabilei Gender

client %>%
  count(Gender)

myplot6<- ggplot(client, aes(Gender, group = Churn))+
  geom_bar(aes(y= ..prop.., fill= factor(..x..)), stat="count")+
  scale_y_continuous(labels = scales::percent) +
  ylab("frecventa relativa")+
  facet_grid(~Churn)
myplot6

#Analiza variabilei HoursSpendOnApp

client %>%
  count(HourSpendOnApp)

ggplot(data=client, aes(x=HourSpendOnApp))+
  geom_bar()

client<-client |>
  filter(HourSpendOnApp <5 & HourSpendOnApp>1 & !is.na(WarehouseToHome))

ggplot(data=client, aes(x=HourSpendOnApp))+
  geom_bar()

myplot7<- ggplot(client, aes(HourSpendOnApp, group = Churn))+
  geom_bar(aes(y= ..prop.., fill= factor(..x..)), stat="count")+
  scale_y_continuous(labels = scales::percent) +
  ylab("frecventa relativa")+
  facet_grid(~Churn)
myplot7

#Analiza variabilei NumberOfDeviceRegistered

client %>%
  count(NumberOfDeviceRegistered)

ggplot(data=client, aes(x=NumberOfDeviceRegistered))+
  geom_bar()

myplot8<- ggplot(client, aes(NumberOfDeviceRegistered, group = Churn))+
  geom_bar(aes(y= ..prop.., fill= factor(..x..)), stat="count")+
  scale_y_continuous(labels = scales::percent) +
  ylab("frecventa relativa")+
  facet_grid(~Churn)
myplot8

#Analiza variabilei PreferedOrderCat <------------------------ Diference

client %>%
  count(PreferedOrderCat)

myplot8<- ggplot(client, aes(PreferedOrderCat, group = Churn))+
  geom_bar(aes(y= ..prop.., fill= factor(..x..)), stat="count")+
  scale_y_continuous(labels = scales::percent) +
  ylab("frecventa relativa")+
  facet_grid(~Churn)
myplot8


#Analiza variabilei SatisfactionsScore

client %>%
  count(SatisfactionScore)

myplot9<- ggplot(client, aes(SatisfactionScore, group = Churn))+
  geom_bar(aes(y= ..prop.., fill= factor(..x..)), stat="count")+
  scale_y_continuous(labels = scales::percent) +
  ylab("frecventa relativa")+
  facet_grid(~Churn)
myplot9

#Analiza variabilei maritatlStatus  <---------------- Diference
client %>%
  count(MaritalStatus)

myplot10<- ggplot(client, aes(MaritalStatus, group = Churn))+
  geom_bar(aes(y= ..prop.., fill= factor(..x..)), stat="count")+
  scale_y_continuous(labels = scales::percent) +
  ylab("frecventa relativa")+
  facet_grid(~Churn)
myplot10

# Analiza variabilei Complain (Plingere)

client %>%
  count(Complain)

myplot11<- ggplot(client, aes(Complain, group = Churn))+
  geom_bar(aes(y= ..prop.., fill= factor(..x..)), stat="count")+
  scale_y_continuous(labels = scales::percent) +
  ylab("frecventa relativa")+
  facet_grid(~Churn)
myplot11

#Analiza variabilei OrderAmountHikeFromlastYear

client %>%
  count(OrderAmountHikeFromlastYear)

ggplot(data=client, aes(x=OrderAmountHikeFromlastYear))+
  geom_bar()

client<- client %>% 
  mutate(AmountHikeTransf=case_when(OrderAmountHikeFromlastYear<=15 ~ "less then 15",
                            cls_students>=15 & cls_students<=20 ~ "less than 20",
                            .default = "20 +"))


myplot12<- ggplot(client, aes(OrderAmountHikeFromlastYear, group = Churn))+
  geom_bar(aes(y= ..prop.., fill= factor(..x..)), stat="count")+
  scale_y_continuous(labels = scales::percent) +
  ylab("frecventa relativa")+
  facet_grid(~Churn)
myplot12






 





mean(client$WarehouseToHome)
x
client <-client %>% 
  mutate(DistaWarehouseChar=if_else(WarehouseToHome>=mean(client$WarehouseToHome,na.rm = TRUE),"at or above mean","below mean")) %>% 
  count(DistaWarehouseChar)

 glimpse(client)

client |>
  group_by(SatisfactionScore) |>
  count(WarehouseToHome)

Satisfaction_Warehouse <- client |> 
  count(SatisfactionScore, WarehouseToHome) |> 
  pivot_wider(names_from = SatisfactionScore, values_from = n)

Satisfaction_Warehouse



