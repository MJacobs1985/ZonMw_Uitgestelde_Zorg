#### EMPTY ALL ----
rm(list = ls())
#### PARALLEL PROCESSING ----
library(doParallel);parallelCluster <- makeCluster(6, type = "SOCK", methods = FALSE)
#### LOAD LIBRARIES ----
library(readr)
library(DataExplorer)
library(dplyr)
library(ggplot2)
library(tidyr)
library(survival)
library(survminer)
library(naniar)
library(stringr)
library(readxl)
library(ggfortify)
library(penalized)
library(splines)
#### LOAD, WRANGLE AND EXPLORE CBS DATA    - LIFE EXPECTANCY ----
CBS_lv <- read_delim("Data/CBS/Levensverwachting.csv",
                     delim = ";", 
                     escape_double = FALSE, 
                     trim_ws = TRUE)
dim(CBS_lv)
View(CBS_lv)
str(CBS_lv)
CBS_lv[CBS_lv == "."] <- NA

table(CBS_lv$ID)
table(CBS_lv$Geslacht)
table(CBS_lv$Marges)
table(CBS_lv$Perioden)
table(CBS_lv$LeeftijdOp31December)
length(table(CBS_lv$LeeftijdOp31December))
length(unique(levels(factor(CBS_lv$LeeftijdOp31December))))

DataExplorer::introduce(CBS_lv)
DataExplorer::profile_missing(CBS_lv)
DataExplorer::plot_intro(CBS_lv)
DataExplorer::plot_correlation(CBS_lv)

recode(CBS_lv$Geslacht, 
       T001038 = "Unknown", 
       '3000' = Man, 
       '4000' = Vrouw)

CBS_lv%>%
  select(Geslacht, Levensverwachting_1, Marges, LeeftijdOp31December, Perioden)%>%
  mutate(Perioden             = substr(Perioden, 1, 4), 
         LeeftijdOp31December = substr(LeeftijdOp31December, 2, 3), 
         Levensverwachting_1 = as.numeric(Levensverwachting_1))%>%
  filter(!Geslacht=="T001038")%>%
  mutate(Geslacht = replace(Geslacht, 
                            Geslacht == 3000, "Man"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 4000, "Women"))%>%
  drop_na()%>%
  ggplot(., aes(x=Perioden, 
                y=Levensverwachting_1, 
                col=LeeftijdOp31December, 
                group=LeeftijdOp31December))+
  geom_point()+
  geom_line()+
  facet_grid(~Geslacht, scales="free")+
  theme_bw()+
  labs(x="Year", 
       y="Life expectancy", 
       col="Age at 31st December", 
       title = "Life expectancy in the Netherlands",
       subtitle = "Source: CBS")

CBS_lv%>%
  select(Geslacht, Levensverwachting_1, Marges, LeeftijdOp31December, Perioden)%>%
  mutate(Perioden             = substr(Perioden, 1, 4), 
         LeeftijdOp31December = substr(LeeftijdOp31December, 2, 3), 
         Levensverwachting_1 = as.numeric(Levensverwachting_1))%>%
  rename(Levensverwachting=Levensverwachting_1)%>%
  filter(!Geslacht=="T001038")%>%
  mutate(Geslacht = replace(Geslacht, 
                            Geslacht == 3000, "Man"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 4000, "Women"))%>%
  drop_na()%>%
  ggplot(., aes(y=Perioden, 
                fill=Levensverwachting, 
                x=LeeftijdOp31December))+
  geom_tile()+
  facet_wrap(~Geslacht, ncol=1)+
  scale_fill_viridis_c(option="turbo")+
  theme_bw()+
  theme(legend.position = "right")+
  labs(y="Year", 
       x="Age at 31st December", 
       fill="Life expectancy", 
       title = "Life expectancy in the Netherlands",
       subtitle = "Source: CBS")

CBS_lv%>%
  select(Geslacht, Levensverwachting_1, Marges, LeeftijdOp31December, Perioden)%>%
  mutate(Perioden             = substr(Perioden, 1, 4), 
         LeeftijdOp31December = substr(LeeftijdOp31December, 2, 3), 
         Levensverwachting_1 = as.numeric(Levensverwachting_1))%>%
  filter(!Geslacht=="T001038")%>%
  filter(LeeftijdOp31December=="00")%>%
  mutate(Geslacht = replace(Geslacht, 
                            Geslacht == 3000, "Man"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 4000, "Women"))%>%
  drop_na(LeeftijdOp31December)%>%
  ggplot(., aes(x=factor(Perioden), 
                y=Levensverwachting_1, 
                col=factor(Geslacht)))+
  geom_point()+
  geom_line()+
  theme_bw()+
  labs(x="Year", 
       y="Life expectancy", 
       col="Age at 31st December", 
       title = "Life expectancy in the Netherlands at Age=0",
       subtitle = "Source: CBS")

#### LOAD, WRANGLE AND EXPLORE WMD DATA    - MORTALITY                    - AKERLINSKY ----
world_mortality <- read_csv("Data/Akerlinsky_WMD/Oversterfte/world_mortality.csv")
table(world_mortality$country_name)
table(world_mortality$year)
colnames(world_mortality)
world_mortality%>%
  filter(time_unit=="monthly" & year>0)%>%
  mutate(date = zoo::as.yearmon(paste(year, time), "%Y %m"))%>%
  ggplot(.)+
  geom_tile(aes(x=date, 
                y=country_name, 
                fill=deaths))+
  scale_fill_viridis_c(option="turbo")+
  theme_bw()+
  theme(legend.position = "bottom")

world_mortality%>%
  filter(country_name=="Netherlands")%>%
  mutate(date = lubridate::make_datetime(year = year) + lubridate::weeks(time))%>%
  ggplot(.)+
  geom_point(aes(x=date, 
                 y=deaths))+
  geom_line(aes(x=date, 
                y=deaths, group=1))+
  theme_bw()+
  labs(x="Date", 
       y="Mortality", 
       title="Mortality (alle causes) for The Netherlands",
       caption="Source: World Mortality Dataset by Ariel Karlinsky")

#### COMPARE WMD, CBS & OWID Data          - MORTALITY  ----
CBS_Sterfte <- read_delim("Data/CBS/Sterfte.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
CBS_Sterfte[CBS_Sterfte == "."] <- NA
CBS_data<-CBS_Sterfte%>%
  select(Geslacht, Overledenen_1, Perioden, LeeftijdOp31December)%>%
  mutate(Overledenen_1 = as.numeric(Overledenen_1), 
         Year = as.numeric(substr(Perioden, 1, 4)), 
         Week = as.numeric(substr(Perioden, 7, 8)), 
         Date = lubridate::ymd(lubridate::make_datetime(year = Year) + 
                                 lubridate::weeks(Week)))%>%
  mutate(Geslacht = replace(Geslacht, 
                            Geslacht == 1100, "Total"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 3000, "Man"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 4000, "Women"),
         LeeftijdOp31December = replace(LeeftijdOp31December, 
                                        LeeftijdOp31December == 10000, "Total"),
         LeeftijdOp31December = replace(LeeftijdOp31December, 
                                        LeeftijdOp31December == 21700, "80_and_older"),
         LeeftijdOp31December = replace(LeeftijdOp31December, 
                                        LeeftijdOp31December == 41700, "0_to_65"),
         LeeftijdOp31December = replace(LeeftijdOp31December, 
                                        LeeftijdOp31December == 53950, "65_to_80"))%>%
  mutate(source="CBS")%>%
  filter(LeeftijdOp31December=="Total")%>%
  filter(Geslacht=="Total")%>%
  filter(Year>2014)%>%
  drop_na()%>%
  select(Date, Overledenen_1,source)%>%
  rename(deaths=Overledenen_1, 
         date=Date)
WMD_data<-read_csv("Data/Akerlinsky_WMD/Oversterfte/world_mortality.csv")%>%
  filter(country_name=="Netherlands")%>%
  mutate(date = lubridate::make_datetime(year = year) + lubridate::weeks(time))%>%
  mutate(source="WMD")%>%
  select(date,deaths,source)%>%
  mutate(date=lubridate::ymd(date))
WMD_data
CBS_data
combined<-rbind(CBS_data, WMD_data)
ggplot(combined)+
  geom_line(aes(x=date, 
                y=deaths, 
                col=source))+
  theme_bw()+
  labs(x="Date", 
       y="Mortality", 
       title="Mortality (alle causes) for The Netherlands",
       caption="Source: World Mortality Dataset by Ariel Karlinsky & CBS")
OWID_data<-read_csv("Data/OWID/Oversterfte/excess-mortality-raw-death-count.csv")%>%
  filter(Entity=="Netherlands")%>%
  mutate(Week=row_number())%>%
  select(-c(projected_deaths_since_2020_all_ages, Day))%>%
  pivot_longer(-c(Entity, Code, Week),
               names_to = "Jaar",
               values_to = "Value",
               values_drop_na = TRUE)%>%
  mutate(Jaar = substring(Jaar,8,11))%>%
  mutate(Year = as.numeric(Jaar), 
         Week = as.numeric(Week), 
         Date = lubridate::ymd(lubridate::make_datetime(year = Year) +
                                 lubridate::weeks(Week)))%>%
  select(Value, Date)%>%
  mutate(source="OWID")%>%
  rename(deaths=Value, 
         date=Date)
combined2<-rbind(combined, OWID_data)
ggplot(combined2)+
  geom_line(aes(x=date, 
                y=deaths, 
                col=source))+
  theme_bw()+
  labs(x="Date", 
       y="Mortality", 
       title="Mortality (alle causes) for The Netherlands",
       caption="Source: World Mortality Dataset by Ariel Karlinsky & CBS & OWID")

ggplot(combined2)+
  geom_line(aes(x=date, 
                y=deaths, 
                col=source))+
  theme_bw()+
  facet_wrap(~source, ncol=1)+
  labs(x="Date", 
       y="Mortality",
       col="Datasource",
       title="Mortality (alle causes) for The Netherlands",
       caption="Source: World Mortality Dataset by Ariel Karlinsky & CBS & OWID")+
  theme(legend.position = "bottom")



#### LOAD, WRANGLE AND EXPLORE CBS DATA    - MORTALITY ----
CBS_Sterfte <- read_delim("Data/CBS/Sterfte.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(CBS_Sterfte)
str(CBS_Sterfte)
CBS_Sterfte[CBS_Sterfte == "."] <- NA

table(CBS_Sterfte$ID)
table(CBS_Sterfte$Geslacht)
table(CBS_Sterfte$Perioden)
table(CBS_Sterfte$LeeftijdOp31December)

CBS_Sterfte%>%
  select(Geslacht, Overledenen_1, Perioden, LeeftijdOp31December)%>%
  mutate(Overledenen_1 = as.numeric(Overledenen_1), 
         Year = as.numeric(substr(Perioden, 1, 4)), 
         Week = as.numeric(substr(Perioden, 7, 8)), 
         Date = lubridate::ymd(lubridate::make_datetime(year = Year) + 
                                 lubridate::weeks(Week)))%>%
  mutate(Geslacht = replace(Geslacht, 
                            Geslacht == 1100, "Total"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 3000, "Man"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 4000, "Women"),
         LeeftijdOp31December = replace(LeeftijdOp31December, 
                                        LeeftijdOp31December == 10000, "Total"),
         LeeftijdOp31December = replace(LeeftijdOp31December, 
                                        LeeftijdOp31December == 21700, "80_and_older"),
         LeeftijdOp31December = replace(LeeftijdOp31December, 
                                        LeeftijdOp31December == 41700, "0_to_65"),
         LeeftijdOp31December = replace(LeeftijdOp31December, 
                                        LeeftijdOp31December == 53950, "65_to_80"))%>%
  filter(!LeeftijdOp31December=="Total")%>%
  filter(!Geslacht=="Total")%>%
  drop_na()%>%
  ggplot(., aes(x=Date, 
                y=Overledenen_1, 
                col=factor(Geslacht)))+
  #geom_point()+
  geom_line()+
  facet_grid(~LeeftijdOp31December, scales="free")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Date", 
       col="Sex", 
       y="Deceased", 
       title="Number of deceased by Date, Sex, and Age category", 
       subtitle = "Source: CBS")

CBS_Sterfte%>%
  select(Geslacht, Overledenen_1, Perioden, LeeftijdOp31December)%>%
  mutate(Overledenen_1 = as.numeric(Overledenen_1), 
         Year = as.numeric(substr(Perioden, 1, 4)), 
         Week = as.numeric(substr(Perioden, 7, 8)), 
         Date = lubridate::ymd(lubridate::make_datetime(year = Year) + 
                                 lubridate::weeks(Week)))%>%
  mutate(Geslacht = replace(Geslacht, 
                            Geslacht == 1100, "Total"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 3000, "Man"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 4000, "Women"),
         LeeftijdOp31December = replace(LeeftijdOp31December, 
                                        LeeftijdOp31December == 10000, "Total"),
         LeeftijdOp31December = replace(LeeftijdOp31December, 
                                        LeeftijdOp31December == 21700, "80_and_older"),
         LeeftijdOp31December = replace(LeeftijdOp31December, 
                                        LeeftijdOp31December == 41700, "0_to_65"),
         LeeftijdOp31December = replace(LeeftijdOp31December, 
                                        LeeftijdOp31December == 53950, "65_to_80"))%>%
  filter(!LeeftijdOp31December=="Total")%>%
  filter(!Geslacht=="Total")%>%
  drop_na()%>%
  ggplot(., aes(x=Date, 
                y=Overledenen_1, 
                col=factor(LeeftijdOp31December)))+
  geom_line()+
  facet_grid(~Geslacht, scales="free")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Date", 
       col="Age category", 
       y="Deceased", 
       title="Number of deceased by Date, Sex, and Age category", 
       subtitle = "Source: CBS")



#### LOAD, WRANGLE AND EXPLORE OWID DATA   - MORTALITY                    - RAW DEATH ----
excess_mortality_raw_death_count <- read_csv("Data/OWID/Oversterfte/excess-mortality-raw-death-count.csv")
colnames(excess_mortality_raw_death_count)

table(excess_mortality_raw_death_count$Entity)
table(excess_mortality_raw_death_count$Entity=="Netherlands")

excess_mortality_raw_death_count%>%
  filter(Entity=="Netherlands")%>%
  select(Day)%>%
  dim()

excess_mortality_raw_death_count%>%
  filter(Entity=="Netherlands")%>%
  mutate(Week=row_number())%>%
  select(-c(projected_deaths_since_2020_all_ages, Day))%>%
  pivot_longer(-c(Entity, Code, Week),
               names_to = "Jaar",
               values_to = "Value",
               values_drop_na = TRUE)%>%
  mutate(Jaar = substring(Jaar,8,11))%>%
  ggplot()+
  geom_point(aes(x=Week, y=Value, col=Jaar, group=Jaar))+
  geom_line(aes(x=Week, y=Value, col=Jaar, group=Jaar))+
  gghighlight::gghighlight(Jaar>2019)+
  theme_bw()+
  labs(x="Week", 
       y="Raw death count ", 
       title="Mortality for The Netherlands", 
       caption="Source: Our World in Data")

excess_mortality_raw_death_count%>%
  filter(Entity=="Netherlands")%>%
  mutate(Week=row_number())%>%
  select(-c(projected_deaths_since_2020_all_ages, Day))%>%
  pivot_longer(-c(Entity, Code, Week),
               names_to = "Jaar",
               values_to = "Value",
               values_drop_na = TRUE)%>%
  mutate(Jaar = substring(Jaar,8,11))%>%
  ggplot()+
  geom_tile(aes(x=factor(Week), fill=Value, y=Jaar))+
  scale_fill_viridis_c(option="turbo")+
  theme_bw()+
  labs(x="Week", 
       y="Year",
       fill="Raw death count ", 
       title="Mortality for The Netherlands", 
       subtitle="Source: Our World in Data")+
  theme(legend.position = "bottom")



#### LOAD, WRANGLE AND EXPLORE WMD DATA    - MORTALITY & EXCESS MORTALITY - DKOBAK ----
excess_mortality_timeseries <- read_csv("Data/Dkobak_WMD/excess-mortality-timeseries.csv")
colnames(excess_mortality_timeseries)
table(excess_mortality_timeseries$time_unit)

MD_NL<-read_csv("Data/Akerlinsky_WMD/Oversterfte/world_mortality.csv")%>%
  filter(country_name=="Netherlands")%>%
  mutate(date = lubridate::make_datetime(year = year) + lubridate::weeks(time))%>%
  select(date, country_name, deaths)
EMD_NL<-excess_mortality_timeseries%>%
  filter(country_name=="Netherlands")%>%
  mutate(date = lubridate::make_datetime(year = year) + lubridate::weeks(time))%>%
  rename(deaths = `excess deaths`)%>%
  select(date, country_name, deaths)
ggplot()+
  geom_line(data=MD_NL, 
            aes(x=date, 
                y=deaths, 
                col="Mortality"))+
  geom_line(data=EMD_NL,
            aes(x=date,
                y=deaths,
                col="Excess Mortality"))+
  theme_bw()+
  labs(x="Date", 
       y="Mortality",
       col="",
       title="Mortality and excess mortality in the Netherlands", 
       caption="Source: World Mortality Data by Ariel Karlinsky and Dmitry Kobak")+
  theme(legend.position="bottom")


MD_NL%>%
  rename(mortality=deaths)%>%
  select(-country_name)%>%
  left_join(., EMD_NL, by = join_by(date))%>%
  rename(EM = deaths)%>%
  mutate(difference=mortality-EM)%>%
  ggplot(aes(x=date))+
  geom_line(aes(y=mortality, 
                col="Observed"))+
  geom_line(aes(y=EM,
                col="Excess"))+
  geom_line(aes(y=difference,
                col="Expected"))+
  theme_bw()+
  labs(x="Date", 
       y="Mortality",
       col="",
       title="Mortality in the Netherlands", 
       subtitle="Looking at observed mortality, expected mortality, and excess mortality",
       caption="Source: World Mortality Data by Ariel Karlinsky and Dmitry Kobak")+
  theme(legend.position="bottom")














#### LOAD, WRANGLE AND EXPLORE CBS DATA    - EXCESS MORTALITY ----
Excess_mortality <- read_csv("Data/CBS/Excess_mortality.csv")
str(Excess_mortality)
Excess_mortality%>%
  mutate(Date = lubridate::ymd(lubridate::make_datetime(year = Jaar) + 
                                 lubridate::weeks(Week)))%>%
  ggplot(aes(x=Date, 
             y=Overledenen))+
  geom_point()+
  geom_line()+
  geom_ribbon(aes(ymin=Verwacht_aantal_overledenen_laag, 
                  ymax=Verwacht_aantal_overledenen_hoog, 
                  fill="Excess Mortality"), 
              alpha=0.2, show.legend = FALSE)+
  theme_bw()+
  labs(x="Date", 
       y="Mortality", 
       caption="Source: CBS", 
       title="Mortality and excess mortality in The Netherlands")


Excess_mortality%>%
  mutate(Date = lubridate::ymd(lubridate::make_datetime(year = Jaar) + 
                                 lubridate::weeks(Week)))%>%
  ggplot(aes(x=Date, 
             y=Overledenen))+
  geom_rect(aes(xmin=as.Date("2020-03-16"),
                xmax=as.Date("2020-07-01"),
                ymin=0,ymax=Inf),fill="lightgrey", alpha=0.1)+
  geom_rect(aes(xmin=as.Date("2020-03-16"),
                xmax=as.Date("2020-06-15"),
                ymin=0,ymax=Inf),fill="grey", alpha=0.1)+
  geom_rect(aes(xmin=as.Date("2020-03-16"),
                xmax=as.Date("2020-05-18"),
                ymin=0,ymax=Inf),fill="darkgrey", alpha=0.5)+
  geom_line()+
  geom_line(aes(y=Verwacht_aantal_overledenen), col="purple")+
  geom_ribbon(aes(ymin=Verwacht_aantal_overledenen_laag, 
                  ymax=Verwacht_aantal_overledenen_hoog), 
                  fill="purple",alpha=0.1, show.legend = FALSE)+
  #geom_segment(aes(x = as.Date("2021-01-06") , y = 0, 
                   #xend = as.Date("2021-01-06"), yend = Inf), lty=2, col="darkgrey") +
  geom_text(aes(x=as.Date("2020-03-16"), y= 0, 
                label = "Stop screening"),
            family="Lato",
            colour = "darkred", 
            hjust = 0, size = 4, angle=90, 
            fontface="plain",check_overlap = TRUE)+
  geom_text(aes(x=as.Date("2020-05-18"), y= 0, 
                label = "Colon"), 
            family="Lato",
            colour = "darkgreen", 
            hjust = 0, size = 4, angle=90, 
            fontface="plain",check_overlap = TRUE)+
  geom_text(aes(x=as.Date("2020-06-15"), y= 0, 
                label = "Breast"), 
            family="Lato",
            colour = "darkgreen", 
            hjust = 0, size = 4, angle=90, 
            fontface="plain",check_overlap = TRUE)+
  geom_text(aes(x=as.Date("2020-07-01"), y= 0, 
                label = "Cervical"), 
            family="Lato",
            colour = "darkgreen", 
            hjust = 0, size = 4, angle=90, 
            fontface="plain",check_overlap = TRUE)+
  #geom_text(aes(x=as.Date("2021-01-06"), y= 0, 
                #label = "Start vaccines"),
            #family="Lato",
            #colour = "black",hjust=0, 
            #vjust = -0.5, size = 4, angle=90, 
            #fontface="plain",check_overlap = TRUE)+
  theme_bw()+
  labs(x="Date", 
       y="Mortality", 
       caption="Source: CBS", 
       title="Mortality and excess mortality in The Netherlands", 
       subtitle="Added are the stop and restart dates of the screening programmes")+
  coord_cartesian(ylim=c(0, 6000))


Excess_mortality%>%
  mutate(Date = lubridate::ymd(lubridate::make_datetime(year = Jaar) + 
                                 lubridate::weeks(Week)))%>%
  ggplot(aes(x=Date, 
             y=Overledenen))+
  #geom_point()+
  geom_line()+
  geom_line(aes(y=Verwacht_aantal_overledenen), col="red")+
  #geom_ribbon(aes(ymin=Verwacht_aantal_overledenen_laag, 
                  #ymax=Verwacht_aantal_overledenen_hoog, 
                  #fill="Excess Mortality"), 
              #alpha=0.2, show.legend = FALSE)+
  theme_bw()+
  labs(x="Datum", 
       y="Sterfte",
       col="Oversterfte",
       caption="Bron: CBS", 
       title="Sterfte en oversterfte in Nederland")

  
Excess_mortality_age <- read_csv("Data/CBS/Excess_mortality_age.csv", 
                                 col_names = FALSE)
colnames(Excess_mortality_age)<-c("Jaar","Week","Overleden_0tot65","Verwacht_0tot65",
                                  "Verwacht_0tot65_laag","Verwacht_0tot65_hoog",
                                  "Overleden_65tot80","Verwacht_65tot80",
                                  "Verwacht_65tot80_laag","Verwacht_65tot80_hoog",
                                  "Overleden_80enouder","Verwacht_80enouder","Verwacht_80enouder_laag",
                                  "Verwacht_80enouder_hoog")
str(Excess_mortality_age)
g1<-Excess_mortality_age%>%
  mutate(Jaar=as.numeric(Jaar))%>%
  mutate(Date = lubridate::ymd(lubridate::make_datetime(year = Jaar) + 
                                 lubridate::weeks(Week)))%>%
  ggplot(aes(x=Date))+
  geom_bar(aes(y=Overleden_0tot65-Verwacht_0tot65), stat="identity")+
  geom_line(aes(y=Overleden_0tot65))+
  geom_line(aes(y=Verwacht_0tot65), col="red", lty=2)+
  geom_ribbon(aes(y=Verwacht_0tot65,
                  ymin=Verwacht_0tot65_laag, 
                  ymax=Verwacht_0tot65_hoog, 
                  fill="Excess Mortality"), 
              alpha=0.2, show.legend = FALSE)+
  theme_bw()+
  labs(x="Date", 
       y="Mortality", 
       caption="Source: CBS", 
       title="Mortality and excess mortality in The Netherlands", 
       subtitle="For 0 tot 65 years of age")
g2<-Excess_mortality_age%>%
  mutate(Jaar=as.numeric(Jaar))%>%
  mutate(Date = lubridate::ymd(lubridate::make_datetime(year = Jaar) + 
                                 lubridate::weeks(Week)))%>%
  ggplot(aes(x=Date))+
  geom_bar(aes(y=Overleden_65tot80-Verwacht_65tot80), stat="identity")+
  geom_line(aes(y=Overleden_65tot80))+
  geom_line(aes(y=Verwacht_65tot80), col="red", lty=2)+
  geom_ribbon(aes(y=Verwacht_65tot80,
                  ymin=Verwacht_65tot80_laag, 
                  ymax=Verwacht_65tot80_hoog, 
                  fill="Excess Mortality"), 
              alpha=0.2, show.legend = FALSE)+
  theme_bw()+
  labs(x="Date", 
       y="Mortality", 
       subtitle="For 65 to 80 years of age")
g3<-Excess_mortality_age%>%
  mutate(Jaar=as.numeric(Jaar))%>%
  mutate(Date = lubridate::ymd(lubridate::make_datetime(year = Jaar) + 
                                 lubridate::weeks(Week)))%>%
  ggplot(aes(x=Date))+
  geom_bar(aes(y=Overleden_80enouder-Verwacht_80enouder), stat="identity")+
  geom_line(aes(y=Overleden_80enouder))+
  geom_line(aes(y=Verwacht_80enouder), col="red", lty=2)+
  geom_ribbon(aes(y=Verwacht_80enouder,
                  ymin=Verwacht_80enouder_laag, 
                  ymax=Verwacht_80enouder_hoog, 
                  fill="Excess Mortality"), 
              alpha=0.2, show.legend = FALSE)+
  theme_bw()+
  labs(x="Date", 
       y="Mortality", 
       subtitle="For 80 years and older")
gridExtra::grid.arrange(g1,g2,g3, ncol=1)



g1<-Excess_mortality_age%>%
  mutate(Jaar=as.numeric(Jaar))%>%
  mutate(Date = lubridate::ymd(lubridate::make_datetime(year = Jaar) + 
                                 lubridate::weeks(Week)))%>%
  ggplot(aes(x=Date))+
  geom_rect(aes(xmin=as.Date("2020-03-16"),
                xmax=as.Date("2020-07-01"),
                ymin=0,ymax=Inf),fill="lightgrey", alpha=0.1)+
  geom_rect(aes(xmin=as.Date("2020-03-16"),
                xmax=as.Date("2020-06-15"),
                ymin=0,ymax=Inf),fill="grey", alpha=0.1)+
  geom_rect(aes(xmin=as.Date("2020-03-16"),
                xmax=as.Date("2020-05-18"),
                ymin=0,ymax=Inf),fill="darkgrey", alpha=0.5)+
  geom_line(aes(y=Overleden_0tot65))+
  geom_line(aes(y=Verwacht_0tot65), col="red")+
  geom_ribbon(aes(y=Verwacht_0tot65,
                  ymin=Verwacht_0tot65_laag, 
                  ymax=Verwacht_0tot65_hoog, 
                  fill="Excess Mortality"), 
              alpha=0.2, show.legend = FALSE)+
  geom_segment(aes(x = as.Date("2021-01-06") , y = 0, 
                   xend = as.Date("2021-01-06"), yend = Inf), lty=2, col="darkgrey") +
  geom_text(aes(x=as.Date("2020-03-16"), y= 0, 
                label = "Stop screening"),
            family="Lato",
            colour = "darkred", 
            hjust = 0, size = 4, angle=90, 
            fontface="plain",check_overlap = TRUE)+
  geom_text(aes(x=as.Date("2020-05-18"), y= 0, 
                label = "Colon"), 
            family="Lato",
            colour = "darkgreen", 
            hjust = 0, size = 4, angle=90, 
            fontface="plain",check_overlap = TRUE)+
  geom_text(aes(x=as.Date("2020-06-15"), y= 0, 
                label = "Breast"), 
            family="Lato",
            colour = "darkgreen", 
            hjust = 0, size = 4, angle=90, 
            fontface="plain",check_overlap = TRUE)+
  geom_text(aes(x=as.Date("2020-07-01"), y= 0, 
                label = "Cervical"), 
            family="Lato",
            colour = "darkgreen", 
            hjust = 0, size = 4, angle=90, 
            fontface="plain",check_overlap = TRUE)+
  geom_text(aes(x=as.Date("2021-01-06"), y= 0, 
                label = "Start vaccines"),
            family="Lato",
            colour = "black",hjust=0, 
            vjust = -0.5, size = 4, angle=90, 
            fontface="plain",check_overlap = TRUE)+
  theme_bw()+
  labs(x="Date", 
       y="Mortality", 
       caption="Source: CBS", 
       title="Mortality and excess mortality in The Netherlands", 
       subtitle="For 0 tot 65 years of age")
g2<-Excess_mortality_age%>%
  mutate(Jaar=as.numeric(Jaar))%>%
  mutate(Date = lubridate::ymd(lubridate::make_datetime(year = Jaar) + 
                                 lubridate::weeks(Week)))%>%
  ggplot(aes(x=Date))+
  geom_rect(aes(xmin=as.Date("2020-03-16"),
                xmax=as.Date("2020-07-01"),
                ymin=0,ymax=Inf),fill="lightgrey", alpha=0.1)+
  geom_rect(aes(xmin=as.Date("2020-03-16"),
                xmax=as.Date("2020-06-15"),
                ymin=0,ymax=Inf),fill="grey", alpha=0.1)+
  geom_rect(aes(xmin=as.Date("2020-03-16"),
                xmax=as.Date("2020-05-18"),
                ymin=0,ymax=Inf),fill="darkgrey", alpha=0.5)+
  geom_line(aes(y=Overleden_65tot80))+
  geom_line(aes(y=Verwacht_65tot80), col="red")+
  geom_ribbon(aes(y=Verwacht_65tot80,
                  ymin=Verwacht_65tot80_laag, 
                  ymax=Verwacht_65tot80_hoog, 
                  fill="Excess Mortality"), 
              alpha=0.2, show.legend = FALSE)+
  geom_segment(aes(x = as.Date("2021-01-06") , y = 0, 
                   xend = as.Date("2021-01-06"), yend = Inf), lty=2, col="darkgrey") +
  geom_text(aes(x=as.Date("2020-03-16"), y= 0, 
                label = "Stop screening"),
            family="Lato",
            colour = "darkred", 
            hjust = 0, size = 4, angle=90, 
            fontface="plain",check_overlap = TRUE)+
  geom_text(aes(x=as.Date("2020-05-18"), y= 0, 
                label = "Colon"), 
            family="Lato",
            colour = "darkgreen", 
            hjust = 0, size = 4, angle=90, 
            fontface="plain",check_overlap = TRUE)+
  geom_text(aes(x=as.Date("2020-06-15"), y= 0, 
                label = "Breast"), 
            family="Lato",
            colour = "darkgreen", 
            hjust = 0, size = 4, angle=90, 
            fontface="plain",check_overlap = TRUE)+
  geom_text(aes(x=as.Date("2020-07-01"), y= 0, 
                label = "Cervical"), 
            family="Lato",
            colour = "darkgreen", 
            hjust = 0, size = 4, angle=90, 
            fontface="plain",check_overlap = TRUE)+
  geom_text(aes(x=as.Date("2021-01-06"), y= 0, 
                label = "Start vaccines"),
            family="Lato",
            colour = "black",hjust=0, 
            vjust = -0.5, size = 4, angle=90, 
            fontface="plain",check_overlap = TRUE)+
  theme_bw()+
  labs(x="Date", 
       y="Mortality", 
       subtitle="For 65 to 80 years of age")
g3<-Excess_mortality_age%>%
  mutate(Jaar=as.numeric(Jaar))%>%
  mutate(Date = lubridate::ymd(lubridate::make_datetime(year = Jaar) + 
                                 lubridate::weeks(Week)))%>%
  ggplot(aes(x=Date))+
  geom_rect(aes(xmin=as.Date("2020-03-16"),
                xmax=as.Date("2020-07-01"),
                ymin=0,ymax=Inf),fill="lightgrey", alpha=0.1)+
  geom_rect(aes(xmin=as.Date("2020-03-16"),
                xmax=as.Date("2020-06-15"),
                ymin=0,ymax=Inf),fill="grey", alpha=0.1)+
  geom_rect(aes(xmin=as.Date("2020-03-16"),
                xmax=as.Date("2020-05-18"),
                ymin=0,ymax=Inf),fill="darkgrey", alpha=0.5)+
  geom_line(aes(y=Overleden_80enouder))+
  geom_line(aes(y=Verwacht_80enouder), col="red")+
  geom_ribbon(aes(y=Verwacht_80enouder,
                  ymin=Verwacht_80enouder_laag, 
                  ymax=Verwacht_80enouder_hoog, 
                  fill="Excess Mortality"), 
              alpha=0.2, show.legend = FALSE)+
  geom_segment(aes(x = as.Date("2021-01-06") , y = 0, 
                   xend = as.Date("2021-01-06"), yend = Inf), lty=2, col="darkgrey") +
  geom_text(aes(x=as.Date("2020-03-16"), y= 0, 
                label = "Stop screening"),
            family="Lato",
            colour = "darkred", 
            hjust = 0, size = 4, angle=90, 
            fontface="plain",check_overlap = TRUE)+
  geom_text(aes(x=as.Date("2020-05-18"), y= 0, 
                label = "Colon"), 
            family="Lato",
            colour = "darkgreen", 
            hjust = 0, size = 4, angle=90, 
            fontface="plain",check_overlap = TRUE)+
  geom_text(aes(x=as.Date("2020-06-15"), y= 0, 
                label = "Breast"), 
            family="Lato",
            colour = "darkgreen", 
            hjust = 0, size = 4, angle=90, 
            fontface="plain",check_overlap = TRUE)+
  geom_text(aes(x=as.Date("2020-07-01"), y= 0, 
                label = "Cervical"), 
            family="Lato",
            colour = "darkgreen", 
            hjust = 0, size = 4, angle=90, 
            fontface="plain",check_overlap = TRUE)+
  geom_text(aes(x=as.Date("2021-01-06"), y= 0, 
                label = "Start vaccines"),
            family="Lato",
            colour = "black",hjust=0, 
            vjust = -0.5, size = 4, angle=90, 
            fontface="plain",check_overlap = TRUE)+
  theme_bw()+
  labs(x="Date", 
       y="Mortality", 
       subtitle="For 80 years and older")
gridExtra::grid.arrange(g1,g2,g3, ncol=1)



Excess_mortality%>%
  mutate(Date = lubridate::ymd(lubridate::make_datetime(year = Jaar) + 
                                 lubridate::weeks(Week)))%>%
  ggplot(aes(x=Date, 
             y=Overledenen))+
  geom_point()+
  geom_line()+
  geom_line(aes(y=Verwacht_aantal_overledenen), col="red")+
  geom_ribbon(aes(ymin=Verwacht_aantal_overledenen_laag, 
                  ymax=Verwacht_aantal_overledenen_hoog, 
                  fill="Excess Mortality"), 
              alpha=0.2, show.legend = FALSE)+
  theme_bw()+
  labs(x="Date", 
       y="Mortality", 
       caption="Source: CBS", 
       title="Mortality and excess mortality in The Netherlands")


EM<-Excess_mortality%>%
  mutate(Date = lubridate::ymd(lubridate::make_datetime(year = Jaar) + 
                                 lubridate::weeks(Week)))%>%
  mutate(month = lubridate::floor_date(Date, 'month'))%>%
  group_by(month)%>%
  summarise(Overledenen = sum(Overledenen), 
            Verwacht_aantal_overledenen=sum(Verwacht_aantal_overledenen), 
            Verwacht_aantal_overledenen_laag=sum(Verwacht_aantal_overledenen_laag),
            Verwacht_aantal_overledenen_hoog=sum(Verwacht_aantal_overledenen_hoog))
Covid_19<-read_excel("Data/CBS/doodsoorzaken-COVID_Marc.xlsx",
                                  sheet = "Vastgesteld_Vermoedelijk")%>%
  mutate(Date = zoo::as.yearmon(paste(.$Jaar, .$Maand), "%Y %m"))%>%
  select(-c(Totaal, Jaar, Maand))%>%
  rename(Confirmed=Vastgesteld, 
         Probable=Vermoedelijk)%>%
  mutate(month= lubridate::as_date(Date))

df<-EM%>%left_join(., Covid_19, by="month")%>%
  mutate(Excess=Overledenen-Verwacht_aantal_overledenen, 
         Non_Covid_19=Excess-(Confirmed+Probable))%>%
  select(month, Excess, Non_Covid_19)


cols <- c("Total mortality" = "black", "Covid-19 mortality" = "darkgrey",
          "Expected mortality" = "blue","Excess mortality" ="red")
ggplot()+
  geom_line(data=EM, aes(x=month, y=Overledenen, col="Total mortality"))+
  geom_point(data=EM, aes(x=month, y=Overledenen, col="Total mortality"))+
  geom_line(data=Covid_19, aes(x=month, y=Confirmed+Probable, col="Covid-19 mortality"))+
  geom_line(data=EM,aes(x=month,y=Verwacht_aantal_overledenen, col="Expected mortality"), lty=2)+
  geom_line(data=Covid_19, aes(x=month, y=Confirmed+Probable, col="Covid-19 mortality"))+
  geom_line(data=df, aes(x=month, y=Excess, col="Excess mortality"))+
  geom_point(data=df, aes(x=month, y=Excess, col="Excess mortality"))+
  geom_ribbon(data=EM,aes(x=month,ymin=Verwacht_aantal_overledenen_laag, 
                  ymax=Verwacht_aantal_overledenen_hoog, fill="Expected mortality"), 
              alpha=0.2, show.legend = FALSE)+
  geom_bar(data=df, aes(x=month, y=Non_Covid_19, fill="Covid-19 mortality"), 
           stat="identity", alpha=0.5, show.legend = FALSE)+
  geom_hline(yintercept = 0, col="black")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Date (months)", 
       y="Mortality",
       col="",
       fill="",
       caption="Source: CBS", 
       title="Mortality and excess mortality in The Netherlands", 
       subtitle="Barchart shows difference between excess mortality and covid-19 mortality: a positive bar shows the amount of exess mortality not attributable to covid-19")+
  scale_colour_manual(values = cols)+
  scale_fill_manual(values   = cols)
  
  















Excess_mortality_zorggebruikers <- read_csv("Data/CBS/Excess_mortality_zorggebruikers.csv")
str(Excess_mortality_zorggebruikers)
g1<-Excess_mortality_zorggebruikers%>%
  mutate(Jaar=as.numeric(Jaar))%>%
  mutate(Date = lubridate::ymd(lubridate::make_datetime(year = Jaar) + 
                                 lubridate::weeks(Week)))%>%
  ggplot(aes(x=Date))+
  geom_bar(aes(y=Wlzzorggebruikers-Wlzzorggebruikers_verwacht), stat="identity")+
  geom_line(aes(y=Wlzzorggebruikers))+
  geom_line(aes(y=Wlzzorggebruikers_verwacht), col="red", lty=2)+
  geom_ribbon(aes(y=Wlzzorggebruikers_verwacht,
                  ymin=Wlzzorggebruikers_laag, 
                  ymax=Wlzzorggebruikers_hoog, 
                  fill="Excess Mortality"), 
              alpha=0.2, show.legend = FALSE)+
  theme_bw()+
  labs(x="Date", 
       y="Mortality", 
       title="Mortality and excess mortality in The Netherlands", 
       subtitle="For WLZ health-care users")
g2<-Excess_mortality_zorggebruikers%>%
  mutate(Jaar=as.numeric(Jaar))%>%
  mutate(Date = lubridate::ymd(lubridate::make_datetime(year = Jaar) + 
                                 lubridate::weeks(Week)))%>%
  ggplot(aes(x=Date))+
  geom_bar(aes(y=Overigebevolking-Overigebevolking_verwacht), stat="identity")+
  geom_line(aes(y=Overigebevolking))+
  geom_line(aes(y=Overigebevolking_verwacht ), col="red", lty=2)+
  geom_ribbon(aes(y=Overigebevolking_verwacht,
                  ymin=Overigebevolking_laag, 
                  ymax=Overigebevolking_hoog, 
                  fill="Excess Mortality"), 
              alpha=0.2, show.legend = FALSE)+
  theme_bw()+
  labs(x="Date", 
       y="Mortality", 
       subtitle="For non-WLZ health-care users")
gridExtra::grid.arrange(g1,g2, ncol=1)



#### LOAD, WRANGLE AND EXPLORE OWID DATA   - EXCESS MORTALITY             - PROJECTED ----
excess_mortality_p_scores_projected_baseline <- read_csv("Data/OWID/Oversterfte/excess-mortality-p-scores-projected-baseline.csv")
dim(excess_mortality_p_scores_projected_baseline)
colnames(excess_mortality_p_scores_projected_baseline)
str(excess_mortality_p_scores_projected_baseline)

excess_mortality_p_scores_projected_baseline%>%
  filter(Entity=="Netherlands")%>%
  ggplot()+
  geom_point(aes(x=Day, y=p_proj_all_ages))+
  geom_line(aes(x=Day, y=p_proj_all_ages))+
  geom_hline(aes(yintercept=0), lty=2, col="red")+
  theme_bw()+
  scale_y_continuous(labels = scales::percent_format(scale=1, accuracy=1))+
  labs(x="Date", 
       y="Excess Mortality", 
       title="Excess Mortality (alle causes) compared to projection for The Netherlands",
       caption="Source: Our World in Data",
       subtitle="The percentage difference between the reported number of weekly or monthly deaths in 2020–2023 and the
projected number of deaths for the same period based on previous years.")


#### LOAD, WRANGLE AND EXPLORE OWID DATA   - EXCESS MORTALITY             - PROJECTED by AGE ----
excess_mortality_p_scores_projected_baseline_by_age <- read_csv("Data/OWID/Oversterfte/excess-mortality-p-scores-projected-baseline-by-age.csv")
colnames(excess_mortality_p_scores_projected_baseline_by_age)
dim(excess_mortality_p_scores_projected_baseline_by_age)

excess_mortality_p_scores_projected_baseline_by_age%>%
  pivot_longer(-c(Entity, Code, Day,p_proj_all_ages),
               names_to = "Leeftijd",
               values_to = "Value",
               values_drop_na = TRUE)%>%
  mutate(Leeftijd = substring(Leeftijd,8,12))%>%
  filter(Entity=="Netherlands")%>%
  ggplot()+
  geom_point(aes(x=Day, y=Value, col=Leeftijd), alpha=0.8)+
  geom_line(aes(x=Day, y=Value, col=Leeftijd), alpha=0.8)+
  geom_point(aes(x=Day, y=p_proj_all_ages), col="black")+
  geom_line(aes(x=Day, y=p_proj_all_ages), col="black")+
  geom_hline(aes(yintercept=0), lty=2, col="black")+
  scale_y_continuous(labels = scales::percent_format(scale=1, accuracy=1))+
  theme_bw()+
  labs(x="Date", 
       y="Excess Mortality", 
       title="Excess Mortality (alle causes) compared to projection for The Netherlands",
       col="Age category",
       caption="Source: Our World in Data",
       subtitle="The percentage difference between the reported number of weekly or monthly deaths in 2020–2023 and the
projected number of deaths for the same period based on previous years.")+
  theme(legend.position = "bottom")

excess_mortality_p_scores_projected_baseline_by_age%>%
  pivot_longer(-c(Entity, Code, Day,p_proj_all_ages),
               names_to = "Leeftijd",
               values_to = "Value",
               values_drop_na = TRUE)%>%
  mutate(Leeftijd = substring(Leeftijd,8,12))%>%
  filter(Entity=="Netherlands")%>%
  ggplot()+
  geom_point(aes(x=Day, y=Value, col=Leeftijd), alpha=0.6, show.legend = FALSE)+
  geom_line(aes(x=Day, y=Value, col=Leeftijd), alpha=0.6, show.legend = FALSE)+
  geom_point(aes(x=Day, y=p_proj_all_ages), col="black")+
  geom_line(aes(x=Day, y=p_proj_all_ages), col="black")+
  facet_grid(~Leeftijd)+
  theme_bw()+
  geom_hline(aes(yintercept=0), lty=2, col="black")+
  scale_y_continuous(labels = scales::percent_format(scale=1, accuracy=1))+
  theme_bw()+
  labs(x="Date", 
       y="Excess Mortality", 
       title="Excess Mortality (alle causes) compared to projection for The Netherlands",
       col="Age category",
       caption="Source: Our World in Data",
       subtitle="The percentage difference between the reported number of weekly or monthly deaths in 2020–2023 and the
projected number of deaths for the same period based on previous years.")+
  theme(legend.position = "bottom")

excess_mortality_p_scores_projected_baseline_by_age%>%
  pivot_longer(-c(Entity, Code, Day,p_proj_all_ages),
               names_to = "Leeftijd",
               values_to = "Value",
               values_drop_na = TRUE)%>%
  mutate(Leeftijd = substring(Leeftijd,8,12))%>%
  filter(Entity=="Netherlands")%>%
  ggplot()+
  geom_bar(aes(x=Day, y=Value, fill=Leeftijd), stat="identity", show.legend = FALSE)+
  geom_bar(aes(x=Day, y=p_proj_all_ages), fill="black", alpha=0.6, stat="identity")+
  facet_grid(~Leeftijd)+
  theme_bw()+
  geom_hline(aes(yintercept=0), lty=2, col="black")+
  scale_y_continuous(labels = scales::percent_format(scale=1, accuracy=1))+
  theme_bw()+
  labs(x="Date", 
       y="Excess Mortality", 
       title="Excess Mortality (alle causes) compared to projection for The Netherlands",
       col="Age category",
       caption="Source: Our World in Data",
       subtitle="The percentage difference between the reported number of weekly or monthly deaths in 2020–2023 and the
projected number of deaths for the same period based on previous years.")+
  theme(legend.position = "bottom")

#### LOAD, WRANGLE AND EXPLORE OWID DATA   - EXCESS MORTALITY             - ECONOMIST DAILY----
excess_deaths_daily_economist_single_entity <- read_csv("Data/OWID/Oversterfte/excess-deaths-daily-economist-single-entity.csv")
colnames(excess_deaths_daily_economist_single_entity)
names_new<-c("Entity", "Code", "Day", "Estimated", "Estimated_top", "Estimated_bottom", "Confirmed")
colnames(excess_deaths_daily_economist_single_entity)<-names_new
str(excess_deaths_daily_economist_single_entity)

excess_deaths_daily_economist_single_entity%>%
  filter(Entity=="Netherlands")%>%
  ggplot()+
  geom_line(aes(x=Day, y=Confirmed, col="Confirmed"))+
  theme_bw()+
  labs(x="Date", 
       y="Mortality", 
       col="Legend")


excess_deaths_daily_economist_single_entity%>%
  filter(Entity=="Netherlands")%>%
  ggplot()+
  geom_point(aes(x=Day, y=Confirmed))

excess_deaths_daily_economist_single_entity%>%
  filter(Entity=="Netherlands")%>%
  ggplot()+
  geom_point(aes(x=Day, y=Estimated_bottom))

excess_deaths_daily_economist_single_entity%>%
  filter(Entity=="Netherlands")%>%
  ggplot()+
  geom_point(aes(x=Day, y=Estimated, col="estimated"))+
  geom_point(aes(x=Day, y=Estimated_bottom, col="bottom"))+
  geom_point(aes(x=Day, y=Estimated_top, col="top"))


geom_ribbon(aes(ymin=Estimated_low, ymax=Estimated_high), alpha=0.4)+
  theme_bw()+
  labs(x="Date", 
       y="Excess deaths", 
       col="Legend")


excess_deaths_daily_economist_single_entity%>%
  filter(Entity=="Netherlands")%>%
  ggplot(.)+
  geom_point(aes(x=Day, y=Estimated, col="Estimated"))+
  geom_line(aes(x=Day, y=Estimated, col="Estimated", group=1))+
  geom_point(aes(x=Day, y=Confirmed, col="Confirmed"))+
  geom_line(aes(x=Day, y=Confirmed, col="Confirmed", group=1))+
  theme_bw()+
  labs(x="Date", 
       y="Excess deaths", 
       col="Legend")






#### LOAD, WRANGLE AND EXPLORE OWID DATA   - EXCESS MORTALITY             - CUMULATIVE WHO & ECONOMIST ----
excess_deaths_cumulative_economist_who <- read_csv("Data/OWID/Oversterfte/excess-deaths-cumulative-economist-who.csv")
colnames(excess_deaths_cumulative_economist_who)
names_new<-c("Entity", "Code", "Day", "WHO_mean", "WHO_high", 
             "WHO_low", "Economist_central", "Economist_low", "Economist_high", "Confirmed")
colnames(excess_deaths_cumulative_economist_who)<-names_new
str(excess_deaths_cumulative_economist_who)

excess_deaths_cumulative_economist_who%>%
  filter(Entity=="Netherlands")%>%
  ggplot()+
  geom_point(aes(x=Day, y=WHO_mean, col="WHO"))+
  geom_point(aes(x=Day, y=Economist_central, col="Economist"))+
  geom_point(aes(x=Day, y=Confirmed, col="Confirmed"))+
  theme_bw()+
  labs(x="Date", 
       y="Cumulative excess deaths",
       col="Source",
       title="Estimated cumulative excess deaths, from The Economist and
the WHO, Netherlands",
caption="Source: The Economist (2022); WHO COVID-19 Dashboard",
subtitle="Cumulative difference between the number of reported or estimated deaths in 2020–2021 and the projected
number of deaths for the same period based on previous years. Estimates differ because the models differ in the
data and methods used.")





#### COMPARE WMD, CBS & OWID Data          - MORTALITY  ----
CBS_EM <- read_csv("Data/CBS/Excess_mortality.csv")%>%
  mutate(Date = lubridate::ymd(lubridate::make_datetime(year = Jaar) + 
                                 lubridate::weeks(Week)))%>%
  select(-c(Jaar, Week, Verwacht_aantal_overledenen_laag, Verwacht_aantal_overledenen_hoog))%>%
  mutate(ED=Overledenen-Verwacht_aantal_overledenen)%>%
  mutate(ED_p=((Overledenen-Verwacht_aantal_overledenen)/Verwacht_aantal_overledenen)*100)%>%print(n=50)
OWID_EM<-read_csv("Data/OWID/Oversterfte/excess-mortality-p-scores-projected-baseline.csv")%>%
  filter(Entity=="Netherlands")%>%
  select(-c(Entity, Code))%>%
  rename(ED_p=p_proj_all_ages)%>%
  rename(Date=Day)
WMD_EM<-read_csv("Data/Dkobak_WMD/excess-mortality-timeseries.csv")%>%
  filter(country_name=="Netherlands")%>%
  mutate(Date = lubridate::ymd(lubridate::make_datetime(year = year) + 
                                 lubridate::weeks(time)))%>%
  select(-c(year, time, country_name, time_unit))%>%
  rename(ED=`excess deaths`)
str(CBS_EM)
str(OWID_EM)
str(WMD_EM)

g1<-ggplot()+
  geom_hline(yintercept = 0, lty=2, col="black")+
  geom_line(data=CBS_EM, aes(x=Date, y=ED, col="CBS"))+
  geom_line(data=WMD_EM, aes(x=Date, y=ED, col="WMD"))+
  theme_bw()+
  labs(x="Date", 
       y="Excess mortality (#)", 
       col="Source", 
       title="Comparing excess mortality metrics for The Netherlands",
       caption="Source: Central Bureau of Statistics Netherlands & World Mortality Data")
g2<-ggplot()+
  geom_hline(yintercept = 0, lty=2, col="black")+
  geom_line(data=CBS_EM, aes(x=Date, y=ED_p, col="CBS"))+
  geom_line(data=OWID_EM, aes(x=Date, y=ED_p, col="OWID"))+
  theme_bw()+
  labs(x="Date", 
       y="Excess mortality (%)", 
       col="Source", 
       caption="Source: Central Bureau of Statistics Netherlands & Our World in Data")
gridExtra::grid.arrange(g1,g2,ncol=1)


#### LOAD, WRANGLE AND EXPLORE CBS DATA    - CAUSES of DEATH              - COVID-19 ----
Covid_19_vastgesteld<- read_excel("Data/CBS/doodsoorzaken-COVID_Marc.xlsx",
                                       sheet = "Vastgesteld_Vermoedelijk")
colnames(Covid_19_vastgesteld)

Covid_19_vastgesteld%>%
  mutate(Date = zoo::as.yearmon(paste(.$Jaar, .$Maand), "%Y %m"))%>%
  select(-c(Totaal, Jaar, Maand))%>%
  rename(Confirmed=Vastgesteld, 
         Probable=Vermoedelijk)%>%
  tidyr::pivot_longer(-Date,
             names_to = "Registration",
             values_to = "Deaths",
             values_drop_na = TRUE)%>%
  ggplot()+
  geom_bar(aes(x=Date, y=Deaths, fill=Registration), stat="identity")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Date", 
       y="Deaths", 
       title="Mortality in the Netherlands due to Covid-19", 
       caption="Source: CBS")



Covid_19_Sterfte <- read_excel("Data/CBS/doodsoorzaken-COVID_Marc.xlsx", 
                                       sheet = "Sterfte")
colnames(Covid_19_Sterfte)
colnames(Covid_19_Sterfte)[4]<-"Covid19"

Covid_19_Sterfte%>%
  mutate(Date = lubridate::ymd(lubridate::make_datetime(year = Jaar) + 
                               lubridate::weeks(Week)))%>%
  select(Date, Totaal, Covid19)%>%
  mutate(NonCovid19=Totaal-Covid19)%>%
  select(-Totaal)%>%
  tidyr::pivot_longer(-Date,
                      names_to = "Reason",
                      values_to = "Deaths",
                      values_drop_na = TRUE)%>%
  ggplot()+
  geom_area(aes(x=Date, y=Deaths, fill=Reason))+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Date", 
       y="Deaths", 
       title="Mortality in the Netherlands due to Covid-19", 
       caption="Source: CBS")

Covid_19_Sterfte_Geslacht_Leeftijd <- read_excel("Data/CBS/doodsoorzaken-COVID_Marc.xlsx", 
                                       sheet = "Covid_Sterfte")
colnames(Covid_19_Sterfte_Geslacht_Leeftijd)
colnames(Covid_19_Sterfte_Geslacht_Leeftijd)[5]<-"0_64"
colnames(Covid_19_Sterfte_Geslacht_Leeftijd)[6]<-"65-79"
colnames(Covid_19_Sterfte_Geslacht_Leeftijd)[7]<-"80-older"
Covid_19_Sterfte_Geslacht_Leeftijd%>%
  mutate(Date = zoo::as.yearmon(paste(.$Jaar, .$Maand), "%Y %m"))%>%
  select(-c(Jaar, Maand, Totaal))%>%
  tidyr::pivot_longer(-c(Date,Geslacht),
                    names_to = "Age",
                    values_to = "Deaths",
                    values_drop_na = TRUE)%>%
  arrange(Age, Geslacht, Date)%>%
  ggplot()+
  geom_bar(aes(x=Date, y=Deaths, fill=Age), stat="identity")+
  theme_bw()+
  facet_grid(~Geslacht)+
  theme(legend.position = "bottom")+
  labs(x="Date", 
       y="Deaths", 
       title="Mortality in the Netherlands due to Covid-19 by Age and Sex", 
       caption="Source: CBS")

Covid_19_Sterfte_Geslacht_Leeftijd%>%
  mutate(Date = zoo::as.yearmon(paste(.$Jaar, .$Maand), "%Y %m"))%>%
  select(-c(Jaar, Maand, Totaal))%>%
  tidyr::pivot_longer(-c(Date,Geslacht),
                      names_to = "Age",
                      values_to = "Deaths",
                      values_drop_na = TRUE)%>%
  arrange(Age, Geslacht, Date)%>%
  ggplot()+
  geom_bar(aes(x=Date, y=Deaths, fill=Geslacht), stat="identity")+
  theme_bw()+
  facet_grid(~Age)+
  theme(legend.position = "bottom")+
  labs(x="Date", 
       y="Deaths", 
       title="Mortality in the Netherlands due to Covid-19 by Age and Sex", 
       caption="Source: CBS")

 

Zorggebruikers <- read_excel("Data/CBS/doodsoorzaken-COVID_Marc.xlsx", 
                                       sheet = "Covid_Sterfte_zorggebruikers")
colnames(Zorggebruikers)
colnames(Zorggebruikers)[4]<-"Niet"
colnames(Zorggebruikers)[5]<-"Wel"
Zorggebruikers%>%
  mutate(Date = lubridate::ymd(lubridate::make_datetime(year = Jaar) + 
                                 lubridate::weeks(Week)))%>%
  select(-c(Jaar, Week))%>%
  tidyr::pivot_longer(-c(Date),
                      names_to = "Long_term_care_user",
                      values_to = "Deaths",
                      values_drop_na = TRUE)%>%
  filter(Long_term_care_user=="Totaal")%>%
  ggplot()+
  geom_line(aes(x=Date, y=Deaths))+
  theme_bw()+
  scale_y_continuous(labels = scales::percent_format(scale=100, accuracy=1))+
  labs(x="Date", 
       y="(%)", 
       title="Percentage of total mortality in the Netherlands due to Covid-19", 
       caption="Source: CBS")

Zorggebruikers%>%
  mutate(Date = lubridate::ymd(lubridate::make_datetime(year = Jaar) + 
                                 lubridate::weeks(Week)))%>%
  select(-c(Jaar, Week))%>%
  rename(non_WLZ=Niet, 
         WLZ=Wel)%>%
  tidyr::pivot_longer(-c(Date),
                      names_to = "Long_term_care_user",
                      values_to = "Deaths",
                      values_drop_na = TRUE)%>%
  filter(!Long_term_care_user=="Totaal")%>%
  ggplot()+
  geom_area(aes(x=Date, y=Deaths, fill=Long_term_care_user))+
  theme_bw()+
  scale_y_continuous(labels = scales::percent_format(scale=100, accuracy=1))+
  labs(x="Date", 
       y="(%)",
       fill="Health care user",
       title="Percentage of Covid mortality in the Netherlands",
       subtitle="for long-term (WLZ) and non-long-term health-care users",
       caption="Source: CBS")+
  theme(legend.position = "bottom")


  


#### LOAD, WRANGLE AND EXPLORE CBS DATA    - CAUSES of DEATH -----
Doodsoorzaken <- read_delim("Data/CBS/Doodsoorzaken.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
colnames(Doodsoorzaken)
table(Doodsoorzaken$Leeftijd)
table(Doodsoorzaken$Geslacht)
hist(Doodsoorzaken$GemiddeldeBevolking_96)
Doodsoorzaken%>%
  mutate(Perioden = substring(Perioden,1,4))%>%
  mutate(Geslacht = replace(Geslacht, 
                            Geslacht == 3000, "Man"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 4000, "Women"))%>%
  filter(Geslacht=="Women")%>%
  filter(Perioden=="2016")%>%
  group_by(Leeftijd)%>%
  select(TotaalInfectieuzeEnParasitaireZktn_2)%>%
  print(n=22)
Doodsoorzaken$Leeftijd<-as.factor(Doodsoorzaken$Leeftijd)
levels(Doodsoorzaken$Leeftijd)
leeftijd_names<-c("Total all ages", "0", "95 year or older", "1 tot 5", 
                  "5 tot 10", "10 tot 15", 
                  "15 tot 20", "20 tot 25", "25 tot 30", "30 tot 35",
                  "35 tot 40", "40 tot 45", "45 tot 50", "50 tot 55",
                  "55 tot 60", "60 tot 65", "65 tot 70", "70 tot 75",
                  "75 tot 80", "80 tot 85", "85 tot 90", "90 tot 95")
levels(Doodsoorzaken$Leeftijd)<-leeftijd_names
str(Doodsoorzaken)
Doodsoorzaken$Leeftijd<-forcats::fct_relevel(Doodsoorzaken$Leeftijd, 
                     "Total all ages", "0", "1 tot 5", 
                     "5 tot 10", "10 tot 15", 
                     "15 tot 20", "20 tot 25", "25 tot 30", "30 tot 35",
                     "35 tot 40", "40 tot 45", "45 tot 50", "50 tot 55",
                     "55 tot 60", "60 tot 65", "65 tot 70", "70 tot 75",
                     "75 tot 80", "80 tot 85", "85 tot 90", "90 tot 95",
                     "95 year or older")
str(Doodsoorzaken$Leeftijd)


Doodsoorzaken%>%
  mutate(Perioden = substring(Perioden,1,4))%>%
  select(-c(TotaalOnderliggendeDoodsoorzaken_1))%>%
  mutate(Geslacht = replace(Geslacht, 
                          Geslacht == 3000, "Man"),
       Geslacht = replace(Geslacht, 
                          Geslacht == 4000, "Women"))%>%
  filter(Geslacht=="T001038")%>%
  filter(Leeftijd=="Total all ages")%>%
  mutate(Leeftijd=factor(Leeftijd))%>%
  mutate(k_18TotaalCOVID19Coronavirus19_93 = tidyr::replace_na(k_18TotaalCOVID19Coronavirus19_93,0))%>%
  rowwise(Perioden)%>%
  mutate(Total = sum(c(TotaalInfectieuzeEnParasitaireZktn_2,TotaalKwaadaardigeNieuwvormingen_9,
           k_22OverigeNieuwvormingen_30,
           k_3ZktnBloedBloedvormendeOrganenEn_31,
           TotaalEndocrieneVoedingsStofwiss_32,
           TotaalPsychischeStoornissen_35,
           TotaalZiektenZenuwstelselEnZintuigen_39,
           TotaalZiektenVanHartEnVaatstelsel_43,
           TotaalZiektenVanDeAdemhalingsorganen_50, 
           TotaalZiektenSpijsverteringsorganen_57,
           k_10ZiektenVanHuidEnSubcutis_63,
           TotaalZiektenSpierenBeendBindwfsl_64,
           TotaalZktnUrinewegenEnGeslOrganen_67,
           k_13ZwangerschapBevallingEnKraambed_70,
           k_14AandoeningenVDPerinatalePeriode_71,
           TotaalAangeborenAfwijkingen_72,
           TotaalSympEnOnvollOmschrZiekte_76,
           TotaalUitwendigeDoodsoorzaken_80,
           k_18TotaalCOVID19Coronavirus19_93)))%>%
  select(Perioden, Total)%>%
  mutate(Period=if_else(Perioden>2019, "Post-Covid", "Pre-Covid"))%>%
  ungroup()%>%
  mutate(Total_lag  = dplyr::lag(.$Total, n = 1), 
         Change=Total-Total_lag, 
         Total_MA5 = zoo::rollmean(x = .$Total, 5, align = "right", fill = NA))%>%
  ggplot()+
  geom_point(aes(x=Perioden, 
                 y=Total, 
                 col=Period))+
  geom_line(aes(x=Perioden, 
                 y=Total, 
                 col=Period, 
                group=1))+
  geom_line(aes(x=Perioden, 
                y=Total_MA5), 
            col="black", lty=2, 
            group=1)+
  geom_bar(aes(x=Perioden, 
               y=Change), 
           stat="Identity", 
           fill="grey")+
  theme_bw()+
  theme(legend.position="bottom", 
      axis.text.x = element_text(angle = 45, vjust = 0.5))+
  labs(title = "Total mortality in the Netherlands",
       subtitle="Across sex, all ages and by period",
       caption="Source: CBS",
       x="Year",
       y="#", 
       fill="Cause")



Doodsoorzaken%>%
  mutate(Perioden = substring(Perioden,1,4))%>%
  select(-c(TotaalOnderliggendeDoodsoorzaken_1))%>%
  mutate(Geslacht = replace(Geslacht, 
                            Geslacht == 3000, "Man"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 4000, "Women"))%>%
  filter(!Geslacht=="T001038")%>%
  filter(Leeftijd=="Total all ages")%>%
  mutate(Leeftijd=factor(Leeftijd))%>%
  mutate(k_18TotaalCOVID19Coronavirus19_93 = tidyr::replace_na(k_18TotaalCOVID19Coronavirus19_93,0))%>%
  rowwise(Perioden)%>%
  mutate(Total = sum(c(TotaalInfectieuzeEnParasitaireZktn_2,TotaalKwaadaardigeNieuwvormingen_9,
                       k_22OverigeNieuwvormingen_30,
                       k_3ZktnBloedBloedvormendeOrganenEn_31,
                       TotaalEndocrieneVoedingsStofwiss_32,
                       TotaalPsychischeStoornissen_35,
                       TotaalZiektenZenuwstelselEnZintuigen_39,
                       TotaalZiektenVanHartEnVaatstelsel_43,
                       TotaalZiektenVanDeAdemhalingsorganen_50, 
                       TotaalZiektenSpijsverteringsorganen_57,
                       k_10ZiektenVanHuidEnSubcutis_63,
                       TotaalZiektenSpierenBeendBindwfsl_64,
                       TotaalZktnUrinewegenEnGeslOrganen_67,
                       k_13ZwangerschapBevallingEnKraambed_70,
                       k_14AandoeningenVDPerinatalePeriode_71,
                       TotaalAangeborenAfwijkingen_72,
                       TotaalSympEnOnvollOmschrZiekte_76,
                       TotaalUitwendigeDoodsoorzaken_80,
                       k_18TotaalCOVID19Coronavirus19_93), na.rm=TRUE))%>%
  select(Perioden, Geslacht, Total)%>%
  mutate(Period=if_else(Perioden>2019, "Post-Covid", "Pre-Covid"))%>%
  ungroup()%>%
  mutate(Total_lag  = dplyr::lag(.$Total, n = 1), 
         Change=Total-Total_lag, 
         Total_MA5 = zoo::rollmean(x = .$Total, 5, align = "right", fill = NA))%>%
  filter(Perioden>1990)%>%
  ggplot()+
  geom_point(aes(x=Perioden, 
                 y=Total, 
                 col=Period))+
  geom_line(aes(x=Perioden, 
                y=Total, 
                col=Period, 
                group=1))+
  geom_line(aes(x=Perioden, 
                y=Total_MA5), 
            col="black", lty=2, 
            group=1)+
  geom_bar(aes(x=Perioden, 
               y=Change), 
           stat="Identity", 
           fill="grey")+
  facet_grid(~Geslacht)+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))+
  labs(title = "Total mortality in the Netherlands",
       subtitle="Across sex, all ages and by period from 1990 onwards",
       caption="Source: CBS",
       x="Year",
       y="#", 
       fill="Cause")

Doodsoorzaken%>%
  mutate(Perioden = substring(Perioden,1,4))%>%
  select(-c(TotaalOnderliggendeDoodsoorzaken_1))%>%
  mutate(Geslacht = replace(Geslacht, 
                            Geslacht == 3000, "Man"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 4000, "Women"))%>%
  filter(!Geslacht=="T001038")%>%
  filter(Leeftijd=="Total all ages")%>%
  mutate(Leeftijd=factor(Leeftijd))%>%
  mutate(k_18TotaalCOVID19Coronavirus19_93 = tidyr::replace_na(k_18TotaalCOVID19Coronavirus19_93,0))%>%
  select(Perioden, Geslacht,TotaalInfectieuzeEnParasitaireZktn_2,TotaalKwaadaardigeNieuwvormingen_9,
                       k_22OverigeNieuwvormingen_30,
                       k_3ZktnBloedBloedvormendeOrganenEn_31,
                       TotaalEndocrieneVoedingsStofwiss_32,
                       TotaalPsychischeStoornissen_35,
                       TotaalZiektenZenuwstelselEnZintuigen_39,
                       TotaalZiektenVanHartEnVaatstelsel_43,
                       TotaalZiektenVanDeAdemhalingsorganen_50, 
                       TotaalZiektenSpijsverteringsorganen_57,
                       k_10ZiektenVanHuidEnSubcutis_63,
                       TotaalZiektenSpierenBeendBindwfsl_64,
                       TotaalZktnUrinewegenEnGeslOrganen_67,
                       k_13ZwangerschapBevallingEnKraambed_70,
                       k_14AandoeningenVDPerinatalePeriode_71,
                       TotaalAangeborenAfwijkingen_72,
                       TotaalSympEnOnvollOmschrZiekte_76,
                       TotaalUitwendigeDoodsoorzaken_80,
                       k_18TotaalCOVID19Coronavirus19_93)%>%
  pivot_longer(cols = -c(Perioden,Geslacht),
               names_to = "Oorzaak",
               values_to = "Aantal",
               values_drop_na = TRUE)%>%
  mutate(Oorzaak = tolower(Oorzaak))%>%
  mutate(Oorzaak = gsub("totaal", "", Oorzaak))%>%
  mutate(Oorzaak = gsub("[[:digit:]]", "", Oorzaak))%>%
  mutate(Oorzaak = sub("k_", "", Oorzaak))%>%
  mutate(Oorzaak = sub("_", "", Oorzaak))%>%
  ggplot(., 
         aes(x=Perioden, 
             y=Aantal, 
             fill=Oorzaak))+
  geom_bar(stat="identity")+
  facet_wrap(~Geslacht)+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))+
  labs(title = "Cause of death in the Netherlands",
       caption="Source: CBS",
       x="Year",
       y="#", 
       fill="Cause")


cols <- c("Yes" = "darkred", "No" = "grey")
Doodsoorzaken%>%
  mutate(Perioden = substring(Perioden,1,4))%>%
  select(-c(TotaalOnderliggendeDoodsoorzaken_1))%>%
  mutate(Geslacht = replace(Geslacht, 
                            Geslacht == 3000, "Man"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 4000, "Women"))%>%
  filter(!Geslacht=="T001038")%>%
  filter(Leeftijd=="Total all ages")%>%
  mutate(Leeftijd=factor(Leeftijd))%>%
  mutate(k_18TotaalCOVID19Coronavirus19_93 = tidyr::replace_na(k_18TotaalCOVID19Coronavirus19_93,0))%>%
  select(Perioden, Geslacht,TotaalInfectieuzeEnParasitaireZktn_2,TotaalKwaadaardigeNieuwvormingen_9,
         k_22OverigeNieuwvormingen_30,
         k_3ZktnBloedBloedvormendeOrganenEn_31,
         TotaalEndocrieneVoedingsStofwiss_32,
         TotaalPsychischeStoornissen_35,
         TotaalZiektenZenuwstelselEnZintuigen_39,
         TotaalZiektenVanHartEnVaatstelsel_43,
         TotaalZiektenVanDeAdemhalingsorganen_50, 
         TotaalZiektenSpijsverteringsorganen_57,
         k_10ZiektenVanHuidEnSubcutis_63,
         TotaalZiektenSpierenBeendBindwfsl_64,
         TotaalZktnUrinewegenEnGeslOrganen_67,
         k_13ZwangerschapBevallingEnKraambed_70,
         k_14AandoeningenVDPerinatalePeriode_71,
         TotaalAangeborenAfwijkingen_72,
         TotaalSympEnOnvollOmschrZiekte_76,
         TotaalUitwendigeDoodsoorzaken_80,
         k_18TotaalCOVID19Coronavirus19_93)%>%
  pivot_longer(cols = -c(Perioden,Geslacht),
               names_to = "Oorzaak",
               values_to = "Aantal",
               values_drop_na = TRUE)%>%
  mutate(Oorzaak = tolower(Oorzaak))%>%
  mutate(Oorzaak = gsub("totaal", "", Oorzaak))%>%
  mutate(Oorzaak = gsub("[[:digit:]]", "", Oorzaak))%>%
  mutate(Oorzaak = sub("k_", "", Oorzaak))%>%
  mutate(Oorzaak = sub("_", "", Oorzaak))%>%
  mutate(Covid=if_else(Oorzaak=="covidcoronavirus", "Yes", "No"))%>%
  ggplot(., 
         aes(x=Perioden, 
             y=Aantal, 
             fill=Covid))+
  geom_bar(stat="identity")+
  facet_wrap(~Geslacht)+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))+
  scale_fill_manual(values = cols)+
  labs(title = "Cause of death in the Netherlands",
       subtitle="Split by Covid and non-Covid across all ages",
       caption="Source: CBS",
       x="Year",
       y="#", 
       fill="Cause")

cols <- c("Covid" = "darkred", "Flue" ="darkgreen", "Other" = "grey")
Doodsoorzaken%>%
  mutate(Perioden = substring(Perioden,1,4))%>%
  select(-c(TotaalOnderliggendeDoodsoorzaken_1))%>%
  mutate(Geslacht = replace(Geslacht, 
                            Geslacht == 3000, "Man"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 4000, "Women"))%>%
  filter(!Geslacht=="T001038")%>%
  filter(Leeftijd=="Total all ages")%>%
  mutate(Leeftijd=factor(Leeftijd))%>%
  mutate(k_18TotaalCOVID19Coronavirus19_93 = tidyr::replace_na(k_18TotaalCOVID19Coronavirus19_93,0))%>%
  select(Perioden, Geslacht,TotaalInfectieuzeEnParasitaireZktn_2,TotaalKwaadaardigeNieuwvormingen_9,
         k_22OverigeNieuwvormingen_30,
         k_3ZktnBloedBloedvormendeOrganenEn_31,
         TotaalEndocrieneVoedingsStofwiss_32,
         TotaalPsychischeStoornissen_35,
         TotaalZiektenZenuwstelselEnZintuigen_39,
         TotaalZiektenVanHartEnVaatstelsel_43,
         TotaalZiektenVanDeAdemhalingsorganen_50, 
         TotaalZiektenSpijsverteringsorganen_57,
         k_10ZiektenVanHuidEnSubcutis_63,
         TotaalZiektenSpierenBeendBindwfsl_64,
         TotaalZktnUrinewegenEnGeslOrganen_67,
         k_13ZwangerschapBevallingEnKraambed_70,
         k_14AandoeningenVDPerinatalePeriode_71,
         TotaalAangeborenAfwijkingen_72,
         TotaalSympEnOnvollOmschrZiekte_76,
         TotaalUitwendigeDoodsoorzaken_80,
         k_18TotaalCOVID19Coronavirus19_93,
         k_81Griep_51)%>%
  pivot_longer(cols = -c(Perioden,Geslacht),
               names_to = "Oorzaak",
               values_to = "Aantal",
               values_drop_na = TRUE)%>%
  mutate(Oorzaak = tolower(Oorzaak))%>%
  mutate(Oorzaak = gsub("totaal", "", Oorzaak))%>%
  mutate(Oorzaak = gsub("[[:digit:]]", "", Oorzaak))%>%
  mutate(Oorzaak = sub("k_", "", Oorzaak))%>%
  mutate(Oorzaak = sub("_", "", Oorzaak))%>%
  mutate(Cause=if_else(Oorzaak=="covidcoronavirus", "Covid", 
                       if_else(Oorzaak=="griep", "Flue", 
                               "Other")))%>%
  ggplot(., 
         aes(x=Perioden, 
             y=Aantal, 
             fill=Cause))+
  geom_bar(stat="identity")+
  facet_wrap(~Geslacht)+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))+
  scale_fill_manual(values = cols)+
  labs(title = "Cause of death in the Netherlands",
       subtitle="Split by Covid, Flue and other across all ages",
       caption="Source: CBS",
       x="Year",
       y="#", 
       fill="Cause")


cols <- c("Covid" = "darkred", "Flue" ="darkgreen", "Cancer"="darkorange", "Other" = "grey")
Doodsoorzaken%>%
  mutate(Perioden = substring(Perioden,1,4))%>%
  select(-c(TotaalOnderliggendeDoodsoorzaken_1))%>%
  mutate(Geslacht = replace(Geslacht, 
                            Geslacht == 3000, "Man"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 4000, "Women"))%>%
  filter(!Geslacht=="T001038")%>%
  filter(Leeftijd=="Total all ages")%>%
  mutate(Leeftijd=factor(Leeftijd))%>%
  mutate(k_18TotaalCOVID19Coronavirus19_93 = tidyr::replace_na(k_18TotaalCOVID19Coronavirus19_93,0))%>%
  select(Perioden, Geslacht,TotaalInfectieuzeEnParasitaireZktn_2,TotaalKwaadaardigeNieuwvormingen_9,
         k_22OverigeNieuwvormingen_30,
         k_3ZktnBloedBloedvormendeOrganenEn_31,
         TotaalEndocrieneVoedingsStofwiss_32,
         TotaalPsychischeStoornissen_35,
         TotaalZiektenZenuwstelselEnZintuigen_39,
         TotaalZiektenVanHartEnVaatstelsel_43,
         TotaalZiektenVanDeAdemhalingsorganen_50, 
         TotaalZiektenSpijsverteringsorganen_57,
         k_10ZiektenVanHuidEnSubcutis_63,
         TotaalZiektenSpierenBeendBindwfsl_64,
         TotaalZktnUrinewegenEnGeslOrganen_67,
         k_13ZwangerschapBevallingEnKraambed_70,
         k_14AandoeningenVDPerinatalePeriode_71,
         TotaalAangeborenAfwijkingen_72,
         TotaalSympEnOnvollOmschrZiekte_76,
         TotaalUitwendigeDoodsoorzaken_80,
         k_18TotaalCOVID19Coronavirus19_93,
         k_81Griep_51)%>%
  pivot_longer(cols = -c(Perioden,Geslacht),
               names_to = "Oorzaak",
               values_to = "Aantal",
               values_drop_na = TRUE)%>%
  mutate(Oorzaak = tolower(Oorzaak))%>%
  mutate(Oorzaak = gsub("totaal", "", Oorzaak))%>%
  mutate(Oorzaak = gsub("[[:digit:]]", "", Oorzaak))%>%
  mutate(Oorzaak = sub("k_", "", Oorzaak))%>%
  mutate(Oorzaak = sub("_", "", Oorzaak))%>%
  mutate(Cause=if_else(Oorzaak=="covidcoronavirus", "Covid", 
                       if_else(Oorzaak=="griep", "Flue",
                               if_else(Oorzaak=="kwaadaardigenieuwvormingen", "Cancer",
                               "Other"))))%>%
    ggplot(., 
         aes(x=Perioden, 
             y=Aantal, 
             fill=Cause))+
  geom_bar(stat="identity")+
  facet_wrap(~Geslacht)+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))+
  scale_fill_manual(values = cols)+
  labs(title = "Cause of death in the Netherlands",
       subtitle="Split by Covid, Flue, Cancer and other across all ages",
       caption="Source: CBS",
       x="Year",
       y="#", 
       fill="Cause")



cols <- c("Covid" = "darkred", "Flue" ="darkgreen", "Cancer"="darkorange", "Other" = "grey")
Doodsoorzaken%>%
  mutate(Perioden = substring(Perioden,1,4))%>%
  select(-c(TotaalOnderliggendeDoodsoorzaken_1))%>%
  mutate(Geslacht = replace(Geslacht, 
                            Geslacht == 3000, "Man"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 4000, "Women"))%>%
  filter(!Geslacht=="T001038")%>%
  filter(Leeftijd=="Total all ages")%>%
  mutate(Leeftijd=factor(Leeftijd))%>%
  mutate(k_18TotaalCOVID19Coronavirus19_93 = tidyr::replace_na(k_18TotaalCOVID19Coronavirus19_93,0))%>%
  select(Perioden, Geslacht,
         TotaalKwaadaardigeNieuwvormingen_9,
         k_18TotaalCOVID19Coronavirus19_93,
         k_81Griep_51)%>%
  pivot_longer(cols = -c(Perioden,Geslacht),
               names_to = "Oorzaak",
               values_to = "Aantal",
               values_drop_na = TRUE)%>%
  mutate(Oorzaak = tolower(Oorzaak))%>%
  mutate(Oorzaak = gsub("totaal", "", Oorzaak))%>%
  mutate(Oorzaak = gsub("[[:digit:]]", "", Oorzaak))%>%
  mutate(Oorzaak = sub("k_", "", Oorzaak))%>%
  mutate(Oorzaak = sub("_", "", Oorzaak))%>%
  mutate(Cause=if_else(Oorzaak=="covidcoronavirus", "Covid", 
                       if_else(Oorzaak=="griep", "Flue",
                               if_else(Oorzaak=="kwaadaardigenieuwvormingen", "Cancer",
                                       "Other"))))%>%
  ggplot(., 
         aes(x=Perioden, 
             y=Aantal, 
             col=Cause))+
  geom_vline(xintercept="2020", lty=2, col="black")+
  geom_point()+
  geom_line(aes(group=Cause))+
  facet_wrap(~Geslacht)+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))+
  scale_colour_manual(values = cols)+
  labs(title = "Cause of death in the Netherlands",
       subtitle="Split by Covid, Flue, Cancer and other across all ages",
       caption="Source: CBS",
       x="Year",
       y="#", 
       fill="Cause")













Doodsoorzaken%>%
  mutate(Perioden = substring(Perioden,1,4))%>%
  mutate(Geslacht = replace(Geslacht, 
                            Geslacht == 3000, "Man"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 4000, "Women"))%>%
  filter(!Geslacht=="T001038")%>%
  filter(Leeftijd=="Total all ages")%>%
  mutate(Leeftijd=factor(Leeftijd))%>%
  select(-c(ID, Leeftijd))%>%
  pivot_longer(cols = -c(Perioden,Geslacht),
               names_to = "Oorzaak",
               values_to = "Aantal",
               values_drop_na = TRUE)%>%
  filter(grepl('Totaal', Oorzaak))%>%
  mutate(Oorzaak = substring(Oorzaak, 7))%>%
  mutate(Oorzaak = gsub("[[:digit:]]", "", Oorzaak))%>%
  mutate(Oorzaak = sub("_", "", Oorzaak))%>%
  mutate(Oorzaak = sub("taal", "", Oorzaak))%>%
  mutate(Covid=if_else(Oorzaak=="COVIDCoronavirus", "Yes", "No"))%>%
  ggplot(., 
         aes(x=Perioden, 
             y=Aantal, 
             fill=Covid))+
  geom_bar(stat="identity")+
  facet_wrap(~Geslacht)+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))+
  labs(title = "Cause of death in the Netherlands",
       caption="Source: CBS",
       x="Year",
       y="#", 
       fill="Cause")




Doodsoorzaken%>%
  mutate(Perioden = substring(Perioden,1,4))%>%
  mutate(Geslacht = replace(Geslacht, 
                            Geslacht == 3000, "Man"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 4000, "Women"))%>%
  filter(!Geslacht=="T001038")%>%
  filter(Leeftijd=="Total all ages")%>%
  mutate(Leeftijd=factor(Leeftijd))%>%
  select(-c(ID, Leeftijd))%>%
  pivot_longer(cols = -c(Perioden,Geslacht),
               names_to = "Oorzaak",
               values_to = "Aantal",
               values_drop_na = TRUE)%>%
  filter(grepl('COVID', Oorzaak))%>%
  mutate(Oorzaak = gsub("[[:digit:k]]", "", Oorzaak))%>%
  mutate(Oorzaak = sub("_", "", Oorzaak))%>%
  ggplot(., 
         aes(x=Perioden, 
             y=Aantal, 
             fill=Oorzaak))+
  geom_bar(stat="identity")+
  facet_wrap(~Geslacht)+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))+
  labs(title = "Cause of death in the Netherlands",
       caption="Source: CBS",
       x="Year",
       y="#", 
       fill="Cause")




Doodsoorzaken%>%
  mutate(Perioden = substring(Perioden,1,4))%>%
  mutate(Geslacht = replace(Geslacht, 
                            Geslacht == 3000, "Man"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 4000, "Women"))%>%
  filter(!Geslacht=="T001038")%>%
  filter(!Leeftijd=="Total all ages")%>%
  mutate(Leeftijd=factor(Leeftijd))%>%
  arrange(desc(Leeftijd), .by_group = TRUE)%>%
  ggplot(aes(x=Perioden, fill=TotaalOnderliggendeDoodsoorzaken_1, y=factor(Leeftijd)))+
  geom_tile()+
  facet_grid(~Geslacht)+
  scale_fill_viridis_c(option="turbo")+
  theme_bw()+
  labs(x="Year", 
       y="Age category",
       fill="Death",
       title="Total number of death with underlying cause in the Netherlands", 
       caption="Source: CBS")+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))

Doodsoorzaken%>%
  mutate(Perioden = substring(Perioden,1,4))%>%
  mutate(Geslacht = replace(Geslacht, 
                            Geslacht == 3000, "Man"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 4000, "Women"))%>%
  filter(!Geslacht=="T001038")%>%
  filter(!Leeftijd=="Total all ages")%>%
  mutate(Leeftijd=factor(Leeftijd))%>%
  arrange(desc(Leeftijd), .by_group = TRUE)%>%
  ggplot(aes(x=Perioden, fill=k_162OnvollOmschrEnOnbekOorzaken_78, y=factor(Leeftijd)))+
  geom_tile()+
  facet_grid(~Geslacht)+
  scale_fill_viridis_c(option="turbo")+
  theme_bw()+
  labs(x="Year", 
       y="Age category",
       fill="Death",
       title="Total number of death not clearly described in the Netherlands",
       subtitle = "Onvolledig Omschreven en Onbekende Oorzaken",
       caption="Source: CBS")+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))


Doodsoorzaken%>%
  mutate(Perioden = substring(Perioden,1,4))%>%
  mutate(Geslacht = replace(Geslacht, 
                            Geslacht == 3000, "Man"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 4000, "Women"))%>%
  filter(!Geslacht=="T001038")%>%
  filter(!Leeftijd=="Total all ages")%>%
  mutate(Leeftijd=factor(Leeftijd))%>%
  arrange(desc(Leeftijd), .by_group = TRUE)%>%
  ggplot(aes(x=Perioden, fill=TotaalKwaadaardigeNieuwvormingen_9, y=factor(Leeftijd)))+
  geom_tile()+
  facet_wrap(~Geslacht, ncol=1)+
  scale_fill_viridis_c(option="turbo")+
  theme_bw()+
  labs(x="Year", 
       y="Age category",
       fill="Death",
       title="Total number of death by cancer in the Netherlands",
       subtitle = "Totaal Kwaadaardige Nieuwvormingen",
       caption="Source: CBS")+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))

Doodsoorzaken%>%
  mutate(Perioden = substring(Perioden,1,4))%>%
  mutate(Geslacht = replace(Geslacht, 
                            Geslacht == 3000, "Man"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 4000, "Women"))%>%
  filter(!Geslacht=="T001038")%>%
  filter(!Leeftijd=="Total all ages")%>%
  mutate(Leeftijd=factor(Leeftijd))%>%
  arrange(desc(Leeftijd), .by_group = TRUE)%>%
  ggplot(aes(x=Perioden, fill=k_214KwNvVanDikkeDarm_13, y=factor(Leeftijd)))+
  geom_tile()+
  facet_grid(~Geslacht)+
  scale_fill_viridis_c(option="turbo")+
  theme_bw()+
  labs(x="Year", 
       y="Age category",
       fill="Death",
       title="Number of death by colon cancer in the Netherlands",
       subtitle = "Kwaadaardige Nieuwvormingen Dikke Darm",
       caption="Source: CBS")+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))

Doodsoorzaken%>%
  mutate(Perioden = substring(Perioden,1,4))%>%
  mutate(Geslacht = replace(Geslacht, 
                            Geslacht == 3000, "Man"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 4000, "Women"))%>%
  filter(Geslacht=="Women")%>%
  filter(!Leeftijd=="Total all ages")%>%
  mutate(Leeftijd=factor(Leeftijd))%>%
  arrange(desc(Leeftijd), .by_group = TRUE)%>%
  ggplot(aes(x=Perioden, fill=k_2112KwNvVanBorst_21, y=factor(Leeftijd)))+
  geom_tile()+
  scale_fill_viridis_c(option="turbo")+
  theme_bw()+
  labs(x="Year", 
       y="Age category",
       fill="Death",
       title="Number of death by breast cancer in the Netherlands",
       subtitle = "Kwaadaardige Nieuwvormingen Borst",
       caption="Source: CBS")+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))

Doodsoorzaken%>%
  mutate(Perioden = substring(Perioden,1,4))%>%
  mutate(Geslacht = replace(Geslacht, 
                            Geslacht == 3000, "Man"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 4000, "Women"))%>%
  filter(Geslacht=="Women")%>%
  filter(!Leeftijd=="Total all ages")%>%
  mutate(Leeftijd=factor(Leeftijd))%>%
  arrange(desc(Leeftijd), .by_group = TRUE)%>%
  ggplot(aes(x=Perioden, fill=k_2113KwNvVanBaarmoederhals_22, y=factor(Leeftijd)))+
  geom_tile()+
  scale_fill_viridis_c(option="turbo")+
  theme_bw()+
  labs(x="Year", 
       y="Age category",
       fill="Death",
       title="Number of death by cervical cancer in the Netherlands",
       subtitle = "Kwaadaardige Nieuwvormingen Baarmoederhals",
       caption="Source: CBS")+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))

Doodsoorzaken%>%
  mutate(Perioden = substring(Perioden,1,4))%>%
  mutate(Geslacht = replace(Geslacht, 
                            Geslacht == 3000, "Man"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 4000, "Women"))%>%
  filter(Geslacht=="Women")%>%
  filter(!Leeftijd=="Total all ages")%>%
  mutate(Leeftijd=factor(Leeftijd))%>%
  arrange(desc(Leeftijd), .by_group = TRUE)%>%
  ggplot(aes(x=Perioden, fill=k_2113KwNvVanBaarmoederhals_22, y=factor(Leeftijd)))+
  geom_tile()+
  scale_fill_viridis_c(option="turbo")+
  theme_bw()+
  labs(x="Year", 
       y="Age category",
       fill="Death",
       title="Number of death by cervical cancer in the Netherlands",
       subtitle = "Kwaadaardige Nieuwvormingen Baarmoederhalskanker",
       caption="Source: CBS")+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))

Doodsoorzaken%>%
  mutate(Perioden = substring(Perioden,1,4))%>%
  mutate(Geslacht = replace(Geslacht, 
                            Geslacht == 3000, "Man"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 4000, "Women"))%>%
  filter(!Geslacht=="T001038")%>%
  filter(!Leeftijd=="Total all ages")%>%
  mutate(Leeftijd=factor(Leeftijd))%>%
  arrange(desc(Leeftijd), .by_group = TRUE)%>%
  ggplot(aes(x=Perioden, fill=k_81Griep_51 , y=factor(Leeftijd)))+
  geom_tile()+
  facet_grid(~Geslacht)+
  scale_fill_viridis_c(option="turbo")+
  theme_bw()+
  labs(x="Year", 
       y="Age category",
       fill="Death",
       title="Total number of death by influenza in the Netherlands",
       subtitle = "Griep",
       caption="Source: CBS")+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))

Doodsoorzaken%>%
  mutate(Perioden = substring(Perioden,1,4))%>%
  mutate(Geslacht = replace(Geslacht, 
                            Geslacht == 3000, "Man"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 4000, "Women"))%>%
  filter(!Geslacht=="T001038")%>%
  filter(!Leeftijd=="Total all ages")%>%
  filter(Perioden>2018)%>%
  mutate(Leeftijd=factor(Leeftijd))%>%
  arrange(desc(Leeftijd), .by_group = TRUE)%>%
  select(Perioden, Leeftijd, Geslacht,k_18TotaalCOVID19Coronavirus19_93, 
         k_181VastgesteldeCOVID19_94,k_182VermoedelijkeCOVID19_95, k_81Griep_51)%>%
  pivot_longer(-c(Perioden, Geslacht, Leeftijd),
               names_to = "Uitkomst",
               values_to = "Value",
               values_drop_na = TRUE)%>%
  mutate(Uitkomst = replace(Uitkomst, 
                            Uitkomst == "k_18TotaalCOVID19Coronavirus19_93", "Total Covid-19"),
         Uitkomst = replace(Uitkomst, 
                            Uitkomst == "k_181VastgesteldeCOVID19_94", "Confirmed Covid-19"),
         Uitkomst = replace(Uitkomst, 
                            Uitkomst == "k_182VermoedelijkeCOVID19_95", "Suspected Covid-19"),
         Uitkomst = replace(Uitkomst, 
                            Uitkomst == "k_81Griep_51", "Influenza"))%>%
  ggplot(aes(x=Leeftijd, col=Geslacht , y=Value))+
  geom_point()+
  geom_line(aes(group=Geslacht))+
  facet_grid(Perioden~Uitkomst)+
  theme_bw()+
  labs(x="Year", 
       y="Age category",
       fill="Death",
       title="Total number of death by Covid-19 and influenza in the Netherlands",
       subtitle = "Confirmed, Suspected and Total Covid-19",
       caption="Source: CBS")+
  theme(legend.position="bottom")


Doodsoorzaken%>%
  mutate(Perioden = substring(Perioden,1,4))%>%
  mutate(Geslacht = replace(Geslacht, 
                            Geslacht == 3000, "Man"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 4000, "Women"))%>%
  filter(!Geslacht=="T001038")%>%
  filter(!Leeftijd=="Total all ages")%>%
  filter(Perioden>2018)%>%
  mutate(Leeftijd=factor(Leeftijd))%>%
  arrange(desc(Leeftijd), .by_group = TRUE)%>%
  select(Perioden, Leeftijd, Geslacht,k_18TotaalCOVID19Coronavirus19_93, 
         k_181VastgesteldeCOVID19_94,k_182VermoedelijkeCOVID19_95, k_81Griep_51)%>%
  pivot_longer(-c(Perioden, Geslacht, Leeftijd),
               names_to = "Uitkomst",
               values_to = "Value",
               values_drop_na = TRUE)%>%
  mutate(Uitkomst = replace(Uitkomst, 
                            Uitkomst == "k_18TotaalCOVID19Coronavirus19_93", "Total Covid-19"),
         Uitkomst = replace(Uitkomst, 
                            Uitkomst == "k_181VastgesteldeCOVID19_94", "Confirmed Covid-19"),
         Uitkomst = replace(Uitkomst, 
                            Uitkomst == "k_182VermoedelijkeCOVID19_95", "Suspected Covid-19"),
         Uitkomst = replace(Uitkomst, 
                            Uitkomst == "k_81Griep_51", "Influenza"))%>%
  filter(Leeftijd%in%c("50 tot 55",
                       "55 tot 60", "60 tot 65", "65 tot 70", "70 tot 75",
                       "75 tot 80", "80 tot 85", "85 tot 90", "90 tot 95",
                       "95 year or older"))%>%
  ggplot(aes(x=Leeftijd, col=Geslacht , y=Value))+
  geom_point()+
  geom_line(aes(group=Geslacht))+
  facet_grid(Perioden~Uitkomst)+
  theme_bw()+
  labs(x="Year", 
       y="Age category",
       fill="Death",
       title="Total number of death by Covid-19 and influenza in the Netherlands",
       subtitle = "Confirmed, Suspected and Total Covid-19 from ages 50 onwards",
       caption="Source: CBS")+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))


Doodsoorzaken%>%
  mutate(Perioden = substring(Perioden,1,4))%>%
  mutate(Geslacht = replace(Geslacht, 
                            Geslacht == 3000, "Man"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 4000, "Women"))%>%
  filter(!Geslacht=="T001038")%>%
  filter(!Leeftijd=="Total all ages")%>%
  filter(Perioden>2018)%>%
  mutate(Leeftijd=factor(Leeftijd))%>%
  arrange(desc(Leeftijd), .by_group = TRUE)%>%
  select(Perioden, Leeftijd, Geslacht,k_18TotaalCOVID19Coronavirus19_93, 
         k_181VastgesteldeCOVID19_94,k_182VermoedelijkeCOVID19_95, k_81Griep_51)%>%
  pivot_longer(-c(Perioden, Geslacht, Leeftijd),
               names_to = "Uitkomst",
               values_to = "Value",
               values_drop_na = TRUE)%>%
  mutate(Uitkomst = replace(Uitkomst, 
                            Uitkomst == "k_18TotaalCOVID19Coronavirus19_93", "Total Covid-19"),
         Uitkomst = replace(Uitkomst, 
                            Uitkomst == "k_181VastgesteldeCOVID19_94", "Confirmed Covid-19"),
         Uitkomst = replace(Uitkomst, 
                            Uitkomst == "k_182VermoedelijkeCOVID19_95", "Suspected Covid-19"),
         Uitkomst = replace(Uitkomst, 
                            Uitkomst == "k_81Griep_51", "Influenza"))%>%
  filter(Leeftijd%in%c("50 tot 55",
                       "55 tot 60", "60 tot 65", "65 tot 70", "70 tot 75",
                       "75 tot 80", "80 tot 85", "85 tot 90", "90 tot 95",
                       "95 year or older"))%>%
  ggplot(aes(x=Leeftijd, col=Uitkomst , y=Value))+
  geom_point()+
  geom_line(aes(group=Uitkomst))+
  facet_grid(Perioden~Geslacht)+
  theme_bw()+
  labs(x="Year", 
       y="Age category",
       fill="Death",
       title="Total number of death by Covid-19 and influenza in the Netherlands",
       subtitle = "Confirmed, Suspected and Total Covid-19 from ages 50 onwards",
       caption="Source: CBS")+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))




#### LOAD, WRANGLE AND EXPLORE VEKTIS DATA - POSTCODE3 ----
cpath<-"C:/Users/marcj/Documents/MSJ/Projects/2023/ZonMW/UitgesteldeZorg/Data/VEKTIS"
list_of_files <- list.files(path = cpath, 
                            pattern = "\\- postcode3.csv$", 
                            full.names = TRUE)
names(list_of_files)<-seq(2011,2021,1)
df1<- plyr::ldply(list_of_files[1:8],
                  data.table::fread, 
                  .id='JAAR') 
df2<- plyr::ldply(list_of_files[9:11],
                  data.table::fread, 
                  .id='jaar')
colnames(df1);colnames(df2)
names(df1) <- tolower(names(df1))
df2<-df2%>%mutate(kosten_tweedelijns_ggz=NA)
setdiff(df1, df2)
df1<-df1%>%mutate(kosten_gzsp=NA)
df2<-df2%>%mutate(kosten_eerstelijns_psychologische_zorg=NA)
setdiff(df1, df2)
dim(df1)
dim(df2)
df1<-df1[,order(colnames(df1))]
df2<-df2[,order(colnames(df2))]
colnames(df1)
colnames(df2)
df<-rbind(df1, df2)
dim(df)
colnames(df)

vis_miss(df)
gg_miss_upset(df)
n_var_miss(df)
gg_miss_upset(df, nsets = n_var_miss(df))

table(df$jaar)
table(df$geslacht)
table(df$leeftijdsklasse)
table(df$postcode_3)
table(df$aantal_verzekerdejaren); hist(log(as.numeric((df$aantal_verzekerdejaren))))


df%>%
  filter(geslacht =="M" | geslacht =="V")%>%
  replace(is.na(.), 0)%>%
  group_by(jaar)%>%
  summarise(total = sum(c_across(kosten_eerstelijns_ondersteuning:
                          kosten_ziekenvervoer_zittend))/1000000000)%>%
  ggplot()+
  geom_bar(aes(x=jaar, 
                y=total), 
           stat="identity")+
  labs(x="Year", 
       y="Cost (billions)",
       title ="Total expenditure per year",
       caption="Source: VEKTIS")+
  theme_bw()

df%>%
  filter(geslacht =="M" | geslacht =="V")%>%
  replace(is.na(.), 0)%>%
  group_by(jaar, geslacht)%>%
  summarise(total = sum(c_across(kosten_eerstelijns_ondersteuning:
                                   kosten_ziekenvervoer_zittend))/1000000000)%>%
  ggplot()+
  geom_bar(aes(x=jaar, 
               y=total, 
               fill=geslacht), 
           stat="identity", position="dodge")+
  labs(x="Year", 
       y="Cost (billions)",
       fill="Sex",
       title ="Total expenditure per year",
       caption="Source: VEKTIS")+
  theme_bw()

df%>%
  filter(geslacht =="M" | geslacht =="V")%>%
  mutate(jaar=factor(jaar), 
         geslacht=factor(geslacht),
         leeftijdsklasse=factor(leeftijdsklasse))%>%
  replace(is.na(.), 0)%>%
  mutate(total = rowSums(pick(where(is.numeric), 
                              -c(aantal_bsn, aantal_verzekerdejaren, postcode_3))))%>%
  mutate(average=total/aantal_bsn)%>%
  ggplot()+
  geom_tile(aes(x=jaar,
                y=postcode_3, 
                fill=average))+
  facet_grid(~geslacht)+
  scale_fill_viridis_c(option="turbo")+
  labs(x="Year", 
       y="Postal code",
       fill="Average cost (euro's)",
       title ="Average expenditure",
       subtitle ="Per postal code, per year, per sex",
       caption="Source: VEKTIS")+
  theme_bw()

df%>%
  filter(geslacht =="M" | geslacht =="V")%>%
  mutate(jaar=factor(jaar), 
         geslacht=factor(geslacht),
         leeftijdsklasse=factor(leeftijdsklasse))%>%
  replace(is.na(.), 0)%>%
  mutate(total = rowSums(pick(where(is.numeric), 
                              -c(aantal_bsn, aantal_verzekerdejaren, postcode_3))))%>%
  rename(Sex=geslacht)%>%
  ggplot()+
  geom_boxplot(aes(x=jaar,
                y=log(total/1000000), 
                col=Sex))+
  labs(x="Year", 
       y="Total cost (millions - log scale)",
       title ="Total expenditure",
       subtitle ="Per year, per sex",
       caption="Source: VEKTIS")+
  theme_bw()


df%>%
  filter(geslacht =="M" | geslacht =="V")%>%
  mutate(jaar=factor(jaar), 
         geslacht=factor(geslacht),
         leeftijdsklasse=factor(leeftijdsklasse))%>%
  replace(is.na(.), 0)%>%
  mutate(total = rowSums(pick(where(is.numeric), 
                              -c(aantal_bsn, aantal_verzekerdejaren, postcode_3))))%>%
  rename(Sex=geslacht)%>%
  mutate(age_cut = cut(as.numeric(leeftijdsklasse), 5))%>%
  ggplot()+
  geom_boxplot(aes(x=jaar,
                   y=log(total/1000000), 
                   fill=age_cut))+
  labs(x="Year", 
       y="Total cost (millions - log scale)",
       fill="Age category",
       title ="Total expenditure",
       subtitle ="Per year, per age",
       caption="Source: VEKTIS")+
  theme_bw()


df%>%
  filter(geslacht =="M" | geslacht =="V")%>%
  mutate(jaar=factor(jaar), 
         geslacht=factor(geslacht),
         leeftijdsklasse=factor(leeftijdsklasse))%>%
  replace(is.na(.), 0)%>%
  mutate(total = rowSums(pick(where(is.numeric), 
                              -c(aantal_bsn, aantal_verzekerdejaren, postcode_3))))%>%
  rename(Sex=geslacht)%>%
  mutate(age_cut = cut(as.numeric(leeftijdsklasse), 5))%>%
  ggplot()+
  geom_boxplot(aes(x=jaar,
                   y=log(total/1000000), 
                   fill=age_cut))+
  facet_grid(~age_cut)+
  labs(x="Year", 
       y="Total cost (millions - log scale)",
       fill="Age category",
       title ="Total expenditure",
       subtitle ="Per year, per age",
       caption="Source: VEKTIS")+
  theme_bw()+
  theme(legend.position="bottom", 
      axis.text.x = element_text(angle = 45, vjust = 0.5))



df%>%
  filter(geslacht =="M" | geslacht =="V")%>%
  mutate(jaar=factor(jaar), 
         geslacht=factor(geslacht),
         leeftijdsklasse=factor(leeftijdsklasse))%>%
  replace(is.na(.), 0)%>%
  mutate(total = rowSums(pick(where(is.numeric), 
                              -c(aantal_bsn, aantal_verzekerdejaren, postcode_3))))%>%
  mutate(average=total/aantal_bsn)%>%
  select(c(jaar, geslacht, leeftijdsklasse, average))%>%
  pivot_longer(-c(jaar, geslacht,leeftijdsklasse),
               names_to = "Average",
               values_to = "Euro",
               values_drop_na = TRUE)%>%
  mutate(age_cut = cut(as.numeric(leeftijdsklasse), 10))%>%
  group_by(age_cut, jaar, geslacht)%>%
  summarise(ave_cost=mean(Euro))%>%
  ggplot()+
  geom_bar(aes(y=age_cut,
                x=ave_cost, 
                fill=geslacht), 
           stat="identity", 
           position="dodge")+
  facet_grid(~jaar, scales="free")+
  labs(y="Age category",
       x="Average cost (euro's)",
       fill="sex",
       title ="Average expenditure",
       subtitle ="Per person, year and sex",
       caption="Source: VEKTIS")+
  theme_bw()


df%>%
  filter(geslacht =="M" | geslacht =="V")%>%
  mutate(jaar=factor(jaar), 
         geslacht=factor(geslacht),
         leeftijdsklasse=factor(leeftijdsklasse))%>%
  replace(is.na(.), 0)%>%
  mutate(total = rowSums(pick(where(is.numeric), 
                              -c(aantal_bsn, aantal_verzekerdejaren, postcode_3))))%>%
  mutate(average=total/aantal_bsn)%>%
  select(c(jaar, geslacht, leeftijdsklasse, average))%>%
  pivot_longer(-c(jaar, geslacht,leeftijdsklasse),
               names_to = "Average",
               values_to = "Euro",
               values_drop_na = TRUE)%>%
  select(-Average)%>%
  tidyr::drop_na()%>%
  mutate(geslacht=as.character(geslacht), 
         jaar_num=as.numeric(as.character(jaar)), 
         leeftijdsklasse=as.numeric(as.character(leeftijdsklasse)))%>%
  group_by(leeftijdsklasse, jaar_num, geslacht)%>%
  summarise(ave_cost=mean(Euro))%>%
  ungroup()%>%
  as.data.frame()%>%
  ggplot()+
  geom_line(aes(x=leeftijdsklasse,
                y=ave_cost,
                col=factor(jaar_num),
                group=factor(jaar_num)))+
  facet_wrap(~geslacht, ncol=1)+
  gghighlight::gghighlight(jaar_num>2018, 
                           calculate_per_facet=TRUE,
                           label_key = jaar_num)+
  labs(x="Age",
       y="Average cost (euro's)",
       coll="year",
       title ="Average expenditure",
       subtitle ="Per person, year and sex",
       caption="Source: VEKTIS")+
  theme_bw()


df%>%
  filter(geslacht =="M" | geslacht =="V")%>%
  mutate(jaar=factor(jaar), 
         geslacht=factor(geslacht),
         leeftijdsklasse=factor(leeftijdsklasse))%>%
  replace(is.na(.), 0)%>%
  mutate(total = rowSums(pick(where(is.numeric), 
                              -c(aantal_bsn, aantal_verzekerdejaren, postcode_3))))%>%
  mutate(average=total/aantal_bsn)%>%
  select(c(jaar, geslacht, leeftijdsklasse, average))%>%
  pivot_longer(-c(jaar, geslacht,leeftijdsklasse),
               names_to = "Average",
               values_to = "Euro",
               values_drop_na = TRUE)%>%
  group_by(leeftijdsklasse, jaar, geslacht)%>%
  summarise(ave_cost=mean(Euro))%>%
  ungroup()%>%
  ggplot()+
  geom_point(aes(x=as.numeric(as.character(leeftijdsklasse)),
                 y=ave_cost, 
                 col=geslacht))+
  geom_line(aes(x=as.numeric(as.character(leeftijdsklasse)),
                y=ave_cost, 
                col=geslacht, 
                group=geslacht))+
  facet_grid(~jaar)+
  labs(x="Age",
       y="Average cost (euro's)",
       coll="year",
       title ="Average expenditure",
       subtitle ="Per person, year and sex",
       caption="Source: VEKTIS")+
  theme_bw()


df%>%
  filter(geslacht =="M" | geslacht =="V")%>%
  mutate(jaar=factor(jaar), 
         geslacht=factor(geslacht),
         leeftijdsklasse=factor(leeftijdsklasse))%>%
  replace(is.na(.), 0)%>%
  select(-c(aantal_bsn, aantal_verzekerdejaren))%>%
  pivot_longer(-c(jaar, geslacht,leeftijdsklasse),
               names_to = "Zorg",
               values_to = "Euro",
               values_drop_na = TRUE)%>%
  tidyr::drop_na()%>%
  mutate(geslacht=as.character(geslacht), 
         jaar_num=as.numeric(as.character(jaar)), 
         leeftijdsklasse=as.numeric(as.character(leeftijdsklasse)))%>%
  filter(!Zorg%in%c("kosten_gzsp", 
                    "kosten_tweedelijns_ggz", 
                    "kosten_eerstelijns_psychologische_zorg", 
                    "kosten_eerstelijns_ondersteuning", 
                    "kosten_eerstelijnsverblijf", 
                    "kosten_langdurige_ggz"))%>%
  ggplot()+
  geom_line(aes(x=leeftijdsklasse,
                y=Euro,
                col=Zorg,
                group=Zorg))+
  gghighlight::gghighlight(max(Euro),  label_key = zorg)+
  facet_grid(geslacht~jaar)+
  labs(x="Age",
       y="Average cost (euro's)",
       coll="year",
       title ="Average expenditure",
       subtitle ="Per person, year and sex",
       caption="Source: VEKTIS")+
  theme_bw()+
  theme(legend.position="bottom")


df%>%
  filter(geslacht =="M" | geslacht =="V")%>%
  select(-c(aantal_verzekerdejaren))%>%
  pivot_longer(-c(jaar, geslacht, postcode_3, leeftijdsklasse, aantal_bsn),
               names_to = "Zorg",
               values_to = "Euro",
               values_drop_na = TRUE)%>%
  group_by(jaar, Zorg)%>%
  summarise(total = sum(Euro)/1000000000)%>%
  filter(!Zorg%in%c("kosten_gzsp", 
                   "kosten_tweedelijns_ggz", 
                   "kosten_eerstelijns_psychologische_zorg", 
                   "kosten_eerstelijns_ondersteuning", 
                   "kosten_eerstelijnsverblijf", 
                   "kosten_langdurige_ggz"))%>%
  ggplot()+
  facet_wrap(~Zorg, ncol=4, scales="free")+
  geom_bar(aes(x=jaar, 
               y=total), 
           stat="identity")+
  labs(x="Year", 
       y="Cost (billions)",
       title ="Total expenditure per year per type of care",
       subtitle ="Excluding various types of mental care",
       caption="Source: VEKTIS")+
  theme_bw()


df%>%
  filter(geslacht =="M" | geslacht =="V")%>%
  select(-c(aantal_verzekerdejaren))%>%
  pivot_longer(-c(jaar, geslacht, postcode_3, leeftijdsklasse, aantal_bsn),
               names_to = "Zorg",
               values_to = "Euro",
               values_drop_na = TRUE)%>%
  group_by(jaar, Zorg, geslacht)%>%
  summarise(total = sum(Euro)/1000000000)%>%
  filter(!Zorg%in%c("kosten_gzsp", 
                    "kosten_tweedelijns_ggz", 
                    "kosten_eerstelijns_psychologische_zorg", 
                    "kosten_eerstelijns_ondersteuning", 
                    "kosten_eerstelijnsverblijf", 
                    "kosten_langdurige_ggz"))%>%
  ggplot()+
  geom_tile(aes(x=jaar, 
                y=Zorg, 
                fill=total))+
  facet_grid(~geslacht)+
  scale_fill_viridis_c(option="turbo")+
  labs(x="Year", 
       y="Type of care",
       fill="Cost (billions)",
       title ="Total expenditure per year, per sex, per type of care",
       subtitle ="Excluding various types of mental care",
       caption="Source: VEKTIS")+
  theme_bw()

df%>%
  filter(geslacht =="M" | geslacht =="V")%>%
  select(-c(aantal_verzekerdejaren))%>%
  pivot_longer(-c(jaar, geslacht, postcode_3, leeftijdsklasse, aantal_bsn),
               names_to = "Zorg",
               values_to = "Euro",
               values_drop_na = TRUE)%>%
  group_by(jaar, Zorg, geslacht)%>%
  summarise(total = sum(Euro)/1000000000)%>%
  filter(!Zorg%in%c("kosten_medisch_specialistische_zorg"))%>%
  ggplot()+
  geom_tile(aes(x=jaar, 
                y=Zorg, 
                fill=total))+
  facet_grid(~geslacht)+
  scale_fill_viridis_c(option="turbo")+
  labs(x="Year", 
       y="Type of care",
       fill="Cost (billions)",
       title ="Total expenditure per year, per sex, per type of care",
       subtitle ="Excluding specialist medical care",
       caption="Source: VEKTIS")+
  theme_bw()


df%>%
  filter(geslacht =="M" | geslacht =="V")%>%
  filter(postcode_3=="977")%>%
  mutate(age_cut = cut(as.numeric(leeftijdsklasse), 10))%>%
  group_by(age_cut, jaar, geslacht)%>%
  summarise(total = sum(kosten_medisch_specialistische_zorg)/1000000)%>%
  ggplot()+
  geom_bar(aes(y=age_cut, 
                 x=total, 
               fill=geslacht), 
           stat="identity")+
  facet_grid(~jaar, scales = "free")+
  labs(y="Age class", 
       x="Cost (millions)",
       fill="Sex",
       title ="Cost for specialist medical care by age and sex",
       subtitle = "for postalcode 977 (random selection)",
       caption="Source: VEKTIS")+
  theme_bw()+
  theme(legend.position="bottom")


df%>%
  mutate(log_vdj=log(as.numeric((aantal_verzekerdejaren))))%>%
  filter(geslacht =="M" | geslacht =="V")%>%
  ggplot()+
  geom_density(aes(x=log_vdj))+
  facet_grid(geslacht~jaar)+
  labs(x="Insurance years (log scale)", 
       y="Density",
       title ="Insurance years per year and sex",
       subtitle = "Postcode3 level",
       caption="Source: VEKTIS")+
  theme_bw()

df%>%
  filter(geslacht =="M" | geslacht =="V")%>%
  mutate(postcode_3=factor(postcode_3), 
         jaar = factor(jaar), 
         geslacht=factor(geslacht))%>%
  ggplot()+
  geom_tile(aes(y=postcode_3, 
                x=jaar, 
                fill=aantal_bsn))+
  facet_grid(~geslacht)+
  scale_fill_viridis_c(option="turbo")+
  labs(x="Year", 
       y="Postal codes",
       fill="# citizens",
       title ="Number of Dutch citizens per postal code",
       subtitle = "BSN & Postcode3",
       caption="Source: VEKTIS")+
  theme_bw()

df%>%
  filter(geslacht =="M" | geslacht =="V")%>%
  ggplot()+
  geom_point(aes(x=as.numeric(leeftijdsklasse), 
                 col=kosten_medisch_specialistische_zorg/1000000, 
                 y=aantal_bsn), 
             alpha=0.8)+
  scale_color_viridis_c(option="turbo")+
  facet_grid(~jaar)+
  labs(x="Age class", 
       col="Cost (millions)",
       y="# citizens",
       title ="Cost for specialist medical care by age category and number of citizens",
       subtitle = "Per postcal code",
       caption="Source: VEKTIS")+
  theme_bw()+
  theme(legend.position="bottom")


set.seed(1)
df%>%
  mutate(postcode_3=factor(postcode_3))%>%
  filter(postcode_3%in%sample(levels(postcode_3),10))%>%
  ggplot()+
  geom_tile(aes(y=postcode_3, 
                x=as.numeric(leeftijdsklasse),
                fill=kosten_medisch_specialistische_zorg/1000000))+
  scale_fill_viridis_c(option="turbo")+
  facet_grid(geslacht~jaar)+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))+
  labs(x="Age class", 
       fill="Cost (millions)",
       y="Postcal code",
       title ="Cost for specialist medical care by year, age category and sex",
       subtitle = "For 10 random postcal codes",
       caption="Source: VEKTIS")+
  theme_bw()+
  theme(legend.position="bottom")


df%>%
  select(postcode_3, jaar, 
         kosten_medisch_specialistische_zorg, aantal_bsn, geslacht)%>%
  tidyr::drop_na()%>%
  ggplot()+
  geom_point(aes(x=postcode_3, 
                 y=log(kosten_medisch_specialistische_zorg),  
                 size=aantal_bsn, 
                 col=geslacht), 
             alpha=0.5)+
  labs(y="Kosten MSZ", 
       x="Postcode")+
  facet_grid(~jaar)+
  theme_bw()+
  theme(legend.position = "bottom")

df%>%
  select(aantal_verzekerdejaren, jaar, 
         kosten_medisch_specialistische_zorg, aantal_bsn, geslacht)%>%
  tidyr::drop_na()%>%
  filter(geslacht=="M" | geslacht =="V")%>%
  ggplot()+
  geom_point(aes(x=log(aantal_verzekerdejaren), 
                 y=log(kosten_medisch_specialistische_zorg),  
                 col=aantal_bsn), 
             alpha=0.5)+
  labs(y="Kosten MSZ", 
       x="Aantal verzekerde jaren")+
  facet_grid(geslacht~jaar)+
  scale_color_viridis_c(option="turbo")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Nunber of insurance years (log)", 
       y="Cost (euro's & log scale)",
       col="# citizens",
       title ="Cost for specialist medical care",
       subtitle = "By insurance years, year, sex and number of Dutch citizens",
       caption="Source: VEKTIS")+
  theme_bw()+
  theme(legend.position="bottom")






#### LOAD, WRANGLE AND EXPLORE IKNL DATA   - CANCER INCIDENCE ----
Incidentie_per_leeftijd <- read_excel("Data/IKNL/NKR/Incidentie_per_leeftijd.xlsx")
colnames(Incidentie_per_leeftijd)
str(Incidentie_per_leeftijd)
Incidentie_per_leeftijd$Leeftijdsgroep
length(table(Incidentie_per_leeftijd$Leeftijdsgroep))
leeftijd_names<-c("0-4", "5-9", "10-14", 
                  "15-19", "20-24", "25-29", "30-34",
                  "35-39", "40-44", "45-49", "50-54",
                  "55-59", "60-64", "65-69", "70-74",
                  "75-79", "80-84", "85+")
length(leeftijd_names)
Incidentie_per_leeftijd$Leeftijdsgroep<-factor(Incidentie_per_leeftijd$Leeftijdsgroep, 
                                               levels=leeftijd_names)
levels(Incidentie_per_leeftijd$Leeftijdsgroep)
Incidentie_per_leeftijd%>%
  group_by(Jaar, Leeftijdsgroep)%>%
  summarise(total=sum(Aantal))%>%
  ggplot()+
  geom_tile(aes(x=Jaar, 
                y=Leeftijdsgroep, 
                fill=total))+
  theme_bw()+
  scale_fill_viridis_c(option="turbo")+
  labs(x="Year", 
       y="Age category", 
       fill="Incidence", 
       title="Total cancer incidence in the Netherlands", 
       caption="Source: IKNL - NKR data ")

Incidentie_per_jaar <- read_excel("Data/IKNL/NKR/Incidentie per jaar.xlsx")
colnames(Incidentie_per_jaar)
ggplot(Incidentie_per_jaar)+
  geom_tile(aes(x=Jaar, 
                y=Regio, 
                fill=Aantal))+
  theme_bw()+
  scale_fill_viridis_c(option="turbo")+
  labs(x="Year", 
        y="Province", 
        fill="Incidence", 
        title="Total cancer incidence in the Netherlands", 
        caption="Source: IKNL - NKR data ")

Incidentie_per_regio_CR<- read_excel("Data/IKNL/NKR/Incidentie per regio_CR.xlsx")
ggplot(Incidentie_per_regio_CR)+
  geom_tile(aes(x=Jaar, 
                y=Regio, 
                fill=Incidentie_per_regio_CR$`Crude rate (CR)`))+
  theme_bw()+
  scale_fill_viridis_c(option="turbo")+
  labs(x="Year", 
       y="Province", 
       fill="Crude Rate", 
       title="Total cancer incidence in the Netherlands", 
       subtitle="Per 100K",
       caption="Source: IKNL - NKR data ")

Incidentie_per_kankersoort <- read_excel("Data/IKNL/NKR/Incidentie per kankersoort.xlsx")
ggplot(Incidentie_per_kankersoort)+
  geom_tile(aes(x=Jaar, 
                y=Kankersoort, 
                fill=Aantal))+
  theme_bw()+
  scale_fill_viridis_c(option="turbo")+
  labs(x="Year", 
       y="Cancer category", 
       fill="Incidence", 
       title="Total cancer incidence in the Netherlands", 
       caption="Source: IKNL - NKR data ")






#### COMPARE IKNL NKR AND SYNTHETIC DATA   - BREAST CANCER  ----
df_NKR_2010<-read_excel("Data/IKNL/NKR/Borstkanker_incidentie_invasief_vrouw.xlsx")
df_NKR_2010<-df_NKR_2010%>%mutate(Source="NKR")%>%select(-Teken)%>%relocate(Leeftijdsgroep, .before = Jaar)
df_NKR_2010
table(df_NKR_2010$Jaar)
df_NKR_2010%>%
  filter(n() > 1) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))

IKNL<- read_delim("Data/IKNL/Synthetisch/NKR_IKNL_breast_syntheticdata.csv", delim = ";", 
                  escape_double = FALSE, trim_ws = TRUE)
dim(IKNL)
str(IKNL)
colnames(IKNL)
col_names <- names(IKNL[,c(4,8:24,27:31,33:46,49:52)])
IKNL[,col_names] <- lapply(IKNL[,col_names] , factor)
sapply(IKNL, class)
levels(IKNL$tumsoort)<-c("Invasief mammacarcinoom",
                         "Ductaal carcinoma in situ",
                         "Lobulair carcinoma in situ")
table(IKNL$tumsoort)
IKNL%>%dplyr::filter(tumsoort=="Invasief mammacarcinoom")


df_IKNL_2010<-IKNL%>%dplyr::filter(gesl==2 & incjr>2009 & tumsoort=="501300")
dim(df_IKNL_2010)

df_NKR_2010%>%
  filter(n() > 1) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))

df_IKNL_2010%>%
  select(leeft, incjr)%>%
  mutate(Leeftijdsgroep = cut(leeft, breaks = 5*(0:18), dig.lab=0, include.lowest=TRUE, right=FALSE, 
                              labels=c("0-4","5-9","10-14",
                                       "15-19", "20-24", "25-29", "30-34",
                                       "35-39", "40-44", "45-49", "50-54",
                                       "55-59", "60-64", "65-69", "70-74",
                                       "75-79", "80-84", "85+")))%>%
  mutate(Leeftijdsgroep = if_else(is.na(Leeftijdsgroep)==TRUE, "85+", Leeftijdsgroep))%>%
  select(Leeftijdsgroep,leeft)%>%
  ggplot()+
  geom_bar(aes(x=factor(leeft)))+
  facet_wrap(~Leeftijdsgroep, ncol=4, scales="free")+
  theme_bw()
  
df_IKNL_2010%>%
  select(leeft, incjr)%>%
  mutate(Leeftijdsgroep = cut(leeft, breaks = 5*(0:18), dig.lab=0, include.lowest=TRUE, right=FALSE, 
                              labels=c("0-4","5-9","10-14",
                                       "15-19", "20-24", "25-29", "30-34",
                                       "35-39", "40-44", "45-49", "50-54",
                                       "55-59", "60-64", "65-69", "70-74",
                                       "75-79", "80-84", "85+")))%>%
  mutate(Leeftijdsgroep = if_else(is.na(Leeftijdsgroep)==TRUE, "85+", Leeftijdsgroep))%>%
  rename(Jaar=incjr)%>%
  group_by(Leeftijdsgroep,Jaar)%>%
  summarise(Aantal = n())%>%
  mutate(Source="Synthetic")%>%
  ungroup()%>%
  rbind(df_NKR_2010)%>%
  filter(Leeftijdsgroep%in%c("45-49","50-54","55-59","60-64",
                             "65-69","70-74","75-79","80-84","85+"))%>%
  ggplot()+
  geom_bar(aes(x=Jaar, y=Aantal, fill=Source),stat="identity", position="dodge")+
  facet_wrap(~Leeftijdsgroep,ncol=3)+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Year", 
       y="Number of cases", 
       title="Comparing incidence for breast cancer (invasive mamma)", 
       caption= "Source: IKNL NKR & Synthetic data")



df_IKNL_2010%>%
  select(leeft, incjr)%>%
  mutate(Leeftijdsgroep = cut(leeft, breaks = 5*(0:18), dig.lab=0, include.lowest=TRUE, right=FALSE, 
                              labels=c("0-4","5-9","10-14",
                                       "15-19", "20-24", "25-29", "30-34",
                                       "35-39", "40-44", "45-49", "50-54",
                                       "55-59", "60-64", "65-69", "70-74",
                                       "75-79", "80-84", "85+")))%>%
  mutate(Leeftijdsgroep = if_else(is.na(Leeftijdsgroep)==TRUE, "85+", Leeftijdsgroep))%>%
  rename(Jaar=incjr)%>%
  group_by(Leeftijdsgroep,Jaar)%>%
  summarise(Aantal = n())%>%
  mutate(Source="Synthetic")%>%
  ungroup()%>%
  mutate(Jaar=as.character(Jaar))%>%
  rename(Aantal_Synthetic=Aantal)%>%
  left_join(., df_NKR_2010, by=c("Leeftijdsgroep", "Jaar"))%>%
  tidyr::drop_na()%>%
  mutate(diff=((Aantal_Synthetic-Aantal)/Aantal_Synthetic)*100)%>%
  filter(Leeftijdsgroep%in%c("45-49","50-54","55-59","60-64",
                             "65-69","70-74","75-79","80-84","85+"))%>%
  ggplot()+
  geom_bar(aes(x=Jaar, y=diff),stat="identity", position="dodge")+
  facet_wrap(~Leeftijdsgroep,ncol=3)+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Year", 
       y="Difference (%)", 
       title="Comparing incidence for breast cancer (invasive mamma)", 
       subtitle="Percentage difference between sources",
       caption= "Source: IKNL NKR & Synthetic data")




  
  
  







#### LOAD, WRANGLE AND EXPLORE IKNL DATA   - CREATE CLINICAL GROUPS ----
IKNL<- read_delim("Data/IKNL/Synthetisch/NKR_IKNL_breast_syntheticdata.csv", delim = ";", 
                  escape_double = FALSE, trim_ws = TRUE)
dim(IKNL)
str(IKNL)
colnames(IKNL)
IKNL$perclymf<-IKNL$pos_lymf/IKNL$ond_lymf
IKNL$c_eq_p<-as.factor(ifelse(IKNL$ct==IKNL$pt, "TRUE", "FALSE"))
IKNL$her2_stat<-ifelse(IKNL$her2_stat==9, NA, IKNL$her2_stat)
IKNL$er_stat<-ifelse(IKNL$er_stat==9, NA, IKNL$er_stat)
IKNL$pr_stat<-ifelse(IKNL$pr_stat==9, NA, IKNL$pr_stat)
IKNL$her2_stat_fact<-ifelse(IKNL$her2_stat==3, 1, 0)
IKNL$er_stat_fact<-ifelse(IKNL$er_stat>0, 1, 0)
IKNL$pr_stat_fact<-ifelse(IKNL$pr_stat>0, 1, 0)
IKNL$gen_stat<-IKNL$her2_stat_fact+IKNL$er_stat_fact+IKNL$pr_stat_fact
IKNL$tum_afm<-as.numeric(IKNL$tum_afm)
IKNL$Stage<-substr(IKNL$stadium, 1, 1)   
col_names <- names(IKNL[,c(4,8:24,27:31,33:46,49:52)])
IKNL[,col_names] <- lapply(IKNL[,col_names] , factor)
sapply(IKNL, class)
levels(IKNL$tumsoort)<-c("Invasief mammacarcinoom",
                         "Ductaal carcinoma in situ",
                         "Lobulair carcinoma in situ")
levels(IKNL$diag_basis)<-c("Clinical-Diagnostic Research",
                           "Hematological of cutological confirmation",
                           "Histological confirmation metastases",
                           "Histological confirmation primary tumor")
levels(IKNL$diffgrad)<-c("Low grade",
                         "Intermediate grade",
                         "High grade",
                         "NVT")
levels(IKNL$uitgebr_chir_code)<-c("Lumpectomie (no OKD)", 
                                  "Lumpectomie (with OKD)", 
                                  "Ablatio (no OKD)",
                                  "Amputation (with OKD)", 
                                  "Resection other (coincidence)", 
                                  "Lymph node dissection")
df<-IKNL%>%dplyr::filter(vit_stat_int>0)%>%filter(tumsoort=="Invasief mammacarcinoom")
df2<-df%>%dplyr::filter(vit_stat==1)%>%filter(tumsoort=="Invasief mammacarcinoom")

IKNL_im<-IKNL%>%dplyr::select(vit_stat_int,vit_stat,incjr, Stage, tumsoort, diffgrad, diag_basis)%>%drop_na()%>%filter(tumsoort=="Invasief mammacarcinoom")
IKNL_im%>%group_by(incjr, Stage, diffgrad, vit_stat)%>%
  mutate(incjr=factor(incjr), vit_stat=factor(vit_stat))%>%
  summarise(n=n())%>%
  group_by(incjr, Stage, diffgrad)%>%
  mutate(total=sum(n), 
         perc=(n/total)*100)%>%
  filter(!Stage=="0" & vit_stat=="0" & !diffgrad=="NVT")%>%
  ggplot(., 
       aes(x=incjr, y=perc, 
           fill=diffgrad, 
           group=diffgrad))+
  facet_grid(~Stage)+
  geom_bar(stat="identity", position="dodge")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Inclusion year", 
       y="Percentage alive", 
       fill="Tumour grade", 
       title="Percentage of patients alive", 
       subtitle="Based on year included, tumour stage and tumour grade", 
       caption="Source: IKNL - synthetic data")

IKNL_im%>%group_by(incjr, Stage, diffgrad, vit_stat, diag_basis)%>%
  mutate(incjr=factor(incjr), vit_stat=factor(vit_stat))%>%
  summarise(n=n())%>%
  group_by(incjr, Stage, diffgrad, diag_basis)%>%
  mutate(total=sum(n), 
         perc=(n/total)*100)%>%
  filter(!Stage=="0" & vit_stat=="0" & !diffgrad=="NVT")%>%
  ggplot(., 
         aes(x=incjr, fill=perc, 
             y=Stage))+
  facet_grid(diag_basis~diffgrad)+
  geom_tile()+
  scale_fill_viridis_c(option="turbo")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Inclusion year", 
       y="Percentage alive", 
       col="Tumour stage", 
       title="Percentage of patients alive", 
       subtitle="Based on year included, tumour stage and tumour grade", 
       caption="Source: IKNL - synthetic data")

df2010<-IKNL%>%dplyr::filter(vit_stat_int>0 & incjr>2009)%>%filter(tumsoort=="Invasief mammacarcinoom")
df2010_clinical<-df2010%>%select(leeft,incjr,tumsoort, diffgrad, gedrag,
                                 gen_stat, Stage, ond_lymf, pos_lymf, perclymf, 
                                 tum_afm, her2_stat_fact, er_stat_fact,pr_stat_fact, 
                                 horm, chemo, rt, meta_rt, target, dir_reconstr,
                                 meta_rt, meta_chir, morf)
levels(df2010_clinical$horm)<-c("No",
                           "Pre-surgery",
                           "Post-surgery",
                           "Pre-and-post-surgery", 
                           "Yes, no surgery")
levels(df2010_clinical$chemo)<-c("No",
                                "Pre-surgery",
                                "Post-surgery",
                                "Pre-and-post-surgery", 
                                "Yes, no surgery")
levels(df2010_clinical$rt)<-c("No",
                   "Pre-surgery",
                   "Post-surgery",
                   "Yes, no surgery")
levels(df2010_clinical$target)<-c("No",
                              "Pre-surgery",
                              "Post-surgery",
                              "Pre-and-post-surgery", 
                              "Yes, no surgery")
levels(df2010_clinical$dir_reconstr)<-c("No","Yes")
levels(df2010_clinical$meta_rt)<-c("No","Yes")
levels(df2010_clinical$meta_chir)<-c("No","Yes")

df2010_clinical%>%
  filter(!Stage=="0" & !diffgrad=="NVT" &!is.na(gen_stat))%>%
  group_by(incjr, Stage)%>%
  summarise(count=n())%>%
  group_by(incjr)%>%
  mutate(total=sum(count), 
         perc=(count/total)*100)%>%
  ggplot(., 
       aes(x=factor(incjr), y=perc, fill=Stage))+
  geom_bar(stat="identity")+
  theme_bw()+
  labs(x="Inclusion year", 
       y="Percentage", 
       fill="Tumour stage", 
       title="Tumour stages over the years", 
       caption="Source: IKNL - synthetic data") 

df2010_clinical%>%
  group_by(incjr, Stage)%>%
  summarise(count=n())%>%
  group_by(incjr)%>%
  mutate(total=sum(count), 
         perc=(count/total)*100)%>%
  ggplot(., 
         aes(x=factor(incjr), y=perc, fill=Stage))+
  geom_bar(stat="identity")+
  theme_bw()+
  labs(x="Inclusion year", 
       y="Percentage", 
       fill="Tumour stage", 
       title="Tumour stages over the years", 
       caption="Source: IKNL - synthetic data") 

df2010_clinical%>%
  filter(!Stage=="0" & !diffgrad=="NVT" &!is.na(gen_stat))%>%
  group_by(incjr, diffgrad)%>%
  summarise(count=n())%>%
  group_by(incjr)%>%
  mutate(total=sum(count), 
         perc=(count/total)*100)%>%
  ggplot(., 
         aes(x=factor(incjr), y=perc, fill=diffgrad))+
  geom_bar(stat="identity")+
  theme_bw()+
  labs(x="Inclusion year", 
       y="Percentage", 
       fill="Tumour grade", 
       title="Tumour grading over the years", 
       caption="Source: IKNL - synthetic data") 

df2010_clinical%>%
  filter(!Stage=="0" & !diffgrad=="NVT" &!is.na(gen_stat))%>%
  group_by(incjr, gen_stat)%>%
  summarise(count=n())%>%
  group_by(incjr)%>%
  mutate(total=sum(count), 
         perc=(count/total)*100)%>%
  ggplot(., 
         aes(x=factor(incjr), y=perc, fill=gen_stat))+
  geom_bar(stat="identity")+
  theme_bw()+
  labs(x="Inclusion year", 
       y="Percentage", 
       fill="Number", 
       title="Genetic predisposition over the years", 
       subtitle="Score is calculated by summing presence of estrogen receptor, progesteron recepter or HER2",
       caption="Source: IKNL - synthetic data") 

df2010_clinical%>%
  filter(!Stage=="0" & !diffgrad=="NVT" &!is.na(gen_stat))%>%
  group_by(incjr, horm)%>%
  summarise(count=n())%>%
  group_by(incjr)%>%
  mutate(total=sum(count), 
         perc=(count/total)*100)%>%
  ggplot(., 
         aes(x=factor(incjr), y=perc, fill=horm))+
  geom_bar(stat="identity")+
  theme_bw()+
  labs(x="Inclusion year", 
       y="Percentage", 
       fill="Category", 
       title="Hormonal treatment over the years", 
       caption="Source: IKNL - synthetic data") 

df2010_clinical%>%
  filter(!Stage=="0" & !diffgrad=="NVT" &!is.na(gen_stat))%>%
  group_by(incjr, chemo)%>%
  summarise(count=n())%>%
  group_by(incjr)%>%
  mutate(total=sum(count), 
         perc=(count/total)*100)%>%
  ggplot(., 
         aes(x=factor(incjr), y=perc, fill=chemo))+
  geom_bar(stat="identity")+
  theme_bw()+
  labs(x="Inclusion year", 
       y="Percentage", 
       fill="Category", 
       title="Chemotherapy over the years", 
       caption="Source: IKNL - synthetic data") 

df2010_clinical%>%
  filter(!Stage=="0" & !diffgrad=="NVT" &!is.na(gen_stat))%>%
  group_by(incjr, rt)%>%
  summarise(count=n())%>%
  group_by(incjr)%>%
  mutate(total=sum(count), 
         perc=(count/total)*100)%>%
  ggplot(., 
         aes(x=factor(incjr), y=perc, fill=rt))+
  geom_bar(stat="identity")+
  theme_bw()+
  labs(x="Inclusion year", 
       y="Percentage", 
       fill="Category", 
       title="Radiotherapy over the years", 
       caption="Source: IKNL - synthetic data") 

df2010_clinical%>%
  filter(!Stage=="0" & !diffgrad=="NVT" &!is.na(gen_stat))%>%
  group_by(incjr, target)%>%
  summarise(count=n())%>%
  group_by(incjr)%>%
  mutate(total=sum(count), 
         perc=(count/total)*100)%>%
  ggplot(., 
         aes(x=factor(incjr), y=perc, fill=target))+
  geom_bar(stat="identity")+
  theme_bw()+
  labs(x="Inclusion year", 
       y="Percentage", 
       fill="Category", 
       title="Targeted therapy over the years", 
       caption="Source: IKNL - synthetic data") 

df2010_clinical%>%
  filter(!Stage=="0" & !diffgrad=="NVT" &!is.na(gen_stat))%>%
  group_by(incjr, target)%>%
  summarise(count=n())%>%
  group_by(incjr)%>%
  mutate(total=sum(count), 
         perc=(count/total)*100)%>%
  ggplot(., 
         aes(x=factor(incjr), y=perc, fill=target))+
  geom_bar(stat="identity")+
  theme_bw()+
  labs(x="Inclusion year", 
       y="Percentage", 
       fill="Category", 
       title="Targeted therapy over the years", 
       caption="Source: IKNL - synthetic data") 

df2010_clinical%>%
  filter(!Stage=="0" & !diffgrad=="NVT" &!is.na(gen_stat))%>%
  group_by(incjr, dir_reconstr)%>%
  summarise(count=n())%>%
  group_by(incjr)%>%
  mutate(total=sum(count), 
         perc=(count/total)*100)%>%
  ggplot(., 
         aes(x=factor(incjr), y=perc, fill=dir_reconstr))+
  geom_bar(stat="identity")+
  theme_bw()+
  labs(x="Inclusion year", 
       y="Percentage", 
       fill="Category", 
       title="Direct reconstruction over the years", 
       caption="Source: IKNL - synthetic data") 

df2010_clinical%>%
  filter(!Stage=="0" & !diffgrad=="NVT" &!is.na(gen_stat))%>%
  group_by(incjr, meta_rt)%>%
  summarise(count=n())%>%
  group_by(incjr)%>%
  mutate(total=sum(count), 
         perc=(count/total)*100)%>%
  ggplot(., 
         aes(x=factor(incjr), y=perc, fill=meta_rt))+
  geom_bar(stat="identity")+
  theme_bw()+
  labs(x="Inclusion year", 
       y="Percentage", 
       fill="Category", 
       title="Radiotherapy for metastasis over the years", 
       caption="Source: IKNL - synthetic data") 

df2010_clinical%>%
  filter(!Stage=="0" & !diffgrad=="NVT" &!is.na(gen_stat))%>%
  group_by(incjr, meta_chir)%>%
  summarise(count=n())%>%
  group_by(incjr)%>%
  mutate(total=sum(count), 
         perc=(count/total)*100)%>%
  ggplot(., 
         aes(x=factor(incjr), y=perc, fill=meta_chir))+
  geom_bar(stat="identity")+
  theme_bw()+
  labs(x="Inclusion year", 
       y="Percentage", 
       fill="Category", 
       title="Surgery for metastasis over the years", 
       caption="Source: IKNL - synthetic data") 


df2010_clinical%>%
  filter(!Stage=="0" & !diffgrad=="NVT" &!is.na(gen_stat))%>%
  group_by(incjr, morf)%>%
  summarise(count=n())%>%
  group_by(incjr)%>%
  mutate(total=sum(count), 
         perc=(count/total)*100)%>%
  ggplot(., 
         aes(x=factor(incjr), y=perc, fill=morf))+
  geom_bar(stat="identity")+
  theme_bw()+
  labs(x="Inclusion year", 
       y="Percentage", 
       fill="Category", 
       title="Histology over the years", 
       caption="Source: IKNL - synthetic data") 


behprepost<-c("0" = "No",
              "1" = "Pre-surgery",
              "2" = "Post-surgery",
              "3" = "Pre-and-post-surgery", 
              "4" = "Yes, no surgery")
df2010_clinical%>%
  filter(!Stage=="0" & !diffgrad=="NVT" &!is.na(gen_stat))%>%
  group_by(chemo, target, horm, rt)%>%
  summarise(count=n())%>%
  ggplot(., 
         aes(x=chemo, y=horm, fill=count))+
  geom_tile()+
  facet_grid(rt~target, labeller = labeller(rt=behprepost, 
                                            target=behprepost))+
  theme_bw()+
  scale_fill_viridis_c(option="turbo")+
  scale_y_discrete("Hormonal therapy", labels=c("No","Pre-surgery","Post-surgery",
                     "Pre-and-post-surgery","Yes, no surgery"))+
  scale_x_discrete("Chemotherapy", labels=c("No","Pre-surgery","Post-surgery",
                     "Pre-and-post-surgery","Yes, no surgery"))+
  labs(x="Chemo therapy", 
       y="Hormonal therapy", 
       fill="Count", 
       title="Therapy provided over the years", 
       caption="Source: IKNL - synthetic data")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))

g1<-df2010_clinical%>%
  filter(!Stage=="0" & !diffgrad=="NVT" &!is.na(her2_stat_fact))%>%
  group_by(incjr, her2_stat_fact)%>%
  summarise(count=n())%>%
  group_by(incjr)%>%
  mutate(total=sum(count), 
         perc=(count/total)*100)%>%
  mutate(her2_stat_fact = if_else(her2_stat_fact==1, "Present", "Non-present"))%>%
  ggplot(., 
         aes(x=factor(incjr), y=perc, fill=her2_stat_fact))+
  geom_bar(stat="identity")+
  theme_bw()+
  labs(x="Inclusion year", 
       y="Percentage", 
       fill="Status", 
       title="HER2 status over the years") 
g2<-df2010_clinical%>%
  filter(!Stage=="0" & !diffgrad=="NVT" &!is.na(er_stat_fact))%>%
  group_by(incjr, er_stat_fact)%>%
  summarise(count=n())%>%
  group_by(incjr)%>%
  mutate(total=sum(count), 
         perc=(count/total)*100)%>%
  mutate(er_stat_fact = if_else(er_stat_fact==1, "Present", "Non-present"))%>%
  ggplot(., 
         aes(x=factor(incjr), y=perc, fill=er_stat_fact))+
  geom_bar(stat="identity")+
  theme_bw()+
  labs(x="Inclusion year", 
       y="Percentage", 
       fill="Status", 
       title="Estrogen receptor status over the years") 
g3<-df2010_clinical%>%
  filter(!Stage=="0" & !diffgrad=="NVT" &!is.na(pr_stat_fact))%>%
  group_by(incjr, pr_stat_fact)%>%
  summarise(count=n())%>%
  group_by(incjr)%>%
  mutate(total=sum(count), 
         perc=(count/total)*100)%>%
  mutate(pr_stat_fact = if_else(pr_stat_fact==1, "Present", "Non-present"))%>%
  ggplot(., 
         aes(x=factor(incjr), y=perc, fill=pr_stat_fact))+
  geom_bar(stat="identity")+
  theme_bw()+
  labs(x="Inclusion year", 
       y="Percentage", 
       fill="Status", 
       title="Progesteron receptor status over the years", 
       caption="Source: IKNL - synthetic data")
gridExtra::grid.arrange(g1,g2,g3, ncol=1)

df2010_clinical%>%
  filter(!Stage=="0" & !diffgrad=="NVT" &!is.na(gen_stat))%>%
  mutate(incjr=factor(incjr))%>%
  group_by(incjr, Stage)%>%
  summarise(mean = median(leeft, na.rm=TRUE))%>%
  ungroup()%>%
  ggplot(., 
         aes(x=incjr, y=mean, col = Stage, group=Stage)) +
  geom_line()+
  geom_point()+
  theme_bw()+
  labs(x="Inclusion year", 
       y="Median age", 
       col="Tumour stage", 
       title="Median age by inclusion year and tumour stage", 
       caption="Source: IKNL - synthetic data")+
  theme(legend.position = "bottom")

df2010_clinical%>%
  filter(!Stage=="0" & !diffgrad=="NVT" &!is.na(gen_stat))%>%
  mutate(incjr=factor(incjr))%>%
  group_by(incjr, diffgrad)%>%
  summarise(mean = median(leeft, na.rm=TRUE))%>%
  ungroup()%>%
  ggplot(., 
         aes(x=incjr, y=mean, col = diffgrad, group=diffgrad)) +
  geom_line()+
  geom_point()+
  theme_bw()+
  labs(x="Inclusion year", 
       y="Median age", 
       col="Tumour grade", 
       title="Median age by inclusion year and tumour grade", 
       caption="Source: IKNL - synthetic data")+
  theme(legend.position = "bottom")


df2010_clinical%>%
  filter(!Stage=="0" & !diffgrad=="NVT" &!is.na(gen_stat))%>%
  mutate(incjr=factor(incjr))%>%
  group_by(incjr, diffgrad, Stage)%>%
  summarise(mean = median(leeft, na.rm=TRUE))%>%
  ungroup()%>%
  ggplot(., 
         aes(x=incjr, y=Stage, fill = mean)) +
  geom_tile()+
  facet_grid(~diffgrad)+
  scale_fill_viridis_c(option="turbo")+
  theme_bw()+
  labs(x="Inclusion year", 
       fill="Median age", 
       y="Tumour stage", 
       title="Median age by inclusion year",
       subtitle="And tumour stage and tumour grade",
       caption="Source: IKNL - synthetic data")+
  theme(legend.position = "bottom")




df2010_clinical%>%
  filter(!Stage=="0" & !diffgrad=="NVT" &!is.na(gen_stat))%>%
  mutate(Leeftijdsgroep = cut(leeft, breaks = 5*(0:18), dig.lab=0, include.lowest=TRUE, right=FALSE, 
                              labels=c("0-4","5-9","10-14",
                                       "15-19", "20-24", "25-29", "30-34",
                                       "35-39", "40-44", "45-49", "50-54",
                                       "55-59", "60-64", "65-69", "70-74",
                                       "75-79", "80-84", "85+")))%>%
  mutate(Leeftijdsgroep = if_else(is.na(Leeftijdsgroep)==TRUE, "85+", Leeftijdsgroep))%>%
  rename(Jaar=incjr)%>%
  group_by(Jaar, Leeftijdsgroep)%>%
  summarise(count=n())%>%
  group_by(Jaar)%>%
  mutate(total=sum(count), 
         perc=(count/total)*100)%>%
  ggplot(., 
         aes(x=factor(Jaar), y=perc, fill=Leeftijdsgroep))+
  geom_bar(stat="identity")+
  theme_bw()+
  labs(x="Inclusion year", 
       y="Percentage", 
       fill="Age group", 
       title="Percentage for each age group and inclusion year", 
       caption="Source: IKNL - synthetic data")+
  theme(legend.position = "bottom")


df2010_clinical%>%
  filter(!Stage=="0" & !diffgrad=="NVT" &!is.na(gen_stat))%>%
  mutate(Leeftijdsgroep = cut(leeft, breaks = 5*(0:18), dig.lab=0, include.lowest=TRUE, right=FALSE, 
                              labels=c("0-4","5-9","10-14",
                                       "15-19", "20-24", "25-29", "30-34",
                                       "35-39", "40-44", "45-49", "50-54",
                                       "55-59", "60-64", "65-69", "70-74",
                                       "75-79", "80-84", "85+")))%>%
  mutate(Leeftijdsgroep = if_else(is.na(Leeftijdsgroep)==TRUE, "85+", Leeftijdsgroep))%>%
  rename(Jaar=incjr)%>%
  group_by(Jaar, Leeftijdsgroep, Stage)%>%
  summarise(count=n())%>%
  group_by(Jaar, Stage)%>%
  mutate(total=sum(count), 
         perc=(count/total)*100)%>%
  ggplot(., 
         aes(x=factor(Jaar), fill=perc, y=Leeftijdsgroep))+
  geom_tile()+
  theme_bw()+
  facet_grid(~Stage)+
  scale_fill_viridis_c(option="turbo")+
  labs(x="Inclusion year", 
       fill="Percentage", 
       y="Age group", 
       title="Percentage for each age group and inclusion year",
       subtitle = "By tumour stage",
       caption="Source: IKNL - synthetic data")+
  theme(legend.position = "bottom")


df2010_clinical%>%
  filter(!Stage=="0" & !diffgrad=="NVT" &!is.na(gen_stat))%>%
  mutate(Leeftijdsgroep = cut(leeft, breaks = 5*(0:18), dig.lab=0, include.lowest=TRUE, right=FALSE, 
                              labels=c("0-4","5-9","10-14",
                                       "15-19", "20-24", "25-29", "30-34",
                                       "35-39", "40-44", "45-49", "50-54",
                                       "55-59", "60-64", "65-69", "70-74",
                                       "75-79", "80-84", "85+")))%>%
  mutate(Leeftijdsgroep = if_else(is.na(Leeftijdsgroep)==TRUE, "85+", Leeftijdsgroep))%>%
  rename(Jaar=incjr)%>%
  group_by(Jaar, Leeftijdsgroep, Stage, diffgrad)%>%
  summarise(count=n())%>%
  group_by(Jaar, Stage)%>%
  mutate(total=sum(count), 
         perc=(count/total)*100)%>%
  ggplot(., 
         aes(x=factor(Jaar), fill=perc, y=Leeftijdsgroep))+
  geom_tile()+
  theme_bw()+
  facet_grid(diffgrad~Stage)+
  scale_fill_viridis_c(option="turbo")+
  labs(x="Inclusion year", 
       fill="Percentage", 
       y="Age group", 
       title="Percentage for each age group and inclusion year",
       subtitle = "By tumour stage and tumour grade",
       caption="Source: IKNL - synthetic data")+
  theme(legend.position = "bottom")





find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

df2010_clinical%>%
  select(Stage,diffgrad,gen_stat,incjr,pos_lymf)%>%
  filter(!Stage=="0" & !diffgrad=="NVT" &!is.na(gen_stat))%>%
  mutate(incjr = factor(incjr))%>%
  group_by(incjr, Stage)%>%
  tidyr::drop_na()%>%
  summarise(Mean   = mean(pos_lymf, na.rm=TRUE), 
            Median = median(pos_lymf, na.rm=TRUE),
            Mode   = modeest::mlv(pos_lymf, na.rm=TRUE, method="mfv"), 
            Mode2  = find_mode(pos_lymf))%>%
  pivot_longer(cols = -c(incjr, Stage), 
               names_to = "Metric", 
               values_to = "Value")%>%
  ggplot(., 
         aes(x=incjr, y=Value, fill=Metric))+
  geom_bar(stat="identity", position="dodge")+
  ylim(-1,10)+
  scale_y_discrete(limits=c("-1", "0","1","2","3","4",
                            "5", "6","7","8","9","10"))+
  theme_bw()+
  labs(x="Inclusion year", 
       fill="Metric", 
       y="Positive lymph nodes", 
       title="Number of positive lymph nodes per inclusion year",
       caption="Source: IKNL - synthetic data")+
  theme(legend.position = "bottom")


df2010_clinical%>%
  select(Stage,diffgrad,gen_stat,incjr,pos_lymf)%>%
  filter(!Stage=="0" & !diffgrad=="NVT" &!is.na(gen_stat))%>%
  mutate(incjr = factor(incjr))%>%
  group_by(incjr, Stage)%>%
  tidyr::drop_na()%>%
  summarise(Mean   = mean(pos_lymf, na.rm=TRUE), 
            Median = median(pos_lymf, na.rm=TRUE),
            Mode   = modeest::mlv(pos_lymf, na.rm=TRUE, method="mfv"), 
            Mode2  = find_mode(pos_lymf))%>%
  pivot_longer(cols = -c(incjr, Stage), 
               names_to = "Metric", 
               values_to = "Value")%>%
  ggplot(., 
         aes(x=incjr, y=Value, fill=Metric))+
  geom_bar(stat="identity", position="dodge")+
  ylim(-1,10)+
  scale_y_discrete(limits=c("-1", "0","1","2","3","4",
                            "5", "6","7","8","9","10"))+
  theme_bw()+
  facet_grid(~Stage)+
  labs(x="Inclusion year", 
       fill="Metric", 
       y="Positive lymph nodes", 
       title="Number of positive lymph nodes per inclusion year",
       subtitle = "By tumour stage",
       caption="Source: IKNL - synthetic data")+
  theme(legend.position = "bottom")


df2010_clinical%>%
  select(Stage,diffgrad,gen_stat,incjr,pos_lymf)%>%
  filter(!Stage=="0" & !diffgrad=="NVT" &!is.na(gen_stat))%>%
  mutate(incjr = factor(incjr))%>%
  group_by(incjr, Stage, diffgrad)%>%
  tidyr::drop_na()%>%
  reframe(Mean   = mean(pos_lymf, na.rm=TRUE), 
            Median = median(pos_lymf, na.rm=TRUE),
            Mode   = modeest::mlv(pos_lymf, na.rm=TRUE, method="mfv"), 
            Mode2  = find_mode(pos_lymf))%>%
  pivot_longer(cols = -c(incjr, Stage,diffgrad), 
               names_to = "Metric", 
               values_to = "Value")%>%
  ggplot(., 
         aes(x=incjr, y=Metric, fill=Value))+
  geom_tile()+
  theme_bw()+
  facet_grid(Stage~diffgrad)+
  scale_fill_viridis_c(option="turbo")+
  labs(x="Inclusion year", 
       y="Metric", 
       fill="Positive lymph nodes", 
       title="Number of positive lymph nodes per inclusion year",
       subtitle = "By tumour stage, tumour grade and metric used",
       caption="Source: IKNL - synthetic data")+
  theme(legend.position = "bottom")

df2010_clinical%>%
  select(Stage,diffgrad,gen_stat,incjr,ond_lymf)%>%
  filter(!Stage=="0" & !diffgrad=="NVT" &!is.na(gen_stat))%>%
  mutate(incjr = factor(incjr))%>%
  group_by(incjr, Stage, diffgrad)%>%
  tidyr::drop_na()%>%
  reframe(Mean   = mean(ond_lymf, na.rm=TRUE), 
          Median = median(ond_lymf, na.rm=TRUE),
          Mode   = modeest::mlv(ond_lymf, na.rm=TRUE, method="mfv"), 
          Mode2  = find_mode(ond_lymf))%>%
  pivot_longer(cols = -c(incjr, Stage,diffgrad), 
               names_to = "Metric", 
               values_to = "Value")%>%
  ggplot(., 
         aes(x=incjr, y=Metric, fill=Value))+
  geom_tile()+
  theme_bw()+
  facet_grid(Stage~diffgrad)+
  scale_fill_viridis_c(option="turbo")+
  labs(x="Inclusion year", 
       y="Metric", 
       fill="Investigated lymph nodes", 
       title="Number of Investigated lymph nodes per inclusion year",
       subtitle = "By tumour stage, tumour grade and metric used",
       caption="Source: IKNL - synthetic data")+
  theme(legend.position = "bottom")

df2010_clinical%>%
  select(Stage,diffgrad,gen_stat,incjr,perclymf)%>%
  filter(!Stage=="0" & !diffgrad=="NVT" &!is.na(gen_stat))%>%
  mutate(incjr = factor(incjr))%>%
  group_by(incjr, Stage, diffgrad)%>%
  tidyr::drop_na()%>%
  reframe(Mean   = mean(perclymf, na.rm=TRUE), 
          Median = median(perclymf, na.rm=TRUE),
          Mode   = modeest::mlv(perclymf, na.rm=TRUE, method="mfv"), 
          Mode2  = find_mode(perclymf))%>%
  pivot_longer(cols = -c(incjr, Stage,diffgrad), 
               names_to = "Metric", 
               values_to = "Value")%>%
  ggplot(., 
         aes(x=incjr, y=Metric, fill=Value))+
  geom_tile()+
  theme_bw()+
  facet_grid(Stage~diffgrad)+
  scale_fill_viridis_c(option="turbo")+
  labs(x="Inclusion year", 
       y="Metric", 
       fill="Percentage lymph nodes positive", 
       title="Percentage of positive lymph nodes per inclusion year",
       subtitle = "By tumour stage, tumour grade and metric used",
       caption="Source: IKNL - synthetic data")+
  theme(legend.position = "bottom")

df2010_clinical%>%
  select(Stage,diffgrad,gen_stat,incjr,tum_afm)%>%
  filter(!Stage=="0" & !diffgrad=="NVT" &!is.na(gen_stat))%>%
  filter(tum_afm<101)%>%
  mutate(incjr = factor(incjr))%>%
  group_by(incjr, Stage, diffgrad)%>%
  tidyr::drop_na()%>%
  reframe(Mean   = mean(tum_afm, na.rm=TRUE), 
          Median = median(tum_afm, na.rm=TRUE),
          Mode   = modeest::mlv(tum_afm, na.rm=TRUE, method="mfv"), 
          Mode2  = find_mode(tum_afm))%>%
  pivot_longer(cols = -c(incjr, Stage,diffgrad), 
               names_to = "Metric", 
               values_to = "Value")%>%
  ggplot(., 
         aes(x=incjr, y=Metric, fill=Value))+
  geom_tile()+
  theme_bw()+
  facet_grid(Stage~diffgrad)+
  scale_fill_viridis_c(option="turbo")+
  labs(x="Inclusion year", 
       y="Metric", 
       fill="Tumour size", 
       title="Tumour size per inclusion year",
       subtitle = "By tumour stage, tumour grade and metric used - excluding tumours larger than 100mm",
       caption="Source: IKNL - synthetic data")+
  theme(legend.position = "bottom")

df2010_clinical%>%
  select(Stage,incjr,tum_afm,diffgrad)%>%
  filter(!Stage=="0")%>%
  mutate(incjr = factor(incjr))%>%
  group_by(incjr, Stage, diffgrad)%>%
  arrange(Stage, incjr)%>%
  tidyr::drop_na()%>%
  summarise(Mean   = mean(tum_afm, na.rm=TRUE), 
            Median = median(tum_afm, na.rm=TRUE), 
            Mode = mode(tum_afm))%>%
  pivot_longer(cols = -c(incjr, Stage, diffgrad), 
               names_to = "Metric", 
               values_to = "Value")%>%
  ggplot(., 
         aes(x=incjr, fill=Value, y=Metric))+
  geom_tile()+
  facet_grid(diffgrad~Stage)+
  scale_fill_viridis_c(option="turbo")+
  theme_bw()+
  labs(x="Inclusion year", 
       y="Metric", 
       fill="Tumour size", 
       title="Tumour size per inclusion year",
       subtitle = "By tumour stage, tumour grade and metric used",
       caption="Source: IKNL - synthetic data")+
  theme(legend.position = "bottom")



df2010_clinical%>%
  filter(!Stage=="0" & !diffgrad=="NVT" &!is.na(gen_stat))%>%
  ggplot(., 
       aes(x=ond_lymf, y=pos_lymf, col=perclymf))+
  geom_point()+
  facet_grid(diffgrad~Stage)+
  scale_color_viridis_c(option="turbo")+
  theme_bw()+
  labs(x="Investigated lymph nodes", 
       y="Positive lymph nodes", 
       col="Percentage lymph nodes", 
       title="Relationship between investigated and positive lymph nodes",
       subtitle = "By tumour stage and tumour grade",
       caption="Source: IKNL - synthetic data")+
  theme(legend.position = "bottom")

ggplot(df2010_clinical, 
       aes(x=ond_lymf, y=pos_lymf, col=gen_stat))+
  geom_point()+
  facet_grid(diffgrad~Stage)+
  theme_bw()








#### LOAD, WRANGLE AND EXPLORE IKNL DATA   - CANCER PREVALENCE ----
Vijf_jaar_prevalentie_per_regio <- read_excel("Data/IKNL/NKR/Vijf_jaar_prevalentie_per_regio.xlsx")

ggplot(Vijf_jaar_prevalentie_per_regio)+
  geom_tile(aes(x=Jaar, 
                y=Regio, 
                fill=`5-jaarsprevalentie`))+
  theme_bw()+
  scale_fill_viridis_c(option="turbo")+
  labs(x="Year", 
       y="Province", 
       fill="Prevalence", 
       title="Total cancer prevalence in the Netherlands", 
       subtitle="Per province",
       caption="Source: IKNL - NKR data ")

Vijf_jaar_prevalentie_per_kanker <- read_excel("Data/IKNL/NKR/Vijf_jaar_prevalentie_per_kanker.xlsx")
Vijf_jaar_prevalentie_per_kanker%>%
  mutate(Period=if_else(Jaar<2020, "pre-Covid","Covid"))%>%
  ggplot(., 
         aes(x=Jaar, 
             col=Period, 
             y=`5-jaarsprevalentie`))+
  geom_point()+
  theme_bw()+
  facet_wrap(~Kankersoort, ncol=1, scales = "free")+
  labs(x="Year", 
       y="Number", 
       col="Period", 
       title="Total cancer prevalence in the Netherlands", 
       subtitle="Five-year prevalence per cancer",
       caption="Source: IKNL - NKR data ")


Borstkanker_Prevalantie_vijf_jaar <- read_excel("Data/IKNL/NKR/Borstkanker_Prevalantie_vijf_jaar.xlsx")
colnames(Borstkanker_Prevalantie_vijf_jaar)
str(Borstkanker_Prevalantie_vijf_jaar)
Borstkanker_Prevalantie_vijf_jaar$Leeftijdsgroep
length(table(Borstkanker_Prevalantie_vijf_jaar$Leeftijdsgroep))
leeftijd_names<-c("0-4", "5-9", "10-14", 
                  "15-19", "20-24", "25-29", "30-34",
                  "35-39", "40-44", "45-49", "50-54",
                  "55-59", "60-64", "65-69", "70-74",
                  "75-79", "80-84", "85+")
Borstkanker_Prevalantie_vijf_jaar$Leeftijdsgroep<-factor(Borstkanker_Prevalantie_vijf_jaar$Leeftijdsgroep, 
                                               levels=leeftijd_names)
colnames(Borstkanker_Prevalantie_vijf_jaar)[3]<-"Prevalentie"

Borstkanker_Prevalantie_vijf_jaar%>%
  select(-Teken)%>%
  drop_na()%>%
  group_by(Jaar)%>%
  summarise(total=sum(Prevalentie))%>%
  mutate(Period=if_else(Jaar<2020, "pre-Covid","Covid"))%>%
  ggplot(., 
         aes(x=Jaar, 
             fill=Period, 
             y=total))+
  geom_bar(stat="identity")+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))+
  labs(x="Year", 
       y="Number", 
       col="Period", 
       title="Breast cancer prevalence in the Netherlands", 
       subtitle="Five-year prevalence",
       caption="Source: IKNL - NKR data ")


Borstkanker_Prevalantie_vijf_jaar%>%
  mutate(Period=if_else(Jaar<2020, "pre-Covid","Covid"))%>%
  ggplot(., 
         aes(x=Jaar, 
             fill=Period, 
             y=Prevalentie))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))+
  labs(x="Year", 
       y="Number", 
       col="Period", 
       title="Breast cancer prevalence in the Netherlands", 
       subtitle="Five-year prevalence",
       caption="Source: IKNL - NKR data ")

Borstkanker_Prevalantie_vijf_jaar%>%
  mutate(Period=if_else(Jaar<2020, "pre-Covid","Covid"))%>%
  filter(!Leeftijdsgroep%in%c("0-4", "5-9", "10-14", "15-19","20-24" ))%>%
  ggplot(., 
         aes(x=Jaar, 
             col=Period, 
             y=`5-jaarsprevalentie`))+
  geom_point()+
  geom_line(aes(group=1))+
  theme_bw()+
  facet_wrap(~Leeftijdsgroep, ncol=5, scales = "free")+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))+
  labs(x="Year", 
       y="Number", 
       col="Period", 
       title="Breast cancer prevalence in the Netherlands", 
       subtitle="Five-year prevalence per age category",
       caption="Source: IKNL - NKR data ")

Baarmoederhals_Prevalantie_vijf_jaar <- read_excel("Data/IKNL/NKR/Baarmoederhals_Prevalantie_vijf_jaar.xlsx")
colnames(Baarmoederhals_Prevalantie_vijf_jaar)[3]<-"Prevalentie"

Baarmoederhals_Prevalantie_vijf_jaar%>%
  select(-Teken)%>%
  drop_na()%>%
  group_by(Jaar)%>%
  summarise(total=sum(Prevalentie))%>%
  mutate(Period=if_else(Jaar<2020, "pre-Covid","Covid"))%>%
  ggplot(., 
         aes(x=Jaar, 
             fill=Period, 
             y=total))+
  geom_bar(stat="identity")+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))+
  labs(x="Year", 
       y="Number", 
       col="Period", 
       title="Cervical cancer prevalence in the Netherlands", 
       subtitle="Five-year prevalence",
       caption="Source: IKNL - NKR data ")


Baarmoederhals_Prevalantie_vijf_jaar%>%
  mutate(Period=if_else(Jaar<2020, "pre-Covid","Covid"))%>%
  ggplot(., 
         aes(x=Jaar, 
             fill=Period, 
             y=Prevalentie))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))+
  labs(x="Year", 
       y="Number", 
       col="Period", 
       title="Cervical cancer prevalence in the Netherlands", 
       subtitle="Five-year prevalence",
       caption="Source: IKNL - NKR data ")

Dikkedarm_Prevalantie_vijf_jaar <- read_excel("Data/IKNL/NKR/Dikkedarm_Prevalantie_vijf_jaar.xlsx")
colnames(Dikkedarm_Prevalantie_vijf_jaar)[3]<-"Prevalentie"

Dikkedarm_Prevalantie_vijf_jaar%>%
  select(-Teken)%>%
  drop_na()%>%
  group_by(Jaar)%>%
  summarise(total=sum(Prevalentie))%>%
  mutate(Period=if_else(Jaar<2020, "pre-Covid","Covid"))%>%
  ggplot(., 
         aes(x=Jaar, 
             fill=Period, 
             y=total))+
  geom_bar(stat="identity")+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))+
  labs(x="Year", 
       y="Number", 
       col="Period", 
       title="Colon cancer prevalence in the Netherlands", 
       subtitle="Five-year prevalence",
       caption="Source: IKNL - NKR data ")


Dikkedarm_Prevalantie_vijf_jaar%>%
  mutate(Period=if_else(Jaar<2020, "pre-Covid","Covid"))%>%
  ggplot(., 
         aes(x=Jaar, 
             fill=Period, 
             y=Prevalentie))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))+
  labs(x="Year", 
       y="Number", 
       col="Period", 
       title="Colon cancer prevalence in the Netherlands", 
       subtitle="Five-year prevalence",
       caption="Source: IKNL - NKR data ")



#### LOAD, WRANGLE AND EXPLORE IKNL DATA   - CANCER MORTALITY ----
Borstkanker_Sterfte <- read_excel("Data/IKNL/NKR/Borstkanker_Sterfte.xlsx")
View(Borstkanker_Sterfte)
colnames(Borstkanker_Sterfte)
str(Borstkanker_Sterfte)
Borstkanker_Sterfte$Leeftijdsgroep
length(table(Borstkanker_Sterfte$Leeftijdsgroep))
leeftijd_names<-c("0-4", "5-9", "10-14", 
                  "15-19", "20-24", "25-29", "30-34",
                  "35-39", "40-44", "45-49", "50-54",
                  "55-59", "60-64", "65-69", "70-74",
                  "75-79", "80-84", "85+")
Borstkanker_Sterfte$Leeftijdsgroep<-factor(Borstkanker_Sterfte$Leeftijdsgroep, 
                                                         levels=leeftijd_names)
Borstkanker_Sterfte%>%
  select(-Teken)%>%
  drop_na()%>%
  group_by(Jaar)%>%
  summarise(total=sum(Aantal))%>%
  mutate(Period=if_else(Jaar<2020, "pre-Covid","Covid"))%>%
  ggplot(., 
         aes(x=Jaar, 
             fill=Period, 
             y=total))+
  geom_bar(stat="identity")+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))+
  labs(x="Year", 
       y="Number", 
       col="Period", 
       title="Breast cancer mortality in the Netherlands", 
       caption="Source: IKNL - NKR data ")

Borstkanker_Sterfte%>%
  filter(!Leeftijdsgroep=="85+")%>%
  ggplot(., 
         aes(x=Jaar, 
             y=Leeftijdsgroep, 
             fill=Aantal))+
  geom_tile()+
  scale_fill_viridis_c(option = "turbo")+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title = "Breast cancer mortality in the Netherlands",
       subtitle= "excluding age:85+",
       caption="Source: IKNL - NKR data",
       x="Year",
       y="Age group", 
       fill="#")

Borstkanker_Sterfte%>%
  filter(!Leeftijdsgroep=="85+")%>%
  rename(Totaal=Aantal)%>%
  mutate(Period=if_else(Jaar<2020, "pre-Covid","Covid"))%>%
  filter(Leeftijdsgroep%in%c("45-49","50-54","55-59","60-64",
                             "65-69","70-74","75-79","80-84","85+"))%>%
  ggplot(., 
         aes(x=Jaar, 
             y=Totaal,
             fill=Period))+
  geom_bar(stat = "identity")+
  facet_wrap(~Leeftijdsgroep, ncol=4, scales="free")+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))+
  labs(title = "Breast cancer mortality in the Netherlands",
       caption="Source: IKNL - NKR data",
       x="Year",
       col="Sex", 
       y="#")


Baarmoederhals_Sterfte <- read_excel("Data/IKNL/NKR/Baarmoederhals_Sterfte.xlsx")
View(Baarmoederhals_Sterfte)
View(Baarmoederhals_Sterfte)
colnames(Baarmoederhals_Sterfte)
str(Baarmoederhals_Sterfte)
Baarmoederhals_Sterfte$Leeftijdsgroep
length(table(Baarmoederhals_Sterfte$Leeftijdsgroep))
leeftijd_names<-c("0-4", "5-9", "10-14", 
                  "15-19", "20-24", "25-29", "30-34",
                  "35-39", "40-44", "45-49", "50-54",
                  "55-59", "60-64", "65-69", "70-74",
                  "75-79", "80-84", "85+")
Baarmoederhals_Sterfte$Leeftijdsgroep<-factor(Baarmoederhals_Sterfte$Leeftijdsgroep, 
                                           levels=leeftijd_names)
Baarmoederhals_Sterfte%>%
  select(-Teken)%>%
  drop_na()%>%
  group_by(Jaar)%>%
  summarise(total=sum(Aantal))%>%
  mutate(Period=if_else(Jaar<2020, "pre-Covid","Covid"))%>%
  ggplot(., 
         aes(x=Jaar, 
             fill=Period, 
             y=total))+
  geom_bar(stat="identity")+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))+
  labs(x="Year", 
       y="Number", 
       col="Period", 
       title="Cervical cancer mortality in the Netherlands", 
       caption="Source: IKNL - NKR data ")

Baarmoederhals_Sterfte%>%
  ggplot(., 
         aes(x=Jaar, 
             y=Leeftijdsgroep, 
             fill=Aantal))+
  geom_tile()+
  scale_fill_viridis_c(option = "turbo")+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title = "Cervical cancer mortality in the Netherlands",
       caption="Source: IKNL - NKR data",
       x="Year",
       y="Age group", 
       fill="#")


Baarmoederhals_Sterfte%>%
  rename(Totaal=Aantal)%>%
  mutate(Period=if_else(Jaar<2020, "pre-Covid","Covid"))%>%
  filter(Leeftijdsgroep%in%c("20-24", "25-29", "30-34",
                             "35-39", "40-44", "45-49", "50-54",
                             "55-59", "60-64", "65-69", "70-74",
                             "75-79", "80-84", "85+"))%>%
  ggplot(., 
         aes(x=Jaar, 
             y=Totaal,
             fill=Period))+
  geom_bar(stat = "identity")+
  facet_wrap(~Leeftijdsgroep, ncol=2, scales="free")+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))+
  labs(title = "Cervical cancer mortality in the Netherlands",
       caption="Source: IKNL - NKR data",
       x="Year",
       col="Sex", 
       y="#")


DikkeDarm_Leeftijd_Sterfte_totaal <- read_excel("Data/IKNL/NKR/DikkeDarm_Leeftijd_Sterfte_totaal.xlsx")
View(DikkeDarm_Leeftijd_Sterfte_totaal)
View(DikkeDarm_Leeftijd_Sterfte_totaal)
colnames(DikkeDarm_Leeftijd_Sterfte_totaal)
str(DikkeDarm_Leeftijd_Sterfte_totaal)
DikkeDarm_Leeftijd_Sterfte_totaal$Leeftijdsgroep
length(table(DikkeDarm_Leeftijd_Sterfte_totaal$Leeftijdsgroep))
leeftijd_names<-c("0-4", "5-9", "10-14", 
                  "15-19", "20-24", "25-29", "30-34",
                  "35-39", "40-44", "45-49", "50-54",
                  "55-59", "60-64", "65-69", "70-74",
                  "75-79", "80-84", "85+")
DikkeDarm_Leeftijd_Sterfte_totaal$Leeftijdsgroep<-factor(DikkeDarm_Leeftijd_Sterfte_totaal$Leeftijdsgroep, 
                                              levels=leeftijd_names)
DikkeDarm_Leeftijd_Sterfte_totaal%>%
  select(-Teken)%>%
  drop_na()%>%
  group_by(Jaar)%>%
  summarise(total=sum(Aantal))%>%
  mutate(Period=if_else(Jaar<2020, "pre-Covid","Covid"))%>%
  ggplot(., 
         aes(x=Jaar, 
             fill=Period, 
             y=total))+
  geom_bar(stat="identity")+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))+
  labs(x="Year", 
       y="Number", 
       col="Period", 
       title="Colon cancer mortality in the Netherlands", 
       caption="Source: IKNL - NKR data ")

DikkeDarm_Leeftijd_Sterfte_totaal%>%
  ggplot(., 
         aes(x=Jaar, 
             y=Leeftijdsgroep, 
             fill=Aantal))+
  geom_tile()+
  scale_fill_viridis_c(option = "turbo")+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title = "Colon cancer mortality in the Netherlands",
       caption="Source: IKNL - NKR data",
       x="Year",
       y="Age group", 
       fill="#")


DikkeDarm_Leeftijd_Sterfte_Vrouw <- read_excel("Data/IKNL/NKR/DikkeDarm_Leeftijd_Sterfte_Vrouw.xlsx")
DikkeDarm_Leeftijd_Sterfte_totaal
DikkeDarm_Leeftijd_Sterfte_totaal%>%
  rename(Totaal=Aantal)%>%
  left_join(., DikkeDarm_Leeftijd_Sterfte_Vrouw, by=c("Jaar", "Leeftijdsgroep"))%>%
  select(c(Jaar, Leeftijdsgroep,Totaal,Aantal))%>%
  rename(Female=Aantal)%>%
  mutate(Male=Totaal-Female)%>%
  select(-Totaal)%>%
  pivot_longer(cols = -c(Jaar,Leeftijdsgroep),
               names_to = "Geslacht",
               values_to = "Aantal",
               values_drop_na = TRUE)%>%
    ggplot(., 
         aes(x=Jaar, 
             y=Leeftijdsgroep, 
             fill=Aantal))+
  geom_tile()+
  facet_wrap(~Geslacht)+
  scale_fill_viridis_c(option = "turbo")+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))+
  labs(title = "Colon cancer mortality in the Netherlands",
       caption="Source: IKNL - NKR data",
       x="Year",
       y="Age group", 
       fill="#")

DikkeDarm_Leeftijd_Sterfte_totaal%>%
  rename(Totaal=Aantal)%>%
  left_join(., DikkeDarm_Leeftijd_Sterfte_Vrouw, by=c("Jaar", "Leeftijdsgroep"))%>%
  select(c(Jaar, Leeftijdsgroep,Totaal,Aantal))%>%
  rename(Female=Aantal)%>%
  mutate(Male=Totaal-Female)%>%
  select(-Totaal)%>%
  pivot_longer(cols = -c(Jaar,Leeftijdsgroep),
               names_to = "Geslacht",
               values_to = "Aantal",
               values_drop_na = TRUE)%>%
  ggplot(., 
         aes(x=Leeftijdsgroep, 
             y=Jaar, 
             fill=Aantal))+
  geom_tile()+
  facet_wrap(~Geslacht)+
  scale_fill_viridis_c(option = "turbo")+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))+
  labs(title = "Colon cancer mortality in the Netherlands",
       caption="Source: IKNL - NKR data",
       y="Year",
       x="Age group", 
       fill="#")

DikkeDarm_Leeftijd_Sterfte_totaal%>%
  rename(Totaal=Aantal)%>%
  left_join(., DikkeDarm_Leeftijd_Sterfte_Vrouw, by=c("Jaar", "Leeftijdsgroep"))%>%
  select(c(Jaar, Leeftijdsgroep,Totaal,Aantal))%>%
  rename(Female=Aantal)%>%
  mutate(Male=Totaal-Female)%>%
  select(-Totaal)%>%
  pivot_longer(cols = -c(Jaar,Leeftijdsgroep),
               names_to = "Geslacht",
               values_to = "Aantal",
               values_drop_na = TRUE)%>%
  mutate(Period = if_else(Jaar<2020, "Pre-Covid", "Covid"))%>%
  filter(Leeftijdsgroep%in%c("45-49","50-54","55-59","60-64",
                       "65-69","70-74","75-79","80-84","85+"))%>%
  ggplot(., 
         aes(x=Jaar, 
             y=Aantal,
             fill=Geslacht))+
  geom_bar(stat = "identity")+
  geom_vline(xintercept = "2020", lty=2, col="black")+
  facet_wrap(~Leeftijdsgroep, ncol=3, scales="free")+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))+
  labs(title = "Colon cancer mortality in the Netherlands",
       caption="Source: IKNL - NKR data",
       x="Year",
       col="Sex", 
       y="#")

DikkeDarm_Leeftijd_Sterfte_totaal%>%
  rename(Totaal=Aantal)%>%
  left_join(., DikkeDarm_Leeftijd_Sterfte_Vrouw, by=c("Jaar", "Leeftijdsgroep"))%>%
  select(c(Jaar, Leeftijdsgroep,Totaal,Aantal))%>%
  rename(Female=Aantal)%>%
  mutate(Male=Totaal-Female)%>%
  select(-Totaal)%>%
  pivot_longer(cols = -c(Jaar,Leeftijdsgroep),
               names_to = "Geslacht",
               values_to = "Aantal",
               values_drop_na = TRUE)%>%
  mutate(Period = if_else(Jaar<2020, "Pre-Covid", "Covid"))%>%
  filter(Leeftijdsgroep%in%c("45-49","50-54","55-59","60-64",
                             "65-69","70-74","75-79","80-84","85+"))%>%
  ggplot(., 
       aes(x=as.numeric(Jaar), 
           fill=Period, 
           y=Aantal))+
  geom_bar(stat="identity")+
  facet_grid(Geslacht~Leeftijdsgroep)+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))+
  labs(title = "Colon cancer mortality in the Netherlands",
       caption="Source: IKNL - NKR data",
       x="Year",
       col="Sex", 
       y="#")






#### LOAD, WRANGLE AND EXPLORE IKNL DATA   - CANCER SURVIVAL ----
Borstkanker_Overleving <- read_excel("Data/IKNL/NKR/Borstkanker_Overleving_vijf_jaar_TNM_vanaf_2010.xlsx")
colnames(Borstkanker_Overleving)
colnames(Borstkanker_Overleving)<-c("Follow_up", "Stage","Relative_survival","Number_of_patients","Sign")
str(Borstkanker_Overleving)
Borstkanker_Overleving$Follow_up_num<-Borstkanker_Overleving$Follow_up
Borstkanker_Overleving$Follow_up<-factor(Borstkanker_Overleving$Follow_up, 
                                            levels=c("0","1","2","3","4","5",
                                                     "6","7","8","9","10"))

Borstkanker_Overleving%>%
  ggplot(aes(x=Follow_up, 
             y=Number_of_patients, 
             col=Stage, 
             group=Stage))+
  geom_point()+
  geom_line()+
  theme_bw()+
  labs(x="Follow up since diagnosis (years)",
       y="Number of patients", 
       col="Tumor stage", 
       title="Number of patients alive following diagnosis for breast cancer in the Netherlands", 
       subtitle="Five-year survival rates per TNM stage from 2010 onwards", 
       caption="Source: IKNL-NKR")


Borstkanker_Overleving%>%
  ggplot(aes(x=Relative_survival, 
             y=Number_of_patients, 
             label=Follow_up))+
  geom_point(aes(col=Follow_up, 
             group=Follow_up), 
             shape=15, size=2, show.legend = FALSE)+
  geom_line(aes(group=1), col="grey", lty=2)+
  geom_label(check_overlap = TRUE)+
  facet_grid(~Stage)+
  theme_bw()+
  scale_x_reverse()+
  theme(legend.position = "bottom")+
  labs(y="Number of patients", 
       x="Relative survival",
       title="Number of patients alive following diagnosis for breast cancer in the Netherlands", 
       subtitle="Five-year survival rates per TNM stage from 2010 onwards", 
       caption="Source: IKNL-NKR")



Baarmoederhalskanker_Overleving <- read_excel("Data/IKNL/NKR/Baarmoederhals_Overleving_vijf_jaar_TNM_vanaf_2010.xlsx")
colnames(Baarmoederhalskanker_Overleving)
colnames(Baarmoederhalskanker_Overleving)<-c("Follow_up", "Stage","Relative_survival","Number_of_patients","Sign")
str(Baarmoederhalskanker_Overleving)
Baarmoederhalskanker_Overleving$Follow_up_num<-Baarmoederhalskanker_Overleving$Follow_up
Baarmoederhalskanker_Overleving$Follow_up<-factor(Baarmoederhalskanker_Overleving$Follow_up, 
                                         levels=c("0","1","2","3","4","5",
                                                  "6","7","8","9","10"))

Baarmoederhalskanker_Overleving%>%
  ggplot(aes(x=Follow_up, 
             y=Number_of_patients, 
             col=Stage, 
             group=Stage))+
  geom_point()+
  geom_line()+
  theme_bw()+
  labs(x="Follow up since diagnosis (years)",
       y="Number of patients", 
       col="Tumor stage", 
       title="Number of patients alive following diagnosis for cervical cancer in the Netherlands", 
       subtitle="Five-year survival rates per TNM stage from 2010 onwards", 
       caption="Source: IKNL-NKR")


Baarmoederhalskanker_Overleving%>%
  ggplot(aes(x=Relative_survival, 
             y=Number_of_patients, 
             label=Follow_up))+
  geom_point(aes(col=Follow_up, 
                 group=Follow_up), 
             shape=15, size=2, show.legend = FALSE)+
  geom_line(aes(group=1), col="grey", lty=2)+
  geom_label(check_overlap = TRUE)+
  facet_grid(~Stage)+
  theme_bw()+
  scale_x_reverse()+
  theme(legend.position = "bottom")+
  labs(y="Number of patients", 
       x="Relative survival",
       title="Number of patients alive following diagnosis for cervical cancer in the Netherlands", 
       subtitle="Five-year survival rates per TNM stage from 2010 onwards", 
       caption="Source: IKNL-NKR")


Dikkedarm_Overleving_man <- read_excel("Data/IKNL/NKR/Dikkedarm_Overleving_vijf_jaar_TNM_vanaf_2010_man.xlsx")
colnames(Dikkedarm_Overleving_man)
colnames(Dikkedarm_Overleving_man)<-c("Follow_up", "Stage","Relative_survival","Number_of_patients","Sign")
Dikkedarm_Overleving_man$Follow_up_num<-Dikkedarm_Overleving_man$Follow_up
Dikkedarm_Overleving_man$Follow_up<-factor(Dikkedarm_Overleving_man$Follow_up, 
                                                  levels=c("0","1","2","3","4","5",
                                                           "6","7","8","9","10"))
Dikkedarm_Overleving_vrouw <- read_excel("Data/IKNL/NKR/Dikkedarm_Overleving_vijf_jaar_TNM_vanaf_2010_vrouw.xlsx")
colnames(Dikkedarm_Overleving_vrouw)
colnames(Dikkedarm_Overleving_vrouw)<-c("Follow_up", "Stage","Relative_survival","Number_of_patients","Sign")

Dikkedarm_Overleving_vrouw$Follow_up_num<-Dikkedarm_Overleving_vrouw$Follow_up
Dikkedarm_Overleving_vrouw$Follow_up<-factor(Dikkedarm_Overleving_vrouw$Follow_up, 
                                           levels=c("0","1","2","3","4","5",
                                                    "6","7","8","9","10"))
str(Dikkedarm_Overleving_vrouw)
str(Dikkedarm_Overleving_man)


Dikkedarm_Overleving_vrouw<-Dikkedarm_Overleving_vrouw%>%
  select(-Sign)%>%
  mutate(Sex="Female")
Dikkedarm_Overleving_man<-Dikkedarm_Overleving_man%>%
  select(-Sign)%>%
  mutate(Sex="Male")
combined<-rbind(Dikkedarm_Overleving_vrouw,Dikkedarm_Overleving_man)


combined%>%
  ggplot(aes(x=Follow_up, 
             y=Number_of_patients, 
             col=Stage, 
             group=Stage))+
  geom_point()+
  geom_line()+
  facet_grid(~Sex)+
  theme_bw()+
  labs(x="Follow up since diagnosis (years)",
       y="Number of patients", 
       col="Tumor stage", 
       title="Number of patients alive following diagnosis for cervical cancer in the Netherlands", 
       subtitle="Five-year survival rates per TNM stage from 2010 onwards", 
       caption="Source: IKNL-NKR")

combined%>%
  ggplot(aes(x=Follow_up, 
             y=Number_of_patients, 
             col=Sex, 
             group=Sex))+
  geom_point()+
  geom_line()+
  facet_grid(~Stage)+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Follow up since diagnosis (years)",
       y="Number of patients", 
       col="Sex", 
       title="Number of patients alive following diagnosis for colon cancer in the Netherlands", 
       subtitle="Five-year survival rates per TNM stage from 2010 onwards", 
       caption="Source: IKNL-NKR")

combined%>%
  ggplot(aes(x=Relative_survival, 
             y=Number_of_patients, 
             label=Follow_up, 
             col=Sex))+
  geom_point(aes(group=Follow_up), 
             shape=15, size=2, show.legend = FALSE)+
  geom_line(aes(group=Sex), lty=2)+
  geom_label(check_overlap = TRUE)+
  facet_grid(~Stage)+
  theme_bw()+
  scale_x_reverse()+
  theme(legend.position = "bottom")+
  labs(y="Number of patients", 
       x="Relative survival",
       title="Number of patients alive following diagnosis for colon cancer in the Netherlands", 
       subtitle="Five-year survival rates per TNM stage from 2010 onwards", 
       caption="Source: IKNL-NKR")


combined%>%
  ggplot(aes(x=Relative_survival, 
             y=Number_of_patients, 
             label=Follow_up, 
             col=Sex))+
  geom_point(aes(group=Follow_up), 
             shape=15, size=2, show.legend = FALSE)+
  geom_line(aes(group=Sex), lty=2)+
  geom_label(check_overlap = TRUE)+
  facet_grid(Sex~Stage)+
  theme_bw()+
  scale_x_reverse()+
  theme(legend.position = "bottom")+
  labs(y="Number of patients", 
       x="Relative survival",
       title="Number of patients alive following diagnosis for colon cancer in the Netherlands", 
       subtitle="Five-year survival rates per TNM stage from 2010 onwards", 
       caption="Source: IKNL-NKR")



#### LOAD, WRANGLE AND EXPLORE OECD DATA   - SCREENING ----
OECD_Screen <- read_csv("Data/OECD/Screening/HEALTH_PROC_26062023120134510.csv")
dim(OECD_Screen)
str(OECD_Screen)
colnames(OECD_Screen)
OECD_Screen<-as.data.frame(apply(OECD_Screen, 2, function(x) gsub('\\s+', '', x)))
table(OECD_Screen$Variable)

OECD_Screen %>%
  select(Variable, Year, Value, Country)%>%
  mutate(Value=as.numeric(Value))%>%
  filter(Variable=="Cervicalcancerscreening,programmedata" | 
           Variable=="Breastcancerscreening,programmedata" | 
           Variable=="Colorectalcancerscreening,programmedata")%>%
  separate(Variable, c("first", "second"), sep = ",") %>% 
  select(-second)%>%
  ggplot(.,
         aes(x=Year, fill=Value, y=Country))+
  geom_tile()+
  theme_bw()+
  scale_fill_viridis_c(option="turbo")+
  labs(title="Screening Rates (programme data)", 
       x="Year", 
       y="Country", 
       fill="Rate", 
       caption="Source: OECD")+
  facet_grid(~first)

OECD_Screen %>%
  select(Variable, Year, Value, Country)%>%
  mutate(Value=as.numeric(Value))%>%
  filter(Variable=="Cervicalcancerscreening,surveydata" | 
           Variable=="Breastcancerscreening,surveydata" | 
           Variable=="Colorectalcancerscreening,surveydata")%>%
  separate(Variable, c("first", "second"), sep = ",") %>% 
  select(-second)%>%
  ggplot(.,
         aes(x=Year, fill=Value, y=Country))+
  geom_tile()+
  theme_bw()+
  scale_fill_viridis_c(option="turbo")+
  labs(title="Screening Rates (survey data)", 
       x="Year", 
       y="Country", 
       fill="Rate", 
       caption="Source: OECD")+
  facet_grid(~first)

#### LOAD, WRANGLE AND EXPLORE SVGZ DATA   - SCREENING - TREND ----
SVGZ_screen_borst <- read_delim("Data/StaatVolksGezondZorg/Breast/deelname-bevolkingsonder_trend.csv",
                                delim = ";", 
                                escape_double = FALSE, 
                                trim_ws = TRUE)
colnames(SVGZ_screen_borst)
str(SVGZ_screen_borst)
ggplot(SVGZ_screen_borst, 
       aes(x=Category, y=Percentage))+
  geom_point()+
  geom_line()+
  theme_bw()

SVGZ_screen_colon <- read_delim("Data/StaatVolksGezondZorg/Colon/trend-in-deelname-bevolk.csv",
                                delim = ";", 
                                escape_double = FALSE, 
                                trim_ws = TRUE)
colnames(SVGZ_screen_colon)
str(SVGZ_screen_colon)
ggplot(SVGZ_screen_colon, 
       aes(x=Category, y=Totaal))+
  geom_point()+
  geom_line()+
  theme_bw()

ggplot(SVGZ_screen_borst, 
       aes(x=Category, y=Percentage, col="Breast"))+
  geom_point()+
  geom_line()+
  geom_point(data=SVGZ_screen_colon, aes(x=Category, y=Totaal, col="Colon"))+
  geom_line(data=SVGZ_screen_colon, aes(x=Category, y=Totaal, col="Colon"))+
  theme_bw()

SVGZ_screen_cervix <- read_delim("Data/StaatVolksGezondZorg/Cervix/trend-deelname-bevolking.csv",
                                 delim = ";", 
                                 escape_double = FALSE, 
                                 trim_ws = TRUE)
colnames(SVGZ_screen_cervix)
str(SVGZ_screen_cervix)
ggplot(SVGZ_screen_cervix, 
       aes(x=`Jaar van uitnodiging`, y=Totaal))+
  geom_point()+
  geom_line()+
  theme_bw()

ggplot(SVGZ_screen_borst, 
       aes(x=Category, y=Percentage, col="Breast"))+
  geom_point()+
  geom_line()+
  geom_point(data=SVGZ_screen_colon, aes(x=Category, y=Totaal, col="Colon"))+
  geom_line(data=SVGZ_screen_colon, aes(x=Category, y=Totaal, col="Colon"))+
  geom_point(data=SVGZ_screen_cervix, aes(x=`Jaar van uitnodiging`, y=Totaal, col="Cervix"))+
  geom_line(data=SVGZ_screen_cervix, aes(x=`Jaar van uitnodiging`, y=Totaal, col="Cervix"))+
  theme_bw()+
  labs(x="Year", 
       y="Percentage", 
       col="Cancer",
       title="Percentage screening in the Netherlands",
       subtitle="of patients invited for screening",
       caption="Source: De Staat van Volksgezondheid en Zorg")



#### LOAD, WRANGLE AND EXPLORE SVGZ DATA   - SCREENING - AGE ----
SVGZ_screen_borst_leeft <- read_delim("Data/StaatVolksGezondZorg/Breast/deelname-bevolkingsonder.csv", 
                                      delim = ";", 
                                      escape_double = FALSE, 
                                      trim_ws = TRUE)
SVGZ_screen_borst_leeft%>%
  select(-Totaal)%>%
  pivot_longer(cols = -Category,
               names_to = "Leeftijd",
               values_to = "Percentage",
               values_drop_na = TRUE)%>%
  ggplot(., 
         aes(x=Category, y=Percentage, col=Leeftijd))+
  geom_point()+
  geom_line()+
  labs(x="Jaar")+
  theme_bw()

SVGZ_screen_colon_leeft <- read_delim("Data/StaatVolksGezondZorg/Colon/trend-in-deelname-bevolk.csv", 
                                      delim = ";", 
                                      escape_double = FALSE, 
                                      trim_ws = TRUE)
SVGZ_screen_colon_leeft%>%
  select(-Totaal)%>%
  pivot_longer(cols = -Category,
               names_to = "Leeftijd",
               values_to = "Percentage",
               values_drop_na = TRUE)%>%
  ggplot(., 
         aes(x=Category, y=Percentage, col=Leeftijd))+
  geom_point()+
  geom_line()+
  labs(x="Jaar")+
  theme_bw()

SVGZ_screen_cervix_leeft <- read_delim("Data/StaatVolksGezondZorg/Cervix/trend-deelname-bevolking.csv", 
                                       delim = ";", 
                                       escape_double = FALSE, 
                                       trim_ws = TRUE)
colnames(SVGZ_screen_cervix_leeft)[1]<-"Category"
SVGZ_screen_cervix_leeft%>%
  select(-Totaal)%>%
  pivot_longer(cols = -Category,
               names_to = "Leeftijd",
               values_to = "Percentage",
               values_drop_na = TRUE)%>%
  ggplot(., 
         aes(x=Category, y=Percentage, col=Leeftijd))+
  geom_point()+
  geom_line()+
  labs(x="Jaar")+
  theme_bw()

SVGZ_screen_cervix_leeft_long<-SVGZ_screen_cervix_leeft%>%
  select(-Totaal)%>%
  pivot_longer(cols = -Category,
               names_to = "Leeftijd",
               values_to = "Percentage",
               values_drop_na = TRUE)%>%
  mutate(Kanker = "Cervix")
SVGZ_screen_borst_leeft_long<-SVGZ_screen_borst_leeft%>%
  select(-Totaal)%>%
  pivot_longer(cols = -Category,
               names_to = "Leeftijd",
               values_to = "Percentage",
               values_drop_na = TRUE)%>%
  mutate(Kanker = "Borst")
SVGZ_screen_colon_leeft_long<-SVGZ_screen_colon_leeft%>%
  select(-Totaal)%>%
  pivot_longer(cols = -Category,
               names_to = "Leeftijd",
               values_to = "Percentage",
               values_drop_na = TRUE)%>%
  mutate(Kanker = "Colon")
colnames(SVGZ_screen_cervix_leeft_long)
colnames(SVGZ_screen_colon_leeft_long)
colnames(SVGZ_screen_borst_leeft_long)

combined<-rbind(SVGZ_screen_cervix_leeft_long,
                SVGZ_screen_colon_leeft_long, 
                SVGZ_screen_borst_leeft_long)
ggplot(combined, 
       aes(x=Category, fill=Percentage, y=Leeftijd))+
  geom_tile()+
  facet_grid(~Kanker)+
  scale_fill_viridis_c(option="turbo")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Year", 
       y="Age", 
       fill="Percentage",
       title="Percentage screening in the Netherlands",
       subtitle="of patients invited for screening",
       caption="Source: De Staat van Volksgezondheid en Zorg")

ggplot(combined, 
       aes(x=Category, y=Percentage, col=Kanker))+
  geom_point()+
  geom_line()+
  facet_grid(~Leeftijd)+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))+
  labs(x="Year", 
       y="Percentage", 
       col="Cancer",
       title="Percentage screening in the Netherlands",
       subtitle="of patients invited for screening",
       caption="Source: De Staat van Volksgezondheid en Zorg")

combined%>%
  mutate(Period = if_else(Category<2020, "Pre-Covid", "Covid"))%>%
  ggplot(., 
       aes(x=Category, y=Percentage, col=Period, 
           group=1))+
  geom_point()+
  geom_line()+
  facet_grid(Kanker~Leeftijd, scales="free")+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))+
  labs(x="Year", 
       y="Percentage", 
       col="Cancer",
       title="Percentage screening in the Netherlands",
       subtitle="of patients invited for screening",
       caption="Source: De Staat van Volksgezondheid en Zorg")



#### LOAD, WRANGLE AND EXPLORE SVGZ DATA   - SCREENING - NEW CASES ----
SVGZ_screen_borst_NG  <- read_delim("Data/StaatVolksGezondZorg/Breast/aantal-nieuwe-gevallen-v.csv", 
                                    delim = ";", escape_double = FALSE, trim_ws = TRUE)
SVGZ_screen_colon_NG  <- read_delim("Data/StaatVolksGezondZorg/Colon/aantal-nieuwe-gevallen-v (1).csv", 
                                    delim = ";", escape_double = FALSE, trim_ws = TRUE)
SVGZ_screen_cervix_NG  <- read_delim("Data/StaatVolksGezondZorg/Cervix/aantal-nieuwe-gevallen-v (2).csv", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
SVGZ_screen_colon_NG<-SVGZ_screen_colon_NG%>%select(Category, Vrouwen)

SVGZ_screen_colon_NG$Category <- reorder(SVGZ_screen_colon_NG$Category , 
                                         as.numeric(gsub("-\\d+","", 
                                                         SVGZ_screen_colon_NG$Category)))
SVGZ_screen_breast_NG$Category <- reorder(SVGZ_screen_breast_NG$Category, 
                                          as.numeric(gsub("-\\d+","", 
                                                          SVGZ_screen_breast_NG$Category)))
SVGZ_screen_cervix_NG$Category <- reorder(SVGZ_screen_cervix_NG$Category, 
                                          as.numeric(gsub("-\\d+","", 
                                                          SVGZ_screen_cervix_NG$Category)))

ggplot()+
  geom_point(data=SVGZ_screen_colon_NG, aes(x=factor(Category), y=Vrouwen, col="Colon"))+
  geom_line(data=SVGZ_screen_colon_NG, aes(x=factor(Category), y=Vrouwen, col="Colon", group=1))+
  geom_point(data=SVGZ_screen_borst_NG, aes(x=factor(Category), y=Vrouwen, col="Borst"))+
  geom_line(data=SVGZ_screen_borst_NG, aes(x=factor(Category), y=Vrouwen, col="Borst", group=1))+
  geom_point(data=SVGZ_screen_cervix_NG, aes(x=factor(Category), y=Vrouwen, col="Cervix"))+
  geom_line(data=SVGZ_screen_cervix_NG, aes(x=factor(Category), y=Vrouwen, col="Cervix", group=1))+
  labs(x="Leeftijd", 
       y="Aantal nieuwe gevallen (vrouw)", 
       title="Aantal nieuwe gevallen van kanker in 2022", 
       col="Kanker")+
  theme_bw()
#### LOAD, WRANGLE AND EXPLORE VZINFO DATA - BREAST ----
vzinfo_sterfte_borstkanker<-read_delim("Data/VZinfo/Breast/vzinfo_sterfte_borstkanker_20230615.csv",
                                       delim = ";", 
                                       escape_double = FALSE, 
                                       trim_ws = TRUE)
str(vzinfo_sterfte_borstkanker)
dim(vzinfo_sterfte_borstkanker)
table(vzinfo_sterfte_borstkanker$perioden)
table(vzinfo_sterfte_borstkanker$doodsoorzaak)
table(vzinfo_sterfte_borstkanker$geslacht)
table(vzinfo_sterfte_borstkanker$leeftijd)
vzinfo_sterfte_borstkanker<-vzinfo_sterfte_borstkanker%>%filter(!leeftijd=="Totaal")
vzinfo_sterfte_borstkanker$leeftijd <- reorder(vzinfo_sterfte_borstkanker$leeftijd, 
                       as.numeric(gsub("-\\d+","", vzinfo_sterfte_borstkanker$leeftijd)))

vzinfo_sterfte_borstkanker%>%
  filter(!geslacht=="Totaal")%>%
  mutate(perioden=factor(perioden),
         geslacht=factor(geslacht))%>%
  mutate(geslacht=recode_factor(geslacht, Vrouwen = "Female", 
                Mannen = "Male"))%>%
  mutate(aantal_per_100000_gestandaardiseerd = as.numeric(gsub(",", "",
                                                               aantal_per_100000_gestandaardiseerd)))%>%
  mutate(aantal_per_100000 = as.numeric(gsub(",", "", 
                                                               aantal_per_100000_gestandaardiseerd)))%>%
  arrange(desc(leeftijd))%>%
  filter(geslacht=="Female")%>%
  ggplot(., 
       aes(x=perioden, 
           y=leeftijd, 
           fill=aantal_per_100000_gestandaardiseerd))+
  geom_tile()+
  facet_wrap(~geslacht, ncol=1)+
  scale_fill_viridis_c(option="turbo")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(title = "Breast cancer mortality in the Netherlands",
       subtitle= "per 100k standardized",
       caption="Source: VZinfo",
       x="Year",
       y="Age group", 
       fill="# per 100k standardized")




vzinfo_sterfte_borstkanker%>%
  filter(!geslacht=="Totaal")%>%
  mutate(perioden=factor(perioden),
         geslacht=factor(geslacht))%>%
  mutate(geslacht=recode_factor(geslacht, Vrouwen = "Female", 
                                Mannen = "Male"))%>%
  mutate(aantal_per_100000_gestandaardiseerd = as.numeric(gsub(",", "",
                                                               aantal_per_100000_gestandaardiseerd)))%>%
  mutate(aantal_per_100000 = as.numeric(gsub(",", "", 
                                             aantal_per_100000_gestandaardiseerd)))%>%
  arrange(desc(leeftijd))%>%
  filter(geslacht=="Female")%>%
  filter(!leeftijd=="85+")%>%
  ggplot(., 
         aes(x=perioden, 
             y=leeftijd, 
             fill=aantal_per_100000_gestandaardiseerd))+
  geom_tile()+
  facet_wrap(~geslacht, ncol=1)+
  scale_fill_viridis_c(option="turbo")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(title = "Breast cancer mortality in the Netherlands",
       subtitle= "per 100k standardized - excluding age:85+",
       caption="Source: VZinfo",
       x="Year",
       y="Age group", 
       fill="# per 100k standardized")


#### LOAD, WRANGLE AND EXPLORE VZINFO DATA - CERVIX ----
vzinfo_sterfte_baarmoederhalskanker<-read_delim("Data/VZinfo/Cervix/vzinfo_sterfte_baarmoederhalskanker_20230615.csv",
                                       delim = ";", 
                                       escape_double = FALSE, 
                                       trim_ws = TRUE)
str(vzinfo_sterfte_baarmoederhalskanker)
dim(vzinfo_sterfte_baarmoederhalskanker)
table(vzinfo_sterfte_baarmoederhalskanker$perioden)
table(vzinfo_sterfte_baarmoederhalskanker$doodsoorzaak)
table(vzinfo_sterfte_baarmoederhalskanker$geslacht)
table(vzinfo_sterfte_baarmoederhalskanker$leeftijd)
vzinfo_sterfte_baarmoederhalskanker<-vzinfo_sterfte_baarmoederhalskanker%>%filter(!leeftijd=="Totaal")
vzinfo_sterfte_baarmoederhalskanker$leeftijd <- reorder(vzinfo_sterfte_baarmoederhalskanker$leeftijd, 
                                               as.numeric(gsub("-\\d+","", vzinfo_sterfte_baarmoederhalskanker$leeftijd)))


g1<-vzinfo_sterfte_baarmoederhalskanker%>%
  filter(!geslacht=="Totaal")%>%
  mutate(perioden=factor(perioden),
         geslacht=factor(geslacht))%>%
  mutate(geslacht=recode_factor(geslacht, Vrouwen = "Female", 
                                Mannen = "Male"))%>%
  arrange(desc(leeftijd))%>%
  filter(geslacht=="Female")%>%
  ggplot(., 
         aes(x=perioden, 
             y=leeftijd, 
             fill=aantal))+
  geom_tile()+
  scale_fill_viridis_c(option="turbo")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(title = "Cervix cancer mortality in the Netherlands",
       x="Year",
       y="Age group", 
       fill="#")

g2<-vzinfo_sterfte_baarmoederhalskanker%>%
  filter(!geslacht=="Totaal")%>%
  mutate(perioden=factor(perioden),
         geslacht=factor(geslacht))%>%
  mutate(geslacht=recode_factor(geslacht, Vrouwen = "Female", 
                                Mannen = "Male"))%>%
  mutate(aantal_per_100000 = as.numeric(gsub(",", "", 
                                             aantal_per_100000)))%>%
  arrange(desc(leeftijd))%>%
  filter(geslacht=="Female")%>%
  ggplot(., 
         aes(x=perioden, 
             y=leeftijd, 
             fill=aantal_per_100000))+
  geom_tile()+
  scale_fill_viridis_c(option="turbo")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Year",
       y="Age group",
       fill="# per 100k")

g3<-vzinfo_sterfte_baarmoederhalskanker%>%
  filter(!geslacht=="Totaal")%>%
  mutate(perioden=factor(perioden),
         geslacht=factor(geslacht))%>%
  mutate(geslacht=recode_factor(geslacht, Vrouwen = "Female", 
                                Mannen = "Male"))%>%
  mutate(aantal_per_100000_gestandaardiseerd = as.numeric(gsub(",", "", 
                                                               aantal_per_100000_gestandaardiseerd)))%>%
  arrange(desc(leeftijd))%>%
  filter(geslacht=="Female")%>%
  ggplot(., 
         aes(x=perioden, 
             y=leeftijd, 
             fill=aantal_per_100000_gestandaardiseerd))+
  geom_tile()+
  scale_fill_viridis_c(option="turbo")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(caption="Source: VZinfo",
       x="Year",
       y="Age group", 
       fill="# per 100k standardized")
gridExtra::grid.arrange(g1,g2,g3,ncol=1)


#### LOAD, WRANGLE AND EXPLORE VZINFO DATA - COLON ----
vzinfo_sterfte_dikkedarmkanker<-read_delim("Data/VZinfo/Colon/vzinfo_sterfte_dikkedarmkanker_20230615.csv",
                                                delim = ";", 
                                                escape_double = FALSE, 
                                                trim_ws = TRUE)
str(vzinfo_sterfte_dikkedarmkanker)
dim(vzinfo_sterfte_dikkedarmkanker)
table(vzinfo_sterfte_dikkedarmkanker$perioden)
table(vzinfo_sterfte_dikkedarmkanker$doodsoorzaak)
table(vzinfo_sterfte_dikkedarmkanker$geslacht)
table(vzinfo_sterfte_dikkedarmkanker$leeftijd)
vzinfo_sterfte_dikkedarmkanker<-vzinfo_sterfte_dikkedarmkanker%>%filter(!leeftijd=="Totaal")
vzinfo_sterfte_dikkedarmkanker$leeftijd <- reorder(vzinfo_sterfte_dikkedarmkanker$leeftijd, 
                                                        as.numeric(gsub("-\\d+","", 
                                                                        vzinfo_sterfte_dikkedarmkanker$leeftijd)))

g1<-vzinfo_sterfte_dikkedarmkanker%>%
  filter(!geslacht=="Totaal")%>%
  mutate(perioden=factor(perioden),
         geslacht=factor(geslacht))%>%
  mutate(geslacht=recode_factor(geslacht, Vrouwen = "Female", 
                                Mannen = "Male"))%>%
  arrange(desc(leeftijd))%>%
  ggplot(., 
         aes(x=perioden, 
             y=leeftijd, 
             fill=aantal))+
  geom_tile()+
  facet_wrap(~geslacht, ncol=2)+
  scale_fill_viridis_c(option="turbo")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(title = "Colon cancer mortality in the Netherlands",
       x="Year",
       y="Age group", 
       fill="#")
g2<-vzinfo_sterfte_dikkedarmkanker%>%
  filter(!geslacht=="Totaal")%>%
  mutate(perioden=factor(perioden),
         geslacht=factor(geslacht))%>%
  mutate(geslacht=recode_factor(geslacht, Vrouwen = "Female", 
                                Mannen = "Male"))%>%
  mutate(aantal_per_100000 = as.numeric(gsub(",", "", 
                                             aantal_per_100000)))%>%
  arrange(desc(leeftijd))%>%
  ggplot(., 
         aes(x=perioden, 
             y=leeftijd, 
             fill=aantal_per_100000))+
  geom_tile()+
  facet_wrap(~geslacht, ncol=2)+
  scale_fill_viridis_c(option="turbo")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Year",
       y="Age group", 
       fill="# per 100k")
g3<-vzinfo_sterfte_dikkedarmkanker%>%
  filter(!geslacht=="Totaal")%>%
  mutate(perioden=factor(perioden),
         geslacht=factor(geslacht))%>%
  mutate(geslacht=recode_factor(geslacht, Vrouwen = "Female", 
                                Mannen = "Male"))%>%
  mutate(aantal_per_100000_gestandaardiseerd = as.numeric(gsub(",", "", 
                                                               aantal_per_100000_gestandaardiseerd)))%>%
  arrange(desc(leeftijd))%>%
  ggplot(., 
         aes(x=perioden, 
             y=leeftijd, 
             fill=aantal_per_100000_gestandaardiseerd))+
  geom_tile()+
  facet_wrap(~geslacht, ncol=2)+
  scale_fill_viridis_c(option="turbo")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(caption="Source: VZinfo",
       x="Year",
       y="Age group", 
       fill="# per 100k standardized")
gridExtra::grid.arrange(g1,g2,g3, ncol=1)

vzinfo_sterfte_dikkedarmkanker%>%
  filter(!geslacht=="Totaal")%>%
  mutate(geslacht=factor(geslacht))%>%
  mutate(geslacht=recode_factor(geslacht, Vrouwen = "Female", 
                                Mannen = "Male"))%>%
  mutate(aantal_per_100000_gestandaardiseerd = as.numeric(gsub(",", "", 
                                                               aantal_per_100000_gestandaardiseerd)))%>%
  mutate(aantal_per_100000 = as.numeric(gsub(",", "", 
                                             aantal_per_100000_gestandaardiseerd)))%>%
  arrange(desc(leeftijd))%>%
  mutate(Period = if_else(perioden<2020, "Pre-Covid", "Covid"))%>%
  filter(perioden>1990)%>%
  filter(leeftijd%in%c("45-49","50-54","55-59","60-64",
                       "65-69","70-74","75-79","80-84","85+"))%>%
  ggplot(., 
         aes(x=perioden, 
             fill=Period, 
             y=aantal_per_100000_gestandaardiseerd))+
  geom_bar(stat="identity")+
  facet_grid(geslacht~leeftijd)+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5))+
  labs(title = "Colon cancer mortality in the Netherlands",
       subtitle= "per 100k standardized",
       caption="Source: VZinfo",
       fill="Period",
       x="Year",
       y="# per 100k standardized")







#### LOAD, WRANGLE AND EXPLORE WMD DATA    - OpenDIS ----

# Een zorgproduct bestaat uit een aantal zorgactiviteiten. 
# Dit zijn onderdelen van de behandeling van een patiënt, 
# zoals bijvoorbeeld een operatie, consult of MRI-scan.

X01_DBC         <- read_csv("Data/OpenDIS/01_DBC.csv")         # DBC-zorgproducten per jaar, specialisme, diagnose
X02_DBC_PROFIEL <- read_csv("Data/OpenDIS/02_DBC_PROFIEL.csv") # Zorgactiviteiten per jaar, specialisme, diagnose, zorgproduct
X03_REF_ZAT     <- read_csv("Data/OpenDIS/03_REF_ZAT.csv")     # Referentietabel zorgactiviteiten
X04_REF_DGN     <- read_csv("Data/OpenDIS/04_REF_DGN.csv")     # Referentietabel diagnoses
X05_REF_ZPD     <- read_csv("Data/OpenDIS/05_REF_ZPD.csv")     # Referentietabel zorgproducten
X06_REF_SPC     <- read_csv("Data/OpenDIS/06_REF_SPC.csv")     # Referentietabel specialismen

dim(X01_DBC)
dim(X04_REF_DGN)

colnames(X01_DBC)
colnames(X02_DBC_PROFIEL)
colnames(X03_REF_ZAT)
colnames(X04_REF_DGN)
colnames(X05_REF_ZPD)
colnames(X06_REF_SPC)

table(X01_DBC$JAAR)
table(X01_DBC$TYPERENDE_DIAGNOSE_CD)
length(unique(X04_REF_DGN$DIAGNOSE_CD))
table(X04_REF_DGN$SPECIALISME_CD)
table(X04_REF_DGN$DIAGNOSE_OMSCHRIJVING)
table(X04_REF_DGN$DIAGNOSE_CD, X04_REF_DGN$DIAGNOSE_OMSCHRIJVING)

X03_REF_ZAT_short<-X03_REF_ZAT%>%select(-c(VERSIE, DATUM_BESTAND, PEILDATUM))
X04_REF_DGN_short<-X04_REF_DGN%>%select(DIAGNOSE_CD, DIAGNOSE_OMSCHRIJVING, SPECIALISME_CD)
X05_REF_ZPD_short<-X05_REF_ZPD%>%select(ZORGPRODUCT_CD, CONSUMENT_OMS)
X06_REF_SPC_short<-X06_REF_SPC%>%select(SPECIALISME_CD , OMSCHRIJVING)%>%rename(OMSCHRIJVING_SPEC = OMSCHRIJVING)

combined_DBC<-X01_DBC%>%
  rename(DIAGNOSE_CD = TYPERENDE_DIAGNOSE_CD, 
         SPECIALISME_CD = BEHANDELEND_SPECIALISME_CD)%>%
  left_join(., X04_REF_DGN_short, by=c("DIAGNOSE_CD", "SPECIALISME_CD"))%>%
  left_join(., X05_REF_ZPD_short, by=c("ZORGPRODUCT_CD"))%>%
  left_join(., X06_REF_SPC_short, by=c("SPECIALISME_CD"))%>%
  distinct()

combined_ZAT<-X02_DBC_PROFIEL%>%
  left_join(., X03_REF_ZAT_short, by=c("ZORGACTIVITEIT_CD", "ZORGPROFIELKLASSE_CD"))%>%
  distinct()

colnames(combined_DBC)
colnames(combined_ZAT)

table(combined_DBC$OMSCHRIJVING_SPEC)
table(combined_ZAT$OMSCHRIJVING)
table(combined_ZAT$ZORGPROFIELKLASSE_OMS)
table(combined_ZAT$ZORGACTIVITEIT_CD)

table(combined_ZAT$BEHANDELEND_SPECIALISME_CD, 
      combined_ZAT$ZORGPROFIELKLASSE_OMS, 
      combined_ZAT$OMSCHRIJVING)


combined_DBC%>%
  filter(grepl('onderzoek',DIAGNOSE_OMSCHRIJVING))%>%
  filter(JAAR<2023)%>%
  group_by(DIAGNOSE_OMSCHRIJVING, JAAR)%>%
  summarise(Total=sum(AANTAL_PAT_PER_ZPD))%>%
  ggplot()+
  geom_tile(aes(y=DIAGNOSE_OMSCHRIJVING, 
                fill=Total, 
                x=factor(JAAR)))+
  theme_bw()+
  scale_fill_viridis_c(option="turbo")+
  labs(fill="Aantal",
       y="Diagnose omschrijving",
       x="Jaar", 
       title="Aantal patienten per jaar per zorgproduct", 
       subtitle="Gezocht op 'onderzoek'",
       caption="Source: OpenDIS")

combined_DBC%>%
  filter(grepl('bevolkingsonderzoek|screening|Screening|Bevolkingsonderzoek',DIAGNOSE_OMSCHRIJVING))%>%
  filter(JAAR<2023)%>%
  group_by(DIAGNOSE_OMSCHRIJVING, JAAR)%>%
  summarise(Total=sum(AANTAL_PAT_PER_ZPD))%>%
  ggplot()+
  geom_tile(aes(y=DIAGNOSE_OMSCHRIJVING, 
                fill=Total, 
                x=factor(JAAR)))+
  theme_bw()+
  scale_fill_viridis_c(option="turbo")+
  labs(fill="Aantal",
       y="Diagnose omschrijving",
       x="Jaar", 
       title="Aantal patienten per jaar per zorgproduct", 
       subtitle="Gezocht op 'bevolkingsonderzoek' en 'screening'",
       caption="Source: OpenDIS")

combined_DBC%>%
  filter(grepl('Kanker|kanker|carcinoom|tumor',DIAGNOSE_OMSCHRIJVING))%>%
  filter(JAAR<2023)%>%
  group_by(DIAGNOSE_OMSCHRIJVING, JAAR)%>%
  summarise(Total=sum(AANTAL_PAT_PER_ZPD))%>%
  mutate(aantal_pat_scaled=as.numeric(scale(Total)))%>%
  ggplot()+
  geom_tile(aes(y=DIAGNOSE_OMSCHRIJVING, 
                fill=aantal_pat_scaled, 
                x=factor(JAAR)))+
  theme_bw()+
  scale_fill_viridis_c(option="turbo")+
  labs(fill="Aantal (geschaald)",
       y="Diagnose omschrijving",
       x="Jaar", 
       title="Aantal patienten per jaar per zorgproduct", 
       subtitle="Gezocht op 'kanker' en 'carcinoom' en 'tumor'",
       caption="Source: OpenDIS")

combined_DBC%>%
  filter(grepl('Mamma tumoren|mamma tumoren',DIAGNOSE_OMSCHRIJVING))%>%
  filter(JAAR<2023)%>%
  group_by(ZORGPRODUCT_CD, CONSUMENT_OMS, JAAR)%>%
  summarise(Total=sum(AANTAL_PAT_PER_ZPD))%>%
  mutate(aantal_pat_scaled=as.numeric(scale(Total)))%>%
  ggplot()+
  geom_tile(aes(y=CONSUMENT_OMS, 
                fill=aantal_pat_scaled, 
                x=factor(JAAR)))+
  theme_bw()+
  scale_fill_viridis_c(option="turbo")+
  labs(fill="Aantal (geschaald)",
       y="Consument omschrijving",
       x="Jaar", 
       title="Aantal patienten per jaar per zorgproduct", 
       subtitle="Gezocht op 'mamma tumoren'",
       caption="Source: OpenDIS")

combined_DBC%>%
  filter(grepl('tumoren',DIAGNOSE_OMSCHRIJVING))%>%
  filter(JAAR<2023)%>%
  group_by(DIAGNOSE_OMSCHRIJVING, JAAR)%>%
  summarise(Total=sum(AANTAL_PAT_PER_ZPD))%>%
  mutate(aantal_pat_scaled=as.numeric(scale(Total)))%>%
  ggplot()+
  geom_tile(aes(y=DIAGNOSE_OMSCHRIJVING, 
                fill=aantal_pat_scaled, 
                x=factor(JAAR)))+
  theme_bw()+
  scale_fill_viridis_c(option="turbo")+
  labs(fill="Aantal (geschaald)",
       y="Diagnose omschrijving",
       x="Jaar", 
       title="Aantal patienten per jaar per zorgproduct", 
       subtitle="Gezocht op 'tumoren'",
       caption="Source: OpenDIS")


combined_DBC%>%
  filter(grepl('tumoren',DIAGNOSE_OMSCHRIJVING))%>%
  group_by(DIAGNOSE_OMSCHRIJVING)%>%
  summarise(Total=sum(AANTAL_PAT_PER_ZPD))%>%
  ggplot()+
  geom_bar(aes(y=DIAGNOSE_OMSCHRIJVING, 
               x=Total), stat="identity")+
  theme_bw()+
  labs(x="Aantal patienten per porgproduct", 
       y="Diagnose omschrijving", 
       title="Aantal patienten per zorgproduct en diagnose omschrijving", 
       subtitle="Gezocht op 'tumoren'",
       caption="Source: OpenDIS")

combined_DBC%>%
  filter(grepl('screening',DIAGNOSE_OMSCHRIJVING))%>%
  group_by(DIAGNOSE_OMSCHRIJVING)%>%
  summarise(Total=sum(AANTAL_PAT_PER_ZPD))%>%
  ggplot()+
  geom_bar(aes(y=DIAGNOSE_OMSCHRIJVING, 
               x=Total), stat="identity")+
  theme_bw()+
  labs(x="Aantal patienten per porgproduct", 
       y="Diagnose omschrijving", 
       title="Aantal patienten per zorgproduct en diagnose omschrijving", 
       subtitle="Gezocht op 'screening'",
       caption="Source: OpenDIS")

combined_ZAT%>%
  filter(grepl('screening',OMSCHRIJVING))%>%
  group_by(OMSCHRIJVING)%>%
  summarise(Total=sum(AANTAL_PAT))%>%
  ggplot()+
  geom_bar(aes(y=OMSCHRIJVING, 
               x=Total), stat="identity")+
  theme_bw()

combined_DBC%>%
  filter(ZORGPRODUCT_CD%in%c("028899046", "028899048"))%>%
  select(JAAR, ZORGPRODUCT_CD, DIAGNOSE_OMSCHRIJVING, CONSUMENT_OMS, 
         OMSCHRIJVING_SPEC,AANTAL_PAT_PER_ZPD,AANTAL_PAT_PER_DIAG,AANTAL_PAT_PER_SPC)%>%
  ggplot()+
  geom_bar(aes(x=factor(JAAR), 
               y=AANTAL_PAT_PER_DIAG, 
               fill=CONSUMENT_OMS), stat="identity")+
  facet_grid(OMSCHRIJVING_SPEC~"", scales = "free")+
  theme_bw()+
  theme(legend.position ="bottom")+
  labs(y="Aantal patienten per diagnose", 
       x="Jaar", 
       fill="Omschrijving",
       title="Aantal patienten per jaar per specialisatie voor screening op kanker van dikke darm of endeldarm", 
       subtitle="Gezocht op zorgproducten 028899046 en 028899048",
       caption="Source: OpenDIS")

combined_DBC%>%
  filter(ZORGPRODUCT_CD%in%c("219899014"))%>%
  select(JAAR, ZORGPRODUCT_CD, DIAGNOSE_OMSCHRIJVING, CONSUMENT_OMS, 
         OMSCHRIJVING_SPEC,AANTAL_PAT_PER_ZPD,AANTAL_PAT_PER_DIAG,AANTAL_PAT_PER_SPC)%>%
  ggplot()+
  geom_bar(aes(x=factor(JAAR), 
               y=AANTAL_PAT_PER_DIAG), stat="identity")+
  facet_grid(~OMSCHRIJVING_SPEC, scales = "free")+
  theme_bw()+
  theme(legend.position ="bottom")+
  labs(y="Aantal patienten per diagnose", 
       x="Jaar", 
       title="Aantal patienten per jaar per specialisatie voor screening op erfelijke vormen van kanker", 
       subtitle="Gezocht op zorgproduct 219899014",
       caption="Source: OpenDIS")



combined_DBC%>%
  filter(grepl('borstkanker|Borstkanker',CONSUMENT_OMS))%>%
  select(DIAGNOSE_OMSCHRIJVING, JAAR, AANTAL_PAT_PER_ZPD, CONSUMENT_OMS, OMSCHRIJVING_SPEC)%>%
  mutate(CONSUMENT_OMS = tm::removeWords(CONSUMENT_OMS,c("bij Borstkanker", "bij borstkanker")))%>%
  filter(JAAR<2023)%>%
  ggplot()+
  geom_tile(aes(x=factor(JAAR), y=CONSUMENT_OMS, fill=log(AANTAL_PAT_PER_ZPD)))+
  theme_bw()+
  facet_grid(~OMSCHRIJVING_SPEC)+
  labs(y="Zorg omschrijving", 
       fill="Aantal patienten (log)",
       x="Jaar",
       title="Aantal patienten per zorgproduct voor borstkanker")+
  theme(legend.position="bottom")+
  scale_fill_viridis_c(option="turbo")


combined_DBC%>%
  filter(SPECIALISME_CD=="0361")%>%
  filter(DIAGNOSE_OMSCHRIJVING=="Mamma tumoren")%>%
  mutate(CONSUMENT_OMS = tm::removeWords(CONSUMENT_OMS,c("bij Borstkanker", "bij borstkanker")))%>%
  group_by(CONSUMENT_OMS, OMSCHRIJVING_SPEC, JAAR)%>%
  summarise(Total=sum(AANTAL_PAT_PER_ZPD))%>%
  filter(JAAR<2023)%>%
  ggplot()+
  geom_tile(aes(y=CONSUMENT_OMS, x=factor(JAAR), fill=Total))+
  theme_bw()+
  facet_grid(""~OMSCHRIJVING_SPEC)+
  scale_fill_viridis_c(option="turbo")+
  theme(legend.position = "bottom")+
  labs(y="Diagnose omschrijving", 
       fill="Aantal",
       title="Aantal patienten per diagnose voor borstkanker", 
       x="Jaar")

combined_DBC%>%
  filter(grepl('mamma|Mamma',DIAGNOSE_OMSCHRIJVING))%>%
  filter(grepl('Maligniteit|tumoren',DIAGNOSE_OMSCHRIJVING))%>%
  filter(JAAR<2023)%>%
  group_by(CONSUMENT_OMS)%>%
  mutate(aantal_pat_scaled=as.numeric(scale(AANTAL_PAT_PER_ZPD)))%>%
  select(JAAR,CONSUMENT_OMS,aantal_pat_scaled)%>%
  mutate(CONSUMENT_OMS = tm::removeWords(CONSUMENT_OMS,c("bij Borstkanker", "bij borstkanker")))%>%
  ggplot()+
  geom_tile(aes(x=factor(JAAR), y=CONSUMENT_OMS, fill=aantal_pat_scaled))+
  theme_bw()+
  scale_fill_viridis_c(option="turbo")+
  labs(y="Zorgproduct", 
       x="JAAR",
       title="Aantal patienten per zorgproduct voor borstkanker", 
       fill="Aantal patienten per zorgproduct (genormaliseerd)")+
  theme(legend.position="bottom")


combined_DBC%>%
  filter(grepl('mamma|Mamma',DIAGNOSE_OMSCHRIJVING))%>%
  filter(grepl('Maligniteit|tumoren',DIAGNOSE_OMSCHRIJVING))%>%
  group_by(CONSUMENT_OMS)%>%
  mutate(aantal_pat_scaled=as.numeric(scale(AANTAL_PAT_PER_ZPD)))%>%
  select(JAAR,CONSUMENT_OMS,aantal_pat_scaled, AANTAL_PAT_PER_ZPD)%>%
  mutate(CONSUMENT_OMS = tm::removeWords(CONSUMENT_OMS,c("bij Borstkanker", "bij borstkanker")))%>%
  group_by(CONSUMENT_OMS)%>%
  mutate(rank = row_number(desc(aantal_pat_scaled)))%>%
  ggplot()+
  geom_tile(aes(y=CONSUMENT_OMS, x=factor(JAAR), fill=factor(rank)))+
  geom_text(aes(y=CONSUMENT_OMS, x=factor(JAAR), label=rank), col="black")+
  theme_bw()+
  labs(y="Zorgproduct", 
       col="Rangschikking",
       title="Aantal patienten per zorgproduct voor borstkanker", 
       x="Aantal patienten per zorgproduct (genormaliseerd)")

combined_DBC%>%
  filter(grepl('borstkanker|Borstkanker',CONSUMENT_OMS))%>%
  select(DIAGNOSE_OMSCHRIJVING, JAAR, AANTAL_PAT_PER_DIAG, CONSUMENT_OMS, OMSCHRIJVING_SPEC)%>%
  mutate(Period = if_else(JAAR<2020, "Pre-Covid", "Covid"))%>%
  filter(JAAR<2023)%>%
  ggplot()+
  geom_bar(aes(x=factor(JAAR), y=AANTAL_PAT_PER_DIAG, fill=Period), 
           stat="identity", position="dodge")+
  facet_grid(~OMSCHRIJVING_SPEC, scale="free")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(col="Specialism", 
       y="Number of DBCs",
       title="Number of DBCs for breast cancer",
       fill="Period",
       subtitle="Per specialism and excluding 2023", 
       caption="Source: OpenDIS",
       x="Year")


combined_DBC%>%
  mutate(ZORGPRODUCT_CD = as.numeric(ZORGPRODUCT_CD))%>%
  filter(ZORGPRODUCT_CD%in%c(020107002:020107057))%>%
  filter(JAAR<2023)%>%
  filter(DIAGNOSE_OMSCHRIJVING=="Maligniteit mamma")%>%
  select(JAAR, ZORGPRODUCT_CD, DIAGNOSE_OMSCHRIJVING, CONSUMENT_OMS, 
         OMSCHRIJVING_SPEC,AANTAL_PAT_PER_ZPD,AANTAL_PAT_PER_DIAG,AANTAL_PAT_PER_SPC)%>%
  group_by(CONSUMENT_OMS,OMSCHRIJVING_SPEC,JAAR)%>%
  summarise(Total=sum(AANTAL_PAT_PER_ZPD))%>%
  mutate(aantal_pat_scaled=as.numeric(scale(Total)))%>%
  ggplot()+
  geom_tile(aes(x=factor(JAAR), 
               fill=aantal_pat_scaled, 
               y=CONSUMENT_OMS), stat="identity")+
  facet_grid(~OMSCHRIJVING_SPEC, scales = "free")+
  theme_bw()+
  scale_fill_viridis_c(option="turbo")+
  theme(legend.position ="bottom")+
  labs(fill="Aantal patienten", 
       x="Jaar", 
       y="Consument omschrijving",
       title="Aantal patienten per jaar per specialisatie per zorgproduct voor borstkanker", 
       subtitle="Gezocht op zorgproducten 020107002 tot 020107057",
       caption="Source: OpenDIS")

combined_DBC%>%
  mutate(ZORGPRODUCT_CD = as.numeric(ZORGPRODUCT_CD))%>%
  filter(ZORGPRODUCT_CD%in%c(020107002:020107057))%>%
  filter(JAAR<2023)%>%
  filter(DIAGNOSE_OMSCHRIJVING=="Maligniteit mamma")%>%
  filter(grepl('uitzaaiingen',CONSUMENT_OMS))%>%
  select(JAAR, ZORGPRODUCT_CD, DIAGNOSE_OMSCHRIJVING, CONSUMENT_OMS, 
         OMSCHRIJVING_SPEC,AANTAL_PAT_PER_ZPD,AANTAL_PAT_PER_DIAG,AANTAL_PAT_PER_SPC)%>%
  group_by(CONSUMENT_OMS,OMSCHRIJVING_SPEC,JAAR)%>%
  summarise(Total=sum(AANTAL_PAT_PER_ZPD))%>%
  ggplot()+
  geom_tile(aes(x=factor(JAAR), 
                fill=Total, 
                y=CONSUMENT_OMS), stat="identity")+
  facet_grid(~OMSCHRIJVING_SPEC, scales = "free")+
  theme_bw()+
  scale_fill_viridis_c(option="turbo")+
  theme(legend.position ="bottom")+
  labs(fill="Aantal patienten", 
       x="Jaar", 
       y="Consument omschrijving",
       title="Aantal patienten per jaar per specialisatie per zorgproduct voor borstkanker", 
       subtitle="Gezocht op zorgproducten 020107002:020107057 en 'uitzaaiingen'",
       caption="Source: OpenDIS")

combined_DBC%>%
  mutate(ZORGPRODUCT_CD = as.numeric(ZORGPRODUCT_CD))%>%
  filter(ZORGPRODUCT_CD%in%c(020107002:020107057))%>%
  filter(JAAR<2023)%>%
  filter(DIAGNOSE_OMSCHRIJVING=="Maligniteit mamma")%>%
  filter(grepl('uitzaaiingen',CONSUMENT_OMS))%>%
  select(JAAR, ZORGPRODUCT_CD, DIAGNOSE_OMSCHRIJVING, CONSUMENT_OMS, 
         OMSCHRIJVING_SPEC,AANTAL_PAT_PER_ZPD,AANTAL_PAT_PER_DIAG,AANTAL_PAT_PER_SPC)%>%
  group_by(CONSUMENT_OMS,OMSCHRIJVING_SPEC,JAAR)%>%
  summarise(Total=sum(AANTAL_PAT_PER_ZPD))%>%
  group_by(CONSUMENT_OMS)%>%
  mutate(rank = row_number(desc(Total)))%>%
  ggplot()+
  geom_tile(aes(y=CONSUMENT_OMS, x=factor(JAAR), fill=Total))+
  geom_text(aes(y=CONSUMENT_OMS, x=factor(JAAR), label=rank), col="black")+
  facet_grid(~OMSCHRIJVING_SPEC, scales = "free")+
  scale_fill_viridis_c(option="turbo")+
  theme_bw()+
  theme(legend.position ="bottom")+
  labs(fill="Aantal patienten", 
       x="Jaar", 
       y="Consument omschrijving",
       title="Aantal patienten per jaar per specialisatie per zorgproduct voor borstkanker", 
       subtitle="Gezocht op zorgproducten 020107002:020107057 en 'uitzaaiingen'",
       caption="Source: OpenDIS")

combined_DBC%>%
  filter(grepl('mamma|Mamma',DIAGNOSE_OMSCHRIJVING))%>%
  filter(grepl('Maligniteit|tumoren',DIAGNOSE_OMSCHRIJVING))%>%
  group_by(CONSUMENT_OMS)%>%
  mutate(aantal_pat_scaled=as.numeric(scale(AANTAL_PAT_PER_ZPD)))%>%
  select(JAAR,CONSUMENT_OMS,aantal_pat_scaled, AANTAL_PAT_PER_ZPD)%>%
  mutate(CONSUMENT_OMS = tm::removeWords(CONSUMENT_OMS,c("bij Borstkanker", "bij borstkanker")))%>%
  group_by(CONSUMENT_OMS)%>%
  mutate(rank = row_number(desc(aantal_pat_scaled)))%>%
  ggplot()+
  geom_tile(aes(y=CONSUMENT_OMS, x=factor(JAAR), fill=factor(rank)))+
  geom_text(aes(y=CONSUMENT_OMS, x=factor(JAAR), label=rank), col="black")+
  theme_bw()+
  labs(y="Zorgproduct", 
       col="Rangschikking",
       title="Aantal patienten per zorgproduct voor borstkanker", 
       x="Aantal patienten per zorgproduct (genormaliseerd)")

combined_DBC%>%
  filter(grepl('kanker van dikke darm',CONSUMENT_OMS))%>%
  select(DIAGNOSE_OMSCHRIJVING, JAAR, AANTAL_PAT_PER_DIAG, CONSUMENT_OMS, OMSCHRIJVING_SPEC)%>%
  mutate(Period = if_else(JAAR<2020, "Pre-Covid", "Covid"))%>%
  filter(JAAR<2023)%>%
  ggplot()+
  geom_bar(aes(x=factor(JAAR), y=AANTAL_PAT_PER_DIAG, fill=Period), 
           stat="identity", position="dodge")+
  facet_grid(~OMSCHRIJVING_SPEC, scale="free")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(col="Specialism", 
       y="Number of DBCs",
       title="Number of DBCs for colon cancer",
       fill="Period",
       subtitle="Per specialism and excluding 2023", 
       caption="Source: OpenDIS",
       x="Year")

combined_DBC%>%
  mutate(ZORGPRODUCT_CD = as.numeric(ZORGPRODUCT_CD))%>%
  filter(ZORGPRODUCT_CD%in%c(029199003:029199106,029199269:029199272,029199286))%>%
  filter(JAAR<2023)%>%
  filter(grepl('Colorectale maligniteit|Maligniteit colorectaal',DIAGNOSE_OMSCHRIJVING))%>%
  select(JAAR, ZORGPRODUCT_CD, DIAGNOSE_OMSCHRIJVING, CONSUMENT_OMS, 
         OMSCHRIJVING_SPEC,AANTAL_PAT_PER_ZPD,AANTAL_PAT_PER_DIAG,AANTAL_PAT_PER_SPC)%>%
  mutate(CONSUMENT_OMS = tm::removeWords(CONSUMENT_OMS,c("bij kanker van dikke darm of endeldarm", 
                                                         "bij Dikkedarm-/endeldarmkanker")))%>%
  group_by(CONSUMENT_OMS,OMSCHRIJVING_SPEC,JAAR)%>%
  summarise(Total=sum(AANTAL_PAT_PER_ZPD))%>%
  mutate(aantal_pat_scaled=as.numeric(scale(Total)))%>%
  ggplot()+
  geom_tile(aes(x=factor(JAAR), 
                fill=aantal_pat_scaled, 
                y=CONSUMENT_OMS), stat="identity")+
  facet_grid(~OMSCHRIJVING_SPEC, scales = "free")+
  theme_bw()+
  scale_fill_viridis_c(option="turbo")+
  theme(legend.position ="bottom")+
  labs(fill="Aantal patienten", 
       x="Jaar", 
       y="Consument omschrijving",
       title="Aantal patienten per jaar per specialisatie per zorgproduct voor dikke darm kanker", 
       subtitle="Gezocht op zorgproducten 029199003 tot 029199106 en 029199269 tot 029199272 en 029199286",
       caption="Source: OpenDIS")

combined_DBC%>%
  mutate(ZORGPRODUCT_CD = as.numeric(ZORGPRODUCT_CD))%>%
  filter(ZORGPRODUCT_CD%in%c(029199003:029199106,029199269:029199272,029199286))%>%
  filter(JAAR<2023)%>%
  filter(grepl('Colorectale maligniteit|Maligniteit colorectaal',DIAGNOSE_OMSCHRIJVING))%>%
  select(JAAR, ZORGPRODUCT_CD, DIAGNOSE_OMSCHRIJVING, CONSUMENT_OMS, 
         OMSCHRIJVING_SPEC,AANTAL_PAT_PER_ZPD,AANTAL_PAT_PER_DIAG,AANTAL_PAT_PER_SPC)%>%
  mutate(CONSUMENT_OMS = tm::removeWords(CONSUMENT_OMS,c("bij kanker van dikke darm of endeldarm", 
                                                         "bij Dikkedarm-/endeldarmkanker")))%>%
  group_by(CONSUMENT_OMS,OMSCHRIJVING_SPEC,JAAR)%>%
  summarise(Total=sum(AANTAL_PAT_PER_ZPD))%>%
  mutate(rank = row_number(desc(Total)))%>%
  ggplot()+
  geom_tile(aes(y=CONSUMENT_OMS, x=factor(JAAR), fill=Total))+
  geom_text(aes(y=CONSUMENT_OMS, x=factor(JAAR), label=rank), col="black")+
  facet_grid(~OMSCHRIJVING_SPEC, scales = "free")+
  theme_bw()+
  scale_fill_viridis_c(option="turbo")+
  theme(legend.position ="bottom")+
  labs(fill="Aantal patienten", 
       x="Jaar", 
       y="Consument omschrijving",
       title="Aantal patienten per jaar per specialisatie per zorgproduct voor dikke darm kanker", 
       subtitle="Gezocht op zorgproducten 029199003 tot 029199106 en 029199269 tot 029199272 en 029199286",
       caption="Source: OpenDIS")


combined_DBC%>%
  mutate(ZORGPRODUCT_CD = as.numeric(ZORGPRODUCT_CD))%>%
  filter(ZORGPRODUCT_CD%in%c(029199003:029199106,029199269:029199272,029199286))%>%
  filter(JAAR<2023)%>%
  filter(grepl('Colorectale maligniteit|Maligniteit colorectaal',DIAGNOSE_OMSCHRIJVING))%>%
  filter(grepl('uitzaaiingen',CONSUMENT_OMS))%>%
  select(JAAR, ZORGPRODUCT_CD, DIAGNOSE_OMSCHRIJVING, CONSUMENT_OMS, 
         OMSCHRIJVING_SPEC,AANTAL_PAT_PER_ZPD,AANTAL_PAT_PER_DIAG,AANTAL_PAT_PER_SPC)%>%
  group_by(CONSUMENT_OMS,OMSCHRIJVING_SPEC,JAAR)%>%
  summarise(Total=sum(AANTAL_PAT_PER_ZPD))%>%
  ggplot()+
  geom_tile(aes(x=factor(JAAR), 
                fill=Total, 
                y=CONSUMENT_OMS), stat="identity")+
  facet_grid(~OMSCHRIJVING_SPEC, scales = "free")+
  theme_bw()+
  scale_fill_viridis_c(option="turbo")+
  theme(legend.position ="bottom")+
  labs(fill="Aantal patienten", 
       x="Jaar", 
       y="Consument omschrijving",
       title="Aantal patienten per jaar per specialisatie per zorgproduct voor dikke darm kanker", 
       subtitle="Gezocht op zorgproducten 029199003:029199106, 029199269:029199272, 029199286 en 'uitzaaiingen'",
       caption="Source: OpenDIS")

combined_DBC%>%
  mutate(ZORGPRODUCT_CD = as.numeric(ZORGPRODUCT_CD))%>%
  filter(ZORGPRODUCT_CD%in%c(029199003:029199106,029199269:029199272,029199286))%>%
  filter(JAAR<2023)%>%
  filter(grepl('Colorectale maligniteit|Maligniteit colorectaal',DIAGNOSE_OMSCHRIJVING))%>%
  filter(grepl('uitzaaiingen',CONSUMENT_OMS))%>%
  select(JAAR, ZORGPRODUCT_CD, DIAGNOSE_OMSCHRIJVING, CONSUMENT_OMS, 
         OMSCHRIJVING_SPEC,AANTAL_PAT_PER_ZPD,AANTAL_PAT_PER_DIAG,AANTAL_PAT_PER_SPC)%>%
  group_by(CONSUMENT_OMS,OMSCHRIJVING_SPEC,JAAR)%>%
  summarise(Total=sum(AANTAL_PAT_PER_ZPD))%>%
  group_by(CONSUMENT_OMS)%>%
  mutate(rank = row_number(desc(Total)))%>%
  ggplot()+
  geom_tile(aes(y=CONSUMENT_OMS, x=factor(JAAR), fill=Total))+
  geom_text(aes(y=CONSUMENT_OMS, x=factor(JAAR), label=rank), col="black")+
  facet_grid(~OMSCHRIJVING_SPEC, scales = "free")+
  scale_fill_viridis_c(option="turbo")+
  theme_bw()+
  theme(legend.position ="bottom")+
  labs(fill="Aantal patienten", 
       x="Jaar", 
       y="Consument omschrijving",
       title="Aantal patienten per jaar per specialisatie per zorgproduct voor dikke darm kanker", 
       subtitle="Gezocht op zorgproducten 029199003:029199106, 029199269:029199272, 029199286 en 'uitzaaiingen'",
       caption="Source: OpenDIS")

combined_DBC%>%
  mutate(ZORGPRODUCT_CD = as.numeric(ZORGPRODUCT_CD))%>%
  filter(ZORGPRODUCT_CD%in%c(020108044:020108090,020108233:020108236))%>%
  filter(JAAR<2023)%>%
  select(JAAR, ZORGPRODUCT_CD, DIAGNOSE_OMSCHRIJVING, CONSUMENT_OMS, 
         OMSCHRIJVING_SPEC,AANTAL_PAT_PER_ZPD,AANTAL_PAT_PER_DIAG,AANTAL_PAT_PER_SPC)%>%
  mutate(CONSUMENT_OMS = tm::removeWords(CONSUMENT_OMS,c("bij Baarmoederhalskanker", 
                                                         "bij baarmoederhalskanker")))%>%
  
  group_by(CONSUMENT_OMS,OMSCHRIJVING_SPEC,JAAR)%>%
  summarise(Total=sum(AANTAL_PAT_PER_ZPD))%>%
  mutate(aantal_pat_scaled=as.numeric(scale(Total)))%>%
  ggplot()+
  geom_tile(aes(x=factor(JAAR), 
                fill=aantal_pat_scaled, 
                y=CONSUMENT_OMS), stat="identity")+
  facet_grid(~OMSCHRIJVING_SPEC, scales = "free")+
  theme_bw()+
  scale_fill_viridis_c(option="turbo")+
  theme(legend.position ="bottom")+
  labs(fill="Aantal patienten", 
       x="Jaar", 
       y="Consument omschrijving",
       title="Aantal patienten per jaar per specialisatie per zorgproduct voor baarmoederhalskanker", 
       subtitle="Gezocht op zorgproducten 020108044 tot 020108090 en 020108233 tot 020108236",
       caption="Source: OpenDIS")


combined_DBC%>%
  mutate(ZORGPRODUCT_CD = as.numeric(ZORGPRODUCT_CD))%>%
  filter(ZORGPRODUCT_CD%in%c(020108044:020108090,020108233:020108236))%>%
  filter(JAAR<2023)%>%
  select(JAAR, ZORGPRODUCT_CD, DIAGNOSE_OMSCHRIJVING, CONSUMENT_OMS, 
         OMSCHRIJVING_SPEC,AANTAL_PAT_PER_ZPD,AANTAL_PAT_PER_DIAG,AANTAL_PAT_PER_SPC)%>%
  mutate(CONSUMENT_OMS = tm::removeWords(CONSUMENT_OMS,c("bij Baarmoederhalskanker", 
                                                         "bij baarmoederhalskanker")))%>%
  
  group_by(CONSUMENT_OMS,OMSCHRIJVING_SPEC,JAAR)%>%
  summarise(Total=sum(AANTAL_PAT_PER_ZPD))%>%
  mutate(rank = row_number(desc(Total)))%>%
  ggplot()+
  geom_tile(aes(y=CONSUMENT_OMS, x=factor(JAAR), fill=Total))+
  geom_text(aes(y=CONSUMENT_OMS, x=factor(JAAR), label=rank), col="white")+
  facet_grid(~OMSCHRIJVING_SPEC, scales = "free")+
  theme_bw()+
  scale_fill_viridis_c(option="turbo")+
  theme(legend.position ="bottom")+
  labs(fill="Aantal patienten", 
       x="Jaar", 
       y="Consument omschrijving",
       title="Aantal patienten per jaar per specialisatie per zorgproduct voor baarmoederhalskanker", 
       subtitle="Gezocht op zorgproducten 020108044 tot 020108090 en 020108233 tot 020108236",
       caption="Source: OpenDIS")


combined_DBC%>%
  mutate(ZORGPRODUCT_CD = as.numeric(ZORGPRODUCT_CD))%>%
  filter(ZORGPRODUCT_CD%in%c(020108044:020108090,020108233:020108236))%>%
  filter(JAAR<2023)%>%
  filter(grepl('uitzaaiingen',CONSUMENT_OMS))%>%
  select(JAAR, ZORGPRODUCT_CD, DIAGNOSE_OMSCHRIJVING, CONSUMENT_OMS, 
         OMSCHRIJVING_SPEC,AANTAL_PAT_PER_ZPD,AANTAL_PAT_PER_DIAG,AANTAL_PAT_PER_SPC)%>%
  mutate(CONSUMENT_OMS = tm::removeWords(CONSUMENT_OMS,c("bij Baarmoederhalskanker", 
                                                         "bij baarmoederhalskanker")))%>%
  
  group_by(CONSUMENT_OMS,OMSCHRIJVING_SPEC,JAAR)%>%
  summarise(Total=sum(AANTAL_PAT_PER_ZPD))%>%
  ggplot()+
  geom_tile(aes(x=factor(JAAR), 
                fill=Total, 
                y=CONSUMENT_OMS), stat="identity")+
  facet_grid(~OMSCHRIJVING_SPEC, scales = "free")+
  theme_bw()+
  scale_fill_viridis_c(option="turbo")+
  theme(legend.position ="bottom")+
  labs(fill="Aantal patienten", 
       x="Jaar", 
       y="Consument omschrijving",
       title="Aantal patienten per jaar per specialisatie per zorgproduct voor baarmoederhalskanker", 
       subtitle="Gezocht op zorgproducten 020108044:020108090, 020108233:020108236 en 'uitzaaiingen'",
       caption="Source: OpenDIS")

combined_DBC%>%
  mutate(ZORGPRODUCT_CD = as.numeric(ZORGPRODUCT_CD))%>%
  filter(ZORGPRODUCT_CD%in%c(020108044:020108090,020108233:020108236))%>%
  filter(JAAR<2023)%>%
  filter(grepl('uitzaaiingen',CONSUMENT_OMS))%>%
  select(JAAR, ZORGPRODUCT_CD, DIAGNOSE_OMSCHRIJVING, CONSUMENT_OMS, 
         OMSCHRIJVING_SPEC,AANTAL_PAT_PER_ZPD,AANTAL_PAT_PER_DIAG,AANTAL_PAT_PER_SPC)%>%
  group_by(CONSUMENT_OMS,OMSCHRIJVING_SPEC,JAAR)%>%
  summarise(Total=sum(AANTAL_PAT_PER_ZPD))%>%
  group_by(CONSUMENT_OMS)%>%
  mutate(rank = row_number(desc(Total)))%>%
  ggplot()+
  geom_tile(aes(y=CONSUMENT_OMS, x=factor(JAAR), fill=Total))+
  geom_text(aes(y=CONSUMENT_OMS, x=factor(JAAR), label=rank), col="black")+
  facet_grid(~OMSCHRIJVING_SPEC, scales = "free")+
  scale_fill_viridis_c(option="turbo")+
  theme_bw()+
  theme(legend.position ="bottom")+
  labs(fill="Aantal patienten", 
       x="Jaar", 
       y="Consument omschrijving",
       title="Aantal patienten per jaar per specialisatie per zorgproduct voor baarmoederhalskanker", 
       subtitle="Gezocht op zorgproducten 020108044:020108090, 020108233:020108236 en 'uitzaaiingen'",
       caption="Source: OpenDIS")







combined_DBC%>%
  filter(grepl('Darmkanker|darmkanker|kanker van dikke darm',CONSUMENT_OMS))%>%
  filter(JAAR<2023)%>%
  group_by(DIAGNOSE_OMSCHRIJVING,JAAR)%>%
  summarise(Total=sum(AANTAL_PAT_PER_ZPD))%>%
  ggplot()+
  geom_tile(aes(y=DIAGNOSE_OMSCHRIJVING, 
                fill=Total, 
                x=factor(JAAR)))+
  theme_bw()+
  scale_fill_viridis_c(option="turbo")+
  labs(y="Diagnose omschrijving", 
       fill="Aantal",
       title="Aantal patienten per diagnose voor darmkanker", 
       x="Jaar",
       subtitle="Gezocht op 'darmkanker' en 'kanker van dikke darm'",
       caption="Source: OpenDIS")

combined_DBC%>%
  filter(grepl('kanker van dikke darm|Dikkedarm-/endeldarmkanker|darmkanker|Darmkanker',CONSUMENT_OMS))%>%
  select(DIAGNOSE_OMSCHRIJVING, JAAR, AANTAL_PAT_PER_DIAG, OMSCHRIJVING_SPEC)%>%
  ggplot()+
  geom_bar(aes(x=factor(JAAR), 
               y=AANTAL_PAT_PER_DIAG, 
               fill=OMSCHRIJVING_SPEC), 
           stat="identity", 
           position="dodge")+
  facet_grid(DIAGNOSE_OMSCHRIJVING~"Jaar", scales="free")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(col="Diagnose omschrijving", 
       y="Aantal patienten per diagnose",
       title="Aantal patienten per diagnose voor darmkanker", 
       fill="Omschrijving specialisatie",
       subtitle="Gezocht op 'Dikkedarm-/endeldarmkanker', 'darmkanker' en 'kanker van dikke darm'",
       x="Jaar",
       caption="Source: OpenDIS")


combined_DBC%>%
  filter(grepl('baarmoederhalskanker|Baarmoederhalskanker',CONSUMENT_OMS))%>%
  select(DIAGNOSE_OMSCHRIJVING, JAAR, AANTAL_PAT_PER_DIAG, CONSUMENT_OMS, OMSCHRIJVING_SPEC)%>%
  mutate(Period = if_else(JAAR<2020, "Pre-Covid", "Covid"))%>%
  filter(JAAR<2023)%>%
  ggplot()+
  geom_bar(aes(x=factor(JAAR), y=AANTAL_PAT_PER_DIAG, fill=Period), 
           stat="identity", position="dodge")+
  facet_grid(~OMSCHRIJVING_SPEC, scale="free")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(col="Specialism", 
       y="Number of DBCs",
       title="Number of DBCs for cervical cancer",
       fill="Period",
       subtitle="Per specialism and excluding 2023", 
       caption="Source: OpenDIS",
       x="Year")























#### LOAD, WRANGLE AND EXPLORE IKNL DATA   - MEDIUM POST ----
library(readr)
library(skimr)
library(tidyverse)
library(DataExplorer)
library(mice)
library(VIM)
library(visdat)
library(naniar)
library(ggplot2)
library(survival)
library(survminer)
library(splines)
library(FactoMineR)
library(factoextra)
library(MASS)
library(ggfortify)
library(rpart)
library(ggridges)
library(car)
library(penalized)
library(ggRandomForests)
library(ranger)
library(rms)
library(glmnet)
library(dplyr)
library(ggplot2)
library(knitr)
library(ciTools)
library(here)
library(flexsurv)
library(muhaz)
library(data.table)
library(DynNom)
library(specr)
library(tweenr)
library(cowplot)
library(lme4)
#### IMPORT DATA & First Exploration 
## A lot of variables are inserted as numerical but actually are not ##
IKNL<- read_delim("Data/IKNL/Synthetisch/NKR_IKNL_breast_syntheticdata.csv", delim = ";", 
                  escape_double = FALSE, trim_ws = TRUE)
View(IKNL)
skim(IKNL)
dim(IKNL)
str(IKNL)
#### DATA WRANGLING 
IKNL$perclymf      <-IKNL$pos_lymf/IKNL$ond_lymf
IKNL$c_eq_p        <-as.factor(ifelse(IKNL$ct==IKNL$pt, "TRUE", "FALSE"))
IKNL$her2_stat     <-ifelse(IKNL$her2_stat==9, NA, IKNL$her2_stat)
IKNL$er_stat       <-ifelse(IKNL$er_stat==9, NA, IKNL$er_stat)
IKNL$pr_stat       <-ifelse(IKNL$pr_stat==9, NA, IKNL$pr_stat)
IKNL$her2_stat_fact<-ifelse(IKNL$her2_stat>0, 1, 0)
IKNL$er_stat_fact  <-ifelse(IKNL$er_stat>0, 1, 0)
IKNL$pr_stat_fact  <-ifelse(IKNL$pr_stat>0, 1, 0)
IKNL$gen_stat      <-IKNL$her2_stat_fact+IKNL$er_stat_fact+IKNL$pr_stat_fact
IKNL$tum_afm       <-as.numeric(IKNL$tum_afm)
## Transform into factors
names(IKNL)
col_names          <- names(IKNL[,c(4,8:24,27:31,33:46,49:52)])
IKNL[,col_names]   <- lapply(IKNL[,col_names] , factor)
sapply(IKNL, class)

DataExplorer::plot_missing(IKNL)
DataExplorer::plot_boxplot(IKNL, by="vit_stat")
DataExplorer::plot_boxplot(IKNL, by="gesl")
DataExplorer::plot_density(IKNL)
table(IKNL$her2_stat)
table(IKNL$er_stat)
table(IKNL$pr_stat)

vis_miss(IKNL[c(3:10)])
vis_miss(IKNL[c(11:20)])  
vis_miss(IKNL[c(20:30)])
vis_miss(IKNL[c(30:40)])  
vis_miss(IKNL[c(40:46,49:52)])
gg_miss_upset(IKNL)
n_var_miss(IKNL)
gg_miss_var(IKNL)
gg_miss_var(IKNL,show_pct = TRUE)
gg_miss_var(IKNL,show_pct = TRUE, facet=incjr)
gg_miss_var(IKNL,show_pct = TRUE, facet=vit_stat)
gg_miss_var(IKNL,show_pct = TRUE, facet=gedrag)
gg_miss_var(IKNL,show_pct = TRUE, facet=gesl)
gg_miss_var(IKNL,show_pct = TRUE, facet=later)
gg_miss_var(IKNL,show_pct = TRUE, facet=her2_stat)
gg_miss_case(IKNL,show_pct = TRUE, facet=vit_stat)
gg_miss_case(IKNL,show_pct = TRUE, facet=gedrag)
gg_miss_case(IKNL,show_pct = TRUE, facet=gesl)
gg_miss_case(IKNL,show_pct = TRUE, facet=later)
gg_miss_case(IKNL,show_pct = TRUE, facet=her2_stat)
gg_miss_fct(IKNL,fct=  vit_stat)
gg_miss_fct(IKNL,fct = gedrag)
gg_miss_fct(IKNL,fct = her2_stat)
gg_miss_fct(IKNL,fct = gen_stat)

IKNLsub<-IKNL%>%dplyr::select(leeft,vit_stat_int,ond_lymf,pos_lymf,tum_afm)%>%
  filter(complete.cases(.))
IKNLsubrand<-IKNLsub[sample(nrow(IKNLsub), 5000), ]
### Graphical exploration 
ggplot(data=data.frame(IKNLsub), 
       aes(x=IKNLsub$leeft, y=IKNLsub$vit_stat_int, 
           col=as.factor(IKNLsub$vit_stat), size=as.factor(IKNLsub$gedrag))) + 
  geom_point(alpha=0.5) + theme_bw()
ggplot(data=data.frame(IKNLsubrand), 
       aes(x=leeft, y=vit_stat_int, 
           size=as.factor(vit_stat), color=as.factor(stadium))) + 
  geom_point(alpha=0.5) + theme_bw()

ggplot(data=data.frame(IKNLsubrand), 
       aes(x=leeft, y=vit_stat_int, 
           size=tum_afm, color=pos_lymf)) + 
  geom_point(alpha=0.4) + theme_bw() +  
  scale_color_gradient(low = 'blue', high = 'red')

ggplot(data=data.frame(IKNLsubrand), 
       aes(x=leeft, y=vit_stat_int, 
           size=ond_lymf, color=pos_lymf)) + 
  geom_point(alpha=0.4) + theme_bw() +  
  scale_color_gradient(low = 'blue', high = 'red')

ggplot(IKNL, aes(x=as.factor(incjr))) + geom_bar() + theme_bw()
ggplot(IKNL, aes(x=as.factor(c_eq_p))) + geom_bar() + theme_bw()
ggplot(IKNL, aes(x=as.factor(c_eq_p))) + geom_bar() + facet_wrap(~as.factor(pt))+theme_bw()

ggplot(IKNL, aes(x=perclymf)) + geom_density() + theme_bw()
ggplot(IKNL, aes(x=pos_lymf)) + geom_density() + theme_bw()

ggplot(IKNL, aes(tumsoort, vit_stat_int, fill=tumsoort)) + 
  geom_violin() + facet_wrap(~gedrag + gen_stat)+ theme_bw()

ggplot(IKNL,aes(as.factor(incjr), vit_stat_int, fill=as.factor(incjr))) + 
  geom_violin() + facet_grid(~tumsoort)+theme_bw()

ggplot(IKNL,aes(vit_stat_int, as.factor(incjr), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Survival Time", option = "C") +
  labs(title = 'Survival Time by Inclusion Year') + theme_bw()

ggplot(IKNL, aes(leeft,fill=gesl)) +  geom_density(alpha=0.5) + 
  facet_wrap(~as.factor(vit_stat)) +  ggtitle("Death or Alive by Age and Sex")+theme_bw()

ggplot(IKNL, aes(as.factor(incjr), fill=vit_stat)) + 
  geom_bar()+facet_wrap(~as.factor(gesl))+theme_bw()

ggplot(IKNL, aes(as.factor(incjr), fill=vit_stat)) + 
  geom_bar()+facet_wrap(~as.factor(okd))+theme_bw()

ggplot(IKNL, aes(vit_stat_int)) + geom_density(color="darkblue", fill="lightblue") + theme_bw()

ggplot(IKNL,aes(vit_stat_int, fill=as.factor(incjr))) + 
  geom_density(alpha=0.5)+facet_wrap(~incjr)+
  ggtitle("Survival Time by Inclusion Year")+  xlab("Survival Time") + theme_bw()

ggplot(IKNL,aes(vit_stat_int, fill=gedrag)) + 
  geom_density(alpha=0.5)+theme_bw()

ggplot(IKNL,aes(vit_stat_int, fill=tumsoort)) + 
  geom_density(alpha=0.5)+
  ggtitle("Survival Time by Tumour")+  xlab("Survival Time") + theme_bw()

ggplot(IKNL,aes(vit_stat_int, fill=later)) +  geom_density(alpha=0.5)+theme_bw()

ggplot(IKNL, aes(vit_stat_int, fill=diffgrad)) + geom_density(alpha=0.5)+theme_bw()

ggplot(IKNL, aes(vit_stat_int, fill=morf)) +  geom_density(alpha=0.5)+theme_bw()

ggplot(IKNL,aes(vit_stat_int, fill=stadium)) + geom_density(alpha=0.5)+theme_bw()

ggplot(IKNL,aes(vit_stat_int, fill=cstadium)) + geom_density(alpha=0.5)+
  ggtitle("Survival Time by Stage")+  xlab("Survival Time") + theme_bw()

ggplot(IKNL,aes(vit_stat_int, fill=pstadium)) + geom_density(alpha=0.5)+theme_bw()

ggplot(IKNL,aes(vit_stat_int, fill=er_stat)) + geom_density(alpha=0.5)+
  ggtitle("Survival Time by ER positive")+  xlab("Survival Time") + theme_bw()

ggplot(IKNL,aes(vit_stat_int, fill=pr_stat)) + geom_density(alpha=0.5)+
  ggtitle("Survival Time by PR positive")+  xlab("Survival Time") + theme_bw()

ggplot(IKNL, aes(vit_stat_int, fill=her2_stat)) + geom_density(alpha=0.5)+
  ggtitle("Survival Time by HER2 positive")+  xlab("Survival Time") + theme_bw()

ggplot(IKNL, aes(vit_stat_int, fill=gen_stat)) + geom_density(alpha=0.5)+
  ggtitle("Survival Time by Positive (ER / PR / HER2)")+  xlab("Survival Time") + theme_bw()

ggplot(IKNL, aes(vit_stat_int, fill=mari_uitslag)) + geom_density(alpha=0.5)+
  ggtitle("Survival Time by Mari Result")+  xlab("Survival Time") + theme_bw()

ggplot(IKNL,aes(vit_stat_int, fill=as.factor(org_chir))) + 
  geom_density(alpha=0.5)+theme_bw()

ggplot(IKNL,aes(vit_stat_int, fill=as.factor(meta_chir))) + 
  geom_density(alpha=0.5)+theme_bw()

ggplot(IKNL,aes(vit_stat_int, fill=as.factor(okd))) + 
  geom_density(alpha=0.5)+theme_bw()

ggplot(IKNL, aes(vit_stat_int, fill=her2_stat_fact )) + 
  geom_density(alpha=0.5)+theme_bw()

ggplot(IKNL, aes(vit_stat_int, fill=er_stat_fact )) + 
  geom_density(alpha=0.5)+theme_bw()

ggplot(IKNL, aes(vit_stat_int, fill=pr_stat_fact )) + 
  geom_density(alpha=0.5)+theme_bw()

ggplot(IKNL, aes(vit_stat_int, fill=gen_stat)) + 
  geom_density(alpha=0.5)+theme_bw()

ggplot(IKNL, aes(leeft,y=vit_stat_int, color=gen_stat)) + 
  geom_point(alpha=0.5)+theme_bw()

ggplot(IKNL, aes(leeft,vit_stat_int, color=gen_stat, fill=gen_stat)) + 
  geom_smooth(alpha=0.5)+facet_wrap(~incjr)+
  ggtitle("Survival time by age, hormonal results and inclusion year")+  
  xlab("Age") + ylab("Survival Time") + theme_bw()

ggplot(IKNL, aes(leeft,vit_stat_int,color=stadium, fill=stadium)) + 
  geom_smooth(alpha=0.5)+ facet_wrap(~incjr)+
  ggtitle("Survival time by age, cancer stage and inclusion year")+  
  xlab("Age") + ylab("Survival Time") + theme_bw()

ggplot(IKNL, aes(leeft,vit_stat_int,color=tumsoort,fill=tumsoort)) + 
  geom_smooth(alpha=0.5)+ facet_wrap(~stadium)+
  ggtitle("Survival time by age, tumor, and cancer stage")+  
  xlab("Age") + ylab("Survival Time") + theme_bw()

ggplot(IKNL, aes(x=leeft,y=vit_stat_int, 
                 color=tumsoort, 
                 fill=tumsoort)) + 
  geom_smooth(alpha=0.5)+ facet_wrap(~gedrag)+
  ggtitle("Survival time by age, tumor and placement, and cancer stage")+  
  xlab("Age") + ylab("Survival Time") + theme_bw()

ggplot(IKNL, aes(x=leeft,y=vit_stat_int,color=tumsoort,fill=tumsoort)) + 
  geom_smooth(alpha=0.5)+
  facet_wrap(~swk)+
  ggtitle("Survival time by age, tumor and placement, and sentinel node")+  
  xlab("Age") + ylab("Survival Time") + theme_bw()

ggplot(IKNL, aes(x=leeft,y=vit_stat_int, color=tumsoort, fill=tumsoort)) + 
  geom_smooth(alpha=0.5)+
  facet_wrap(~diffgrad)+
  ggtitle("Survival time by age, tumor and placement, and differential grade")+  
  xlab("Age") + ylab("Survival Time") + theme_bw()

##  Overall survival estimate
f1 <- survfit(Surv(vit_stat_int,vit_stat) ~ 1, 
              data = IKNL)
ggsurvplot(f1, data = IKNL,
           conf.int = TRUE,
           risk.table = TRUE,
           xlab = "Days", 
           ylab = "Overall survival probability",
           ggtheme = theme_bw())

## Survival by year
sd<-survdiff(Surv(vit_stat_int, 
                  vit_stat) ~ incjr, data = IKNL)
1 - pchisq(sd$chisq, length(sd$n) - 1)

ggsurv<-ggsurvplot(
  fit =  survfit(Surv(vit_stat_int, 
                      vit_stat) ~ incjr, data = IKNL),
  pval = TRUE,
  pval.method = TRUE,
  surv.median.line = "hv",
  conf.int = TRUE,
  risk.table = TRUE,
  risk.table.col="strata",
  xlab = "Days", 
  ylab = "Survival Probability by Days")
curv_facet <- ggsurv$plot + facet_wrap(~incjr);curv_facet

ggsurv<-ggsurvplot(
  fit =  survfit(Surv(vit_stat_int, 
                      vit_stat) ~ tumsoort, data = IKNL),
  pval = TRUE,
  pval.method = TRUE,
  surv.median.line = "hv",
  conf.int = TRUE,
  risk.table = TRUE,
  risk.table.col="strata",
  xlab = "Days", 
  ylab = "Survival Probability by Days")
curv_facet <- ggsurv$plot + facet_wrap(~tumsoort);curv_facet

fit <- survfit(Surv(vit_stat_int,vit_stat) ~ tumsoort, data = IKNL)
autoplot(fit)
autoplot(fit, surv.linetype = 'dashed', conf.int = FALSE,
         censor.shape = '*', censor.size = 5, facets = TRUE, ncol = 3)
autoplot(survfit(Surv(vit_stat_int,vit_stat) ~ tumsoort, data = IKNL), fun = 'event')

res <- pairwise_survdiff(Surv(vit_stat_int,vit_stat) ~ tumsoort, data = IKNL); res

fit <- survfit(Surv(vit_stat_int,vit_stat) ~ tumsoort + incjr, data = IKNL)
ggsurv <- ggsurvplot(fit, fun = "event", conf.int = TRUE, ggtheme = theme_bw())
ggsurv$plot +theme_bw() + theme (legend.position = "none")+facet_grid(incjr ~ tumsoort)

sd<-survdiff(Surv(vit_stat_int, 
                  vit_stat) ~ stadium, data = IKNL)
1 - pchisq(sd$chisq, length(sd$n) - 1)

ggsurv<-ggsurvplot(
  fit =  survfit(Surv(vit_stat_int, 
                      vit_stat) ~ stadium, data = IKNL),
  pval = TRUE,
  pval.method = TRUE,
  surv.median.line = "hv",
  conf.int = TRUE,
  risk.table = TRUE,
  risk.table.col="strata",
  xlab = "Days", 
  ylab = "Survival Probability by Days")
curv_facet <- ggsurv$plot + facet_wrap(~stadium);curv_facet

sd<-survdiff(Surv(vit_stat_int, 
                  vit_stat) ~ stadium, data = IKNL)
1 - pchisq(sd$chisq, length(sd$n) - 1)

## Survival by tumour placement
ggsurv<-ggsurvplot(
  fit =  survfit(Surv(vit_stat_int,vit_stat) ~gedrag, data = IKNL),
  pval = TRUE,
  pval.method = TRUE,
  surv.median.line = "hv",
  conf.int = TRUE,
  risk.table = TRUE,
  risk.table.col="strata",
  xlab = "Days", 
  ylab = "Survival Probability by Days")
curv_facet <- ggsurv$plot + facet_wrap(~gedrag);curv_facet

## Survival by sentinal node
ggsurv<-ggsurvplot(
  fit =  survfit(Surv(vit_stat_int, vit_stat) ~swk, data = IKNL),
  pval = TRUE,
  pval.method = TRUE,
  surv.median.line = "hv",
  conf.int = TRUE,
  risk.table = TRUE,
  risk.table.col="strata",
  xlab = "Days", 
  ylab = "Survival Probability by Days")
curv_facet <- ggsurv$plot + facet_wrap(~swk);curv_facet

## Survival by Her2_status
sd<-survdiff(Surv(vit_stat_int, vit_stat) ~ her2_stat, data = IKNL)
1 - pchisq(sd$chisq, length(sd$n) - 1)

ggsurv<-ggsurvplot(
  fit =  survfit(Surv(vit_stat_int, vit_stat) ~ her2_stat, data = IKNL),
  pval = TRUE,
  pval.method = TRUE,
  surv.median.line = "hv",
  conf.int = TRUE,
  risk.table = TRUE,
  risk.table.col="strata",
  xlab = "Days", 
  ylab = "Survival Probability by Days")
curv_facet <- ggsurv$plot + facet_wrap(~her2_stat);curv_facet

## Survival by er_status
sd<-survdiff(Surv(vit_stat_int, vit_stat) ~ er_stat_fact, data = IKNL)
1 - pchisq(sd$chisq, length(sd$n) - 1)

ggsurv<-ggsurvplot(
  fit =  survfit(Surv(vit_stat_int, vit_stat) ~ er_stat_fact, data = IKNL),
  pval = TRUE,
  pval.method = TRUE,
  surv.median.line = "hv",
  conf.int = TRUE,
  risk.table = TRUE,
  risk.table.col="strata",
  xlab = "Days", 
  ylab = "Survival Probability by Days")
curv_facet <- ggsurv$plot + facet_wrap(~er_stat_fact);curv_facet

## Survival by pr_status
sd<-survdiff(Surv(vit_stat_int, vit_stat) ~ pr_stat_fact, data = IKNL)
1 - pchisq(sd$chisq, length(sd$n) - 1)

ggsurv<-ggsurvplot(
  fit =  survfit(Surv(vit_stat_int, vit_stat) ~ pr_stat_fact, data = IKNL),
  pval = TRUE,
  pval.method = TRUE,
  surv.median.line = "hv",
  conf.int = TRUE,
  risk.table = TRUE,
  risk.table.col="strata",
  xlab = "Days", 
  ylab = "Survival Probability by Days")
curv_facet <- ggsurv$plot + facet_wrap(~pr_stat_fact);curv_facet

## Survival by gen_status
sd<-survdiff(Surv(vit_stat_int, vit_stat) ~ gen_stat, data = IKNL)
1 - pchisq(sd$chisq, length(sd$n) - 1)

ggsurv<-ggsurvplot(
  fit =  survfit(Surv(vit_stat_int, vit_stat) ~ gen_stat, data = IKNL),
  pval = TRUE,
  pval.method = TRUE,
  surv.median.line = "hv",
  conf.int = TRUE,
  risk.table = TRUE,
  risk.table.col="strata",
  xlab = "Days", 
  ylab = "Survival Probability by Days")
curv_facet <- ggsurv$plot + facet_wrap(~gen_stat);curv_facet


## Survival by Chirurgische code
sd<-survdiff(Surv(vit_stat_int, vit_stat) ~ uitgebr_chir_code, data = IKNL)
1 - pchisq(sd$chisq, length(sd$n) - 1)

ggsurv<-ggsurvplot(
  fit =  survfit(Surv(vit_stat_int, vit_stat) ~ uitgebr_chir_code, data = IKNL),
  pval = TRUE,
  pval.method = TRUE,
  surv.median.line = "hv",
  conf.int = TRUE,
  risk.table = TRUE,
  risk.table.col="strata",
  xlab = "Days", 
  ylab = "Survival Probability by Days")
curv_facet <- ggsurv$plot + facet_wrap(~uitgebr_chir_code);curv_facet


tryset<-IKNL%>%dplyr::select(vit_stat_int, vit_stat,leeft,tumsoort,perclymf,tum_afm, incjr)
tryset<-tryset[complete.cases(tryset), ]
## Age
res.cut<-surv_cutpoint(tryset,
                       time="vit_stat_int", 
                       event="vit_stat",
                       variables="leeft")
summary(res.cut)
plot(res.cut, "leeft")
res.cat <- surv_categorize(res.cut)
head(res.cat)
fit <- survfit(Surv(vit_stat_int,vit_stat) ~leeft, data = res.cat)
ggsurvplot(fit, data = res.cat,  pval = TRUE,
           pval.method = TRUE,
           surv.median.line = "hv",
           conf.int = TRUE,
           risk.table = TRUE,
           risk.table.col="strata",
           xlab = "Days", 
           ylab = "Survival Probability by Days")


ggplot(IKNL, aes(x=leeft,y=vit_stat_int, 
                 color=as.factor(tumsoort), 
                 fill=as.factor(tumsoort))) + 
  geom_smooth(alpha=0.5)+
  geom_vline(xintercept=72, linetype="dashed")+
  facet_wrap(~gedrag)+
  ggtitle("Survival time by age, tumor and placement, and cancer stage")+  
  xlab("Age") +
  ylab("Survival Time") + 
  theme_bw()

ggplot(IKNL, aes(x=leeft,y=vit_stat_int, 
                 color=tumsoort, 
                 fill=tumsoort)) + 
  geom_smooth(alpha=0.5)+
  facet_wrap(~swk)+
  ggtitle("Survival time by age, tumor and placement, and sentinel node")+  
  xlab("Age") +
  ylab("Survival Time") + 
  theme_bw()

ggplot(IKNL, aes(x=leeft,y=vit_stat_int, 
                 color=tumsoort, 
                 fill=tumsoort)) + 
  geom_smooth(alpha=0.5)+
  facet_wrap(~diffgrad)+
  ggtitle("Survival time by age, tumor and placement, and differential grade")+  
  xlab("Age") +
  ylab("Survival Time") + 
  theme_bw()

ggplot(IKNL, aes(x=leeft,y=vit_stat_int,
                 group=as.factor(gen_stat),
                 colour=as.factor(gen_stat), 
                 fill=as.factor(gen_stat))) + 
  geom_smooth(alpha=0.5)+
  geom_vline(xintercept=72, linetype="dashed")+
  facet_wrap(~incjr)+
  ggtitle("Survival time by age, hormonal results and inclusion year")+  
  xlab("Age") +
  ylab("Survival Time") + 
  theme_bw()

ggplot(IKNL, aes(x=leeft,y=vit_stat_int, 
                 color=stadium, 
                 fill=stadium)) + 
  geom_smooth(alpha=0.5)+
  geom_vline(xintercept=72, linetype="dashed")+
  facet_wrap(~incjr)+
  ggtitle("Survival time by age, cancer stage and inclusion year")+  
  xlab("Age") +
  ylab("Survival Time") + 
  theme_bw()

ggplot(IKNL, aes(x=leeft,y=vit_stat_int, 
                 color=as.factor(tumsoort), 
                 fill=as.factor(tumsoort))) + 
  geom_smooth(alpha=0.5)+
  geom_vline(xintercept=72, linetype="dashed")+
  facet_wrap(~stadium)+
  ggtitle("Survival time by age, tumor, and cancer stage")+  
  xlab("Age") +
  ylab("Survival Time") + 
  theme_bw()

# Survival by tumor placement and age >72
ggsurv<-IKNL%>%
  dplyr::filter(leeft>72)%>%
  ggsurvplot(
    fit =  survfit(Surv(vit_stat_int, 
                        vit_stat) ~gedrag, data = .),
    pval = TRUE,
    pval.method = TRUE,
    surv.median.line = "hv",
    conf.int = TRUE,
    risk.table = TRUE,
    risk.table.col="strata",
    xlab = "Days", 
    ylab = "Survival Probability by Days")
curv_facet <- ggsurv$plot + facet_wrap(~gedrag);curv_facet

# Survival by gen stat and age >72
ggsurv<-IKNL%>%
  dplyr::filter(leeft>72)%>%
  ggsurvplot(
    fit =  survfit(Surv(vit_stat_int, 
                        vit_stat) ~gen_stat, data = .),
    pval = TRUE,
    pval.method = TRUE,
    surv.median.line = "hv",
    conf.int = TRUE,
    risk.table = TRUE,
    risk.table.col="strata",
    xlab = "Days", 
    ylab = "Survival Probability by Days")
curv_facet <- ggsurv$plot + facet_wrap(~gen_stat);curv_facet

# Survival by tumor kind and age>72
ggsurv<-IKNL%>%
  dplyr::filter(leeft>72)%>%
  ggsurvplot(
    fit =  survfit(Surv(vit_stat_int, 
                        vit_stat) ~tumsoort, data = .),
    pval = TRUE,
    pval.method = TRUE,
    surv.median.line = "hv",
    conf.int = TRUE,
    risk.table = TRUE,
    risk.table.col="strata",
    xlab = "Days", 
    ylab = "Survival Probability by Days")
curv_facet <- ggsurv$plot + facet_wrap(~tumsoort);curv_facet

tryset<-IKNL%>%dplyr::select(vit_stat_int, vit_stat,leeft,tumsoort, perclymf, tum_afm, incjr)
tryset$leeft2 <- cut(tryset$leeft,
                     c(10,20,30,40,50,60,70,80,90,100))
tryset$leeft2<-as.character(tryset$leeft2)
ggsurv<-ggsurvplot(
  fit =  survfit(Surv(vit_stat_int, 
                      vit_stat) ~ leeft2, data = tryset),
  pval = TRUE,
  pval.method = TRUE,
  surv.median.line = "hv",
  conf.int = TRUE,
  risk.table = TRUE,
  risk.table.col="strata",
  xlab = "Days", 
  ylab = "Survival Probability by Days")

## Percentage Lymp Node positive
res.cut<-surv_cutpoint(tryset,
                       time="vit_stat_int", 
                       event="vit_stat",
                       variables="perclymf")
summary(res.cut)
plot(res.cut, "perclymf")
res.cat <- surv_categorize(res.cut)
head(res.cat)
fit <- survfit(Surv(vit_stat_int,vit_stat) ~perclymf, data = res.cat)
ggsurvplot(fit, data = res.cat,  pval = TRUE,
           pval.method = TRUE,
           surv.median.line = "hv",
           conf.int = TRUE,
           risk.table = TRUE,
           risk.table.col="strata",
           xlab = "Days", 
           ylab = "Survival Probability by Days")


## Tumor size
res.cut<-surv_cutpoint(tryset,
                       time="vit_stat_int", 
                       event="vit_stat",
                       variables="tum_afm")
summary(res.cut)
plot(res.cut, "tum_afm")
res.cat <- surv_categorize(res.cut)
head(res.cat)
fit <- survfit(Surv(vit_stat_int,vit_stat) ~tum_afm, data = res.cat)
ggsurvplot(fit, data = res.cat,  pval = TRUE,
           pval.method = TRUE,
           surv.median.line = "hv",
           conf.int = TRUE,
           risk.table = TRUE,
           risk.table.col="strata",
           xlab = "Days", 
           ylab = "Survival Probability by Days")

res.cut<-surv_cutpoint(tryset,
                       time="vit_stat_int", 
                       event="vit_stat",
                       variables="incjr")
summary(res.cut)
plot(res.cut, "incjr")
res.cat <- surv_categorize(res.cut)
head(res.cat)
fit <- survfit(Surv(vit_stat_int,vit_stat) ~incjr, data = res.cat)
ggsurvplot(fit, data = res.cat,  pval = TRUE,
           pval.method = TRUE,
           surv.median.line = "hv",
           conf.int = TRUE,
           risk.table = TRUE,
           risk.table.col="strata",
           xlab = "Days", 
           ylab = "Survival Probability by Days")

surv <- Surv(IKNL$vit_stat_int, IKNL$vit_stat)
ggsurvevents(surv)


controlsurv=coxph.control(iter.max=10000)
fit<-survival::coxph(Surv(vit_stat_int, 
                vit_stat) ~
             splines::ns(leeft,3)+
             later+
             diffgrad+
             tumsoort+
             splines::ns(tum_afm,3)+
             splines::ns(pos_lymf,3)+
             gedrag+
             stadium+
             gen_stat+
             swk+
             mari+
             org_chir,
           x=T, y=T, model=T,
           data=IKNL,
           na.action=na.exclude,
           control=controlsurv)
summary(fit)
concordance(fit)
cox_fit <- survfit(fit)
plot(survfit( fit))
fit %>%gtsummary::tbl_regression(exp = TRUE) 
ggforest(fit)
ftest<-cox.zph(fit)
ggcoxzph(ftest)
ggcoxdiagnostics(fit, type="deviance", ox.scale="linear.predictions")
ggcoxdiagnostics(fit, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(fit, type="martingale", ox.scale="linear.predictions")
ggcoxdiagnostics(fit, type="dfbeta", ox.scale="observation.id")

IKNLsub<-IKNL%>%dplyr::select(leeft,vit_stat_int,vit_stat,ond_lymf,pos_lymf,tum_afm)%>%
  filter(complete.cases(.))
fit3<-coxph(Surv(vit_stat_int,vit_stat) ~
              I(leeft^3)+
              I(tum_afm^3)+
              I(pos_lymf^3),
            data=IKNLsub,
            control=controlsurv) 
ggcoxfunctional(fit3,data=IKNLsub)

# Baseline
ggsurvplot(survfit(fit, data=IKNL),  c("#E7B800"),
           ggtheme = theme_minimal())
# New Data Set
gedrag_df<-with(IKNL,
                data.frame(tumsoort   = c("502200","501300", "503200"),
                           gedrag     = c("3","2","3"), 
                           leeft      = c(61.8713, 61.8713,78),
                           tum_afm    = c(20,20,20),
                           pos_lymf   = c(0, 0,0),
                           later      = c( "1","2","1"),
                           diffgrad   = c("3","3","3"),
                           stadium    = c("2B", "2B","3B"),
                           gen_stat   = c("1","2","3"),
                           swk        = c("1","1","0"),
                           mari       = c("0","0","0"),
                           org_chir   = c("1","1","1")))
pfit <- survfit(fit, newdata = gedrag_df)
ggsurvplot(pfit,data=gedrag_df)
ggsurvplot(pfit, 
           data=gedrag_df,
           risk.table = TRUE,
           risk.table.col="strata",      
           pval = TRUE,             
           conf.int = TRUE,
           palette = c("#E7B800", "#2E9FDF","#fe0a00"),
           xlab = "Time in days",   
           break.time.by = 500,     
           ggtheme = theme_light(), 
           risk.table.y.text.col = T,
           risk.table.height = 0.25, 
           risk.table.y.text = FALSE,
           ncensor.plot = TRUE,      
           ncensor.plot.height = 0.25,
           surv.median.line = "hv",  
           legend.labs =c("502200","501300", "503200"))

S <- Surv(IKNL$vit_stat_int,IKNL$vit_stat)
dd<-datadist(IKNL)
options(datadist="dd")
fit<-rms::cph(S ~ 
                rcs(leeft,3)+
                gesl+
                later+
                diffgrad+
                tumsoort+
                rcs(tum_afm,3)+
                pos_lymf+
                gedrag+
                stadium+
                gen_stat+
                swk+
                mari+
                org_chir, x=T,y=T,surv=T,
              time.inc=3000,
              data=IKNL)
summary(fit)
coef(fit)
fit_cal<-calibrate(fit,u=1000,cmethod='KM',m=150,B=500)
plot(fit_cal)
fit_cal<-calibrate(fit,u=2000,cmethod='KM',m=150,B=500)
plot(fit_cal)
fit_cal<-calibrate(fit,u=3000,cmethod='KM',m=150,B=500)
plot(fit_cal)

aa_fit<-aareg(Surv(vit_stat_int,vit_stat) ~
                splines::ns(leeft,3)+
                later+
                diffgrad+
                tumsoort+
                splines::ns(tum_afm,3)+
                pos_lymf+
                gedrag+
                stadium+
                gen_stat+
                swk+
                mari+
                org_chir,
              data=IKNL,
              na.action=na.exclude) 
autoplot(aa_fit)

## RANDOM FOREST SURVIVAL
IKNLsub<-IKNL%>%
  dplyr::select(vit_stat_int,vit_stat,
                leeft,
                later,
                diffgrad,
                tumsoort,
                tum_afm,
                pos_lymf,
                gedrag,
                stadium,
                gen_stat,
                swk,
                mari,
                tum_afm,
                pos_lymf,
                org_chir)%>%filter(complete.cases(.))%>%
  slice_sample(n=5000)
IKNLsub<-as.data.frame(IKNLsub)
# Ranger package
f_fit<-ranger::ranger(Surv(vit_stat_int,vit_stat) ~
                leeft+
                later+
                diffgrad+
                tumsoort+
                gedrag+
                stadium+
                gen_stat+
                swk+
                mari+
                tum_afm+
                pos_lymf+
                org_chir,
              data=IKNLsub,
              mtry = 4,
              importance = "permutation",
              splitrule = "extratrees",
              verbose = TRUE)
# Average the survival models
death_times <- f_fit$unique.death.times 
surv_prob <- data.frame(f_fit$survival)
avg_prob <- sapply(surv_prob,mean)
plot(f_fit$unique.death.times, f_fit$survival[1,])
ranger::treeInfo(f_fit, tree = 1)
ranger::importance(f_fit)
ranger::importance_pvalues(f_fit)
plot(randomForestSRC::tune.nodesize(Surv(vit_stat_int,vit_stat) ~
                     ns(leeft,3)+
                     later+
                     diffgrad+
                     tumsoort+
                     gedrag+
                     stadium+
                     gen_stat+
                     swk+
                     mari+
                     tum_afm+
                     pos_lymf+
                     org_chir,
                   data=IKNLsub, trace=TRUE)$err)
rfsrc_pbc <- randomForestSRC::rfsrc(Surv(vit_stat_int,vit_stat) ~
                     ns(leeft,3)+
                     later+
                     diffgrad+
                     tumsoort+
                     gedrag+
                     stadium+
                     gen_stat+
                     swk+
                     mari+
                     tum_afm+
                     pos_lymf+
                     org_chir,
                   data=IKNLsub,
                   nodesize=9,
                   nsplit = 10, na.action = "na.impute",
                   tree.err = TRUE,importance = TRUE)
summary(rfsrc_pbc)
plot(rfsrc_pbc)

ggRFsrc <- plot(ggRandomForests::gg_rfsrc(rfsrc_pbc), alpha = 0.2) +
  scale_color_manual(values = IKNLsub$leeft) +
  theme(legend.position = "none") +
  labs(y = "Survival Probability", x = "Time (days)") +
  coord_cartesian(ylim = c(-0.01, 1.01)) + theme_bw()
show(ggRFsrc)
plot(ggRandomForests::gg_vimp(rfsrc_pbc))

varsel_pbc <- randomForestSRC::var.select(rfsrc_pbc)
gg_md <- ggRandomForests::gg_minimal_depth(varsel_pbc)
plot(gg_md)
plot(ggRandomForests::gg_minimal_vimp(gg_md))

ggRFsrc + geom_vline(aes(xintercept = 1000), linetype = "dashed") +
  geom_vline(aes(xintercept = 2000), linetype = "dashed") + 
  geom_vline(aes(xintercept = 3000), linetype = "dashed") + 
  geom_vline(aes(xintercept = 4000), linetype = "dashed") + theme_bw()

gg_v <- ggRandomForests::gg_variable(rfsrc_pbc, time = c(1000, 2000, 3000, 4000),
                    time.labels = c("1000 days",
                                    "2000 days",
                                    "3000 days",
                                    "4000 days"))
plot(gg_v, xvar = "leeft", alpha = 0.4) + 
  labs(y = "Survival", x = "Leeftijd") + 
  theme(legend.position = "none") +
  theme_bw()+
  coord_cartesian(ylim = c(-0.01, 1.01))
plot(gg_v, xvar = "tum_afm", alpha = 0.4) + 
  labs(y = "Survival", x = "Leeftijd") + 
  theme(legend.position = "none") +
  theme_bw()+
  coord_cartesian(ylim = c(-0.01, 1.01))
plot(gg_v, xvar = "pos_lymf", alpha = 0.4) + 
  labs(y = "Survival", x = "Leeftijd") + 
  theme(legend.position = "none") +
  theme_bw()+
  coord_cartesian(ylim = c(-0.01, 1.01))

xvar <- c("leeft", "tum_afm", "pos_lymf")
plot(gg_v, xvar = xvar[-1], panel=TRUE, alpha = 0.4) + 
  labs(y = "Survival", x = "Leeftijd") + 
  theme(legend.position = "none") +
  theme_bw()+
  coord_cartesian(ylim = c(-0.01, 1.01))

plot(gg_v, xvar = "gen_stat", alpha = 0.4) + 
  labs(y = "Survival") +
  theme(legend.position = "none")+
  theme_bw()+
  coord_cartesian(ylim = c(-0.01, 1.01))

plot(gg_v, xvar = "tumsoort", alpha = 0.4) + 
  labs(y = "Survival") +
  theme(legend.position = "none")+
  coord_cartesian(ylim = c(-0.01, 1.01))

gg_v <- ggRandomForests::gg_variable(rfsrc_pbc)
plot(gg_v, xvar = xvar[-1], panel=TRUE, alpha = 0.4) + theme_bw()
plot(gg_v, xvar = xvar[1], panel=TRUE, alpha = 0.4)  + theme_bw()
plot(gg_v, xvar = xvar[2], panel=TRUE, alpha = 0.4)  + theme_bw()
plot(gg_v, xvar = xvar[3], panel=TRUE, alpha = 0.4)  + theme_bw()

ggint <- ggRandomForests::gg_interaction(rfsrc_pbc)
plot(ggint, xvar = xvar) + theme_bw()

## PENALIZED package --> L1 is for LASSO, L2 is for ridge regression
# Has a lot of trouble on larger datasets so I used a stratified random sample 
IKNLsub<-IKNL%>%
  dplyr::select(vit_stat_int,vit_stat,
                leeft,
                later,
                diffgrad,
                tumsoort,
                tum_afm,
                pos_lymf,
                gedrag,
                stadium,
                gen_stat,
                swk,
                mari,
                org_chir)%>%filter(complete.cases(.))%>%
  slice_sample(n=10000)
fit1 <- penalized::profL1(Surv(vit_stat_int,vit_stat) ~
                 splines::ns(leeft,3)+
                 later+
                 diffgrad+
                 tumsoort+
                 gedrag+
                 stadium+
                 gen_stat+
                 swk+
                 mari+
                 tum_afm+
                 pos_lymf+
                 org_chir,
               data=IKNLsub,fold=5, plot=TRUE)
penalized::plotpath(fit1$fullfit)
fit2 <- penalized::profL2(Surv(vit_stat_int,vit_stat) ~
                 splines::ns(leeft,3)+
                 later+
                 diffgrad+
                 tumsoort+
                 gedrag+
                 stadium+
                 gen_stat+
                 swk+
                 mari+
                 tum_afm+
                 pos_lymf+
                 org_chir,
               data=IKNLsub,plot=TRUE,fold=fit1$fold,
               minl = 0.01, maxl = 1000)
penalized::plotpath(fit2$fullfit, log="x")
# Optimum L1 and L2 values
opt1 <- optL1(Surv(vit_stat_int,vit_stat) ~
                splines::ns(leeft,3)+
                later+
                diffgrad+
                tumsoort+
                gedrag+
                stadium+
                gen_stat+
                swk+
                mari+
                tum_afm+
                pos_lymf+
                org_chir,
              data=IKNLsub,fold=fit1$fold, standardize=TRUE)
coefficients(opt1$fullfit)
dev.off()
plot(opt1$predictions)
opt2 <- optL2(Surv(vit_stat_int,vit_stat) ~
                splines::ns(leeft,3)+
                later+
                diffgrad+
                tumsoort+
                gedrag+
                stadium+
                gen_stat+
                swk+
                mari+
                tum_afm+
                pos_lymf+
                org_chir,
              data=IKNLsub,fold=fit2$fold, standardize=TRUE)
coefficients(opt2$fullfit)
plot(opt2$predictions)
# Applying optimum L1 and L2 values
IKNLsub<-IKNL%>%
  dplyr::select(vit_stat_int,vit_stat,
                leeft,
                incjr,
                later,
                diffgrad,
                tumsoort,
                gedrag,
                stadium,
                gen_stat,
                swk,
                mari,
                org_chir)%>%filter(complete.cases(.))%>%filter(vit_stat_int>0)
fit_penal<-penalized::penalized(Surv(vit_stat_int,vit_stat) ~
                       splines::ns(leeft,3)+
                       incjr+
                       later+
                       diffgrad+
                       tumsoort+
                       gedrag+
                       stadium+
                       gen_stat+
                       swk+
                       mari+
                       org_chir,
                     data=IKNLsub,
                     lambda1 = opt1$lambda, lambda2=opt2$lambda,
                     standardize=TRUE)
penalized::plotpath(fit_penal, log="x")
coefficients(fit_penal, "all")

dev.off()
y<-cbind(time=IKNLsub$vit_stat_int, status=IKNLsub$vit_stat)
x<-cbind(leeft=IKNLsub$leeft,
         incjr=IKNLsub$incjr,
         later=IKNLsub$later,
         gesl=IKNLsub$gesl,
         diffgrad=IKNLsub$diffgrad,
         tumsoort=IKNLsub$tumsoort,
         gedrag=IKNLsub$gedrag,
         stadium=IKNLsub$stadium,
         gen_stat=IKNLsub$gen_stat,
         swk=IKNLsub$swk,
         mari=IKNLsub$mari,
         org_chir=IKNLsub$org_chir)
fit <- glmnet::glmnet(x,y, family = "cox", maxit = 1000, standardize=T)
par(mfrow = c(1, 2))
plot(fit$lambda)
plot(fit, label=TRUE)
coef(fit, s = 0.01)
dat<-as.data.frame(cbind(y,x))
cv.fit <- glmnet::cv.glmnet(as(x, "dgCMatrix"),y, family = "cox")
plot(cv.fit)
cv.fit$lambda.min
cv.fit$lambda.1se
coef.min = coef(cv.fit, s = "lambda.min")
coef.min 

fit <- glmnet::glmnet(x,y, family = "cox", maxit = 1000, standardize=T, lambda=cv.fit$lambda.min)
par(mfrow = c(1, 2))
plot(fit$lambda)
plot(fit, label=TRUE)
coef(fit, s = 0.01)

ggplot(IKNL, aes(x=leeft, y = vit_stat_int )) +
  geom_point(aes(color = factor(vit_stat)))+
  ggtitle("Censored obs. in red") +
  theme_bw()

fit <- survreg(Surv(vit_stat_int, 
                    vit_stat) ~ leeft + stadium + tumsoort, data = IKNLsub)
summary(fit)
with_ints <- ciTools::add_ci(IKNLsub, fit, names = c("lcb", "ucb")) %>%
  ciTools::add_pi(fit, names = c("lpb", "upb"))
knitr::kable(head(with_ints))

ggplot(with_ints, aes(x = leeft, y = vit_stat_int)) +
  geom_point(aes(color = stadium)) +
  facet_wrap(~tumsoort)+
  theme_bw() +
  ggtitle("Model fit with 95% CIs and PIs",
          "solid line = mean, dotted line = median") +
  geom_line(aes(y = mean_pred), linetype = 1) +
  geom_line(aes(y = median_pred), linetype = 2) +
  geom_ribbon(aes(ymin = lcb, ymax = ucb), alpha = 0.5) +
  geom_ribbon(aes(ymin = lpb, ymax = upb), alpha = 0.1)

probs <- ciTools::add_probs(IKNLsub, fit, q = 500,
                            name = c("prob", "lcb", "ucb"),
                            comparison = ">")
ggplot(probs, aes(x = leeft, y = prob, color=stadium, fill=stadium)) +
  ggtitle("Estimated prob. of avg. spring lasting longer than 500 hrs.") +
  ylim(c(0,1)) +
  facet_wrap(~tumsoort)+
  theme_bw() +
  geom_line(aes(y = prob)) +
  geom_ribbon(aes(ymin = lcb, ymax = ucb), alpha = 0.5)

quants <- ciTools::add_quantile(IKNLsub, fit, p = 0.90,
                                name = c("quant", "lcb", "ucb"))
ggplot(quants, aes(x = leeft, y = vit_stat_int)) +
  geom_point(aes(color = stadium)) +
  ggtitle("Estimated 90th percentile of condtional failure distribution, with CI") +
  facet_wrap(~tumsoort)+
  theme_bw() +
  geom_line(aes(y = quant)) +
  geom_ribbon(aes(ymin = lcb, ymax = ucb), alpha = 0.5)




rms::Predict(fit, leeft=50, gedrag='3',fun=plogis)
plot(rms::nomogram(fit, fun = function(x)plogis(x)))

DynNom::DynNom(fit, IKNL)






























































#### LOAD, WRANGLE AND EXPLORE IKNL DATA   - SYNTHETIC CANCER CLINICAL PREDICTION MODEL -----
## Load and prepare data
IKNL<- read_delim("Data/IKNL/Synthetisch/NKR_IKNL_breast_syntheticdata.csv", delim = ";", 
                  escape_double = FALSE, trim_ws = TRUE)
dim(IKNL)
str(IKNL)
colnames(IKNL)
IKNL$perclymf<-IKNL$pos_lymf/IKNL$ond_lymf
IKNL$c_eq_p<-as.factor(ifelse(IKNL$ct==IKNL$pt, "TRUE", "FALSE"))
IKNL$her2_stat<-ifelse(IKNL$her2_stat==9, NA, IKNL$her2_stat)
IKNL$er_stat<-ifelse(IKNL$er_stat==9, NA, IKNL$er_stat)
IKNL$pr_stat<-ifelse(IKNL$pr_stat==9, NA, IKNL$pr_stat)
IKNL$her2_stat_fact<-ifelse(IKNL$her2_stat==3, 1, 0)
IKNL$er_stat_fact<-ifelse(IKNL$er_stat>0, 1, 0)
IKNL$pr_stat_fact<-ifelse(IKNL$pr_stat>0, 1, 0)
IKNL$gen_stat<-IKNL$her2_stat_fact+IKNL$er_stat_fact+IKNL$pr_stat_fact
IKNL$tum_afm<-as.numeric(IKNL$tum_afm)
IKNL$Stage<-substr(IKNL$stadium, 1, 1)   
col_names <- names(IKNL[,c(4,8:24,27:31,33:46,49:52)])
IKNL[,col_names] <- lapply(IKNL[,col_names] , factor)
sapply(IKNL, class)
str(IKNL)

## Rename levels of variables
levels(IKNL$tumsoort)<-c("Invasief mammacarcinoom",
                         "Ductaal carcinoma in situ",
                         "Lobulair carcinoma in situ")
levels(IKNL$diag_basis)<-c("Clinical-Diagnostic Research",
                           "Hematological of cutological confirmation",
                           "Histological confirmation metastases",
                           "Histological confirmation primary tumor")
levels(IKNL$diffgrad)<-c("Low grade",
                         "Intermediate grade",
                         "High grade",
                         "NVT")
levels(IKNL$uitgebr_chir_code)<-c("Lumpectomie (no OKD)", 
                                  "Lumpectomie (with OKD)", 
                                  "Ablatio (no OKD)",
                                  "Amputation (with OKD)", 
                                  "Resection other (coincidence)", 
                                  "Lymph node dissection")
levels(IKNL$horm)<-c("No",
                     "Pre-surgery",
                     "Post-surgery",
                     "Pre-and-post-surgery", 
                     "Yes, no surgery")
levels(IKNL$chemo)<-c("No",
                      "Pre-surgery", 
                      "Post-surgery",
                      "Pre-and-post-surgery",
                      "Yes, no surgery")
levels(IKNL$rt)<-c("No",
                   "Pre-surgery",
                   "Post-surgery",
                   "Yes, no surgery")
levels(IKNL$target)<-c("No",
                       "Pre-surgery",
                       "Post-surgery",
                       "Pre-and-post-surgery", 
                       "Yes, no surgery")
levels(IKNL$mari)<-c("No",
                     "Yes")
levels(IKNL$swk)<-c("No",
                       "Yes", 
                       "Not registered")
levels(IKNL$tumsoort)
levels(IKNL$diag_basis)
levels(IKNL$diffgrad)
levels(IKNL$uitgebr_chir_code)
levels(IKNL$horm)
levels(IKNL$chemo)
levels(IKNL$rt)
levels(IKNL$target)
levels(IKNL$mari)
levels(IKNL$swk)

df<-IKNL%>%dplyr::filter(vit_stat_int>0)%>%filter(tumsoort=="Invasief mammacarcinoom")
df2<-df%>%dplyr::filter(vit_stat==1)%>%filter(tumsoort=="Invasief mammacarcinoom")
df2c<-df2%>%dplyr::select(vit_stat_int,vit_stat,stadium, tumsoort)%>%drop_na()%>%filter(tumsoort=="Invasief mammacarcinoom")
df2010<-IKNL%>%dplyr::filter(vit_stat_int>0 & incjr>2009)%>%filter(tumsoort=="Invasief mammacarcinoom")
df2010_clinical<-df2010%>%dplyr::select(vit_stat_int, vit_stat, leeft,incjr,diffgrad,Stage, 
                                 pos_lymf, tum_afm, her2_stat_fact, er_stat_fact,pr_stat_fact, 
                                 horm, chemo, rt, target,diag_basis, uitgebr_chir_code, 
                                 mari, swk)%>%
  as.data.frame(df2010_clinical)%>%
  filter(Stage>0)%>%
  filter(!diffgrad%in%c("NVT"))%>%
  filter(!rt%in%c("Yes, no surgery"))%>% # this is because that cell is zero, creating convergence issues
  mutate(Stage=factor(Stage),
       diffgrad=factor(diffgrad), 
       rt=factor(rt))
str(df2010_clinical)
table(df2010_clinical$rt)

## Look for correlations of multicollinearity
df2010_clinical%>%
  dplyr::select(vit_stat_int,vit_stat,leeft,incjr,diffgrad,Stage,er_stat_fact,pr_stat_fact,
         her2_stat_fact,tum_afm,pos_lymf,horm,chemo,rt,target,uitgebr_chir_code)%>%
  tidyr::drop_na()%>%
  dplyr::select_if(., is.numeric)%>%
  cor()

## First clinical model
S <- Surv(df2010_clinical$vit_stat_int,df2010_clinical$vit_stat)
dd<-rms::datadist(df2010_clinical)
options(datadist="dd")
rms_fit<-rms::cph(S ~
                    I(leeft^3)+
                    incjr+
                    diffgrad+
                    Stage+
                    er_stat_fact+
                    pr_stat_fact+
                    her2_stat_fact+
                    I(tum_afm^3)+
                    I(pos_lymf^3)+
                    swk+
                    mari+
                    horm+
                    chemo+
                    rt+
                    uitgebr_chir_code,
                  x=T,y=T,surv=T,
                  time.inc=500,
                  data=df2010_clinical)
rms_fit 

## Prepare for Elastic Net
df2010_clinical_sub<-df2010_clinical%>%
  dplyr::select(vit_stat_int,
                vit_stat,
                leeft,
                incjr,
                diffgrad,
                Stage,
                er_stat_fact,
                pr_stat_fact,
                her2_stat_fact,
                tum_afm,
                pos_lymf,
                swk,
                mari,
                horm,
                chemo,
                rt,
                uitgebr_chir_code)%>%filter(complete.cases(.))%>%
  group_by(incjr)%>%
  slice_sample(n=500)
dim(df2010_clinical_sub)
dim(df2010_clinical)
## Conduct Elastic Net
fit1 <- profL1(Surv(vit_stat_int,vit_stat) ~
                 ns(leeft,3)+
                 incjr+
                 diffgrad+
                 Stage+
                 er_stat_fact+
                 pr_stat_fact+
                 her2_stat_fact+
                 ns(tum_afm,3)+
                 ns(pos_lymf,3)+
                 swk+
                 mari+
                 horm+
                 chemo+
                 rt+
                 uitgebr_chir_code,
               data=df2010_clinical_sub,fold=5, plot=TRUE)
plotpath(fit1$fullfit)
fit2 <- profL2(Surv(vit_stat_int,vit_stat) ~
                 ns(leeft,3)+
                 incjr+
                 diffgrad+
                 Stage+
                 er_stat_fact+
                 pr_stat_fact+
                 her2_stat_fact+
                 ns(tum_afm,3)+
                 ns(pos_lymf,3)+
                 swk+
                 mari+
                 horm+
                 chemo+
                 rt+
                 uitgebr_chir_code,
               data=df2010_clinical_sub,plot=TRUE,fold=fit1$fold,
               minl = 0.01, maxl = 1000)
plotpath(fit2$fullfit, log="x")
# Optimum L1 and L2 values
opt1 <- optL1(Surv(vit_stat_int,vit_stat) ~
                ns(leeft,3)+
                incjr+
                diffgrad+
                Stage+
                er_stat_fact+
                pr_stat_fact+
                her2_stat_fact+
                ns(tum_afm,3)+
                ns(pos_lymf,3)+
                swk+
                mari+
                horm+
                chemo+
                rt+
                uitgebr_chir_code,
              data=df2010_clinical_sub,fold=fit1$fold, standardize=TRUE)
coefficients(opt1$fullfit)
plot(opt1$predictions)
opt2 <- optL2(Surv(vit_stat_int,vit_stat) ~
                ns(leeft,3)+
                incjr+
                diffgrad+
                Stage+
                er_stat_fact+
                pr_stat_fact+
                her2_stat_fact+
                ns(tum_afm,3)+
                ns(pos_lymf,3)+
                swk+
                mari+
                horm+
                chemo+
                rt+
                uitgebr_chir_code,
              data=df2010_clinical_sub,fold=fit2$fold, standardize=TRUE)
coefficients(opt2$fullfit)
plot(opt2$predictions)

# Applying optimum L1 and L2 values
df2010_clinical_sub<-df2010_clinical%>%
  dplyr::select(vit_stat_int,
                vit_stat,
                leeft,
                incjr,
                diffgrad,
                Stage,
                er_stat_fact,
                pr_stat_fact,
                her2_stat_fact,
                tum_afm,
                pos_lymf,
                swk,
                mari,
                horm,
                chemo,
                rt,
                uitgebr_chir_code)%>%
  filter(complete.cases(.))%>%
  filter(vit_stat_int>0)
fit_penal<-penalized(Surv(vit_stat_int,vit_stat) ~
                       ns(leeft,3)+
                       incjr+
                       diffgrad+
                       Stage+
                       er_stat_fact+
                       pr_stat_fact+
                       her2_stat_fact+
                       ns(tum_afm,3)+
                       ns(pos_lymf,3)+
                       swk+
                       mari+
                       horm+
                       chemo+
                       rt+
                       uitgebr_chir_code,
                     data=df2010_clinical_sub,
                     lambda1 = opt1$lambda, lambda2=opt2$lambda,
                     standardize=TRUE)


## Visualize penalization
coefs<-coefficients(fit_penal, "all")%>%as.data.frame()
vars = rownames(as.data.frame(coefs))
df<-data.frame(variables=vars,
              coefficient=coefs$.)%>%
  mutate(include = factor(if_else(!coefficient==0, "yes", "no")))%>%
  arrange(variables)

ggplot(df)+
  geom_bar(aes(y=variables, x=coefficient), stat="identity", fill="black")+
  theme_bw()+
  facet_grid(include~"", scales="free")+
  labs(x="Coefficient", 
       y="Variable", 
       col="Inclusion",
       title="Results from an Elastic Net Cox Regression model", 
       subtitle="44 regression coefficients of which 19 are non-zero", 
       caption="Source: IKNL - synthetic data")

## Elastic Net via GLMNET
dev.off()
y<-cbind(time=df2010_clinical_sub$vit_stat_int, status=df2010_clinical_sub$vit_stat)
x<-cbind(leeft=df2010_clinical_sub$leeft,
         incjr=df2010_clinical_sub$incjr,
         diffgrad=df2010_clinical_sub$diffgrad,
         Stage=df2010_clinical_sub$Stage,
         er_stat_fact=df2010_clinical_sub$er_stat_fact,
         pr_stat_fact=df2010_clinical_sub$pr_stat_fact,
         her2_stat_fact=df2010_clinical_sub$her2_stat_fact,
         tum_afm=df2010_clinical_sub$tum_afm,
         pos_lymf=df2010_clinical_sub$pos_lymf,
         swk=df2010_clinical_sub$sw,
         mari=df2010_clinical_sub$mari,
         horm=df2010_clinical_sub$horm,
         chemo=df2010_clinical_sub$chemo,
         rt=df2010_clinical_sub$rt,
         uitgebr_chir_code=df2010_clinical_sub$uitgebr_chir_code)
fit <- glmnet::glmnet(x,y, family = "cox", maxit = 1000)
par(mfrow = c(1, 1))
plot(fit, label=TRUE)
cv.fit <- glmnet::cv.glmnet(x,y, family = "cox", maxit = 100000, nfolds=100)
plot(cv.fit)
cv.fit$lambda.min
cv.fit$lambda.1se
coef.min = coef(cv.fit, s = "lambda.min")
coef.min 

## Internal validation of full clinical model
rms_fit_500<-rms::cph(S ~
                    I(leeft^3)+
                    incjr+
                    diffgrad+
                    Stage+
                    er_stat_fact+
                    pr_stat_fact+
                    her2_stat_fact+
                    I(tum_afm^3)+
                    I(pos_lymf^3)+
                    swk+
                    mari+
                    horm+
                    chemo+
                    rt+
                    uitgebr_chir_code,
                  x=T,y=T,surv=T,
                  time.inc=500,
                  data=df2010_clinical)
rms_fit
fit_cal_500<-rms::calibrate(rms_fit_500,u=500,cmethod='KM',m=500,B=1000)
plot(fit_cal)
rms_fit_1000<-rms::cph(S ~
                    I(leeft^3)+
                    incjr+
                    diffgrad+
                    Stage+
                    er_stat_fact+
                    pr_stat_fact+
                    her2_stat_fact+
                    I(tum_afm^3)+
                    I(pos_lymf^3)+
                    swk+
                    mari+
                    horm+
                    chemo+
                    rt+
                    uitgebr_chir_code,
                  x=T,y=T,surv=T,
                  time.inc=1000,
                  data=df2010_clinical)
fit_cal_1000<-rms::calibrate(rms_fit_1000,u=1000,cmethod='KM',m=500,B=1000)
plot(fit_cal_1000)
rms_fit_1500<-rms::cph(S ~
                         I(leeft^3)+
                         incjr+
                         diffgrad+
                         Stage+
                         er_stat_fact+
                         pr_stat_fact+
                         her2_stat_fact+
                         I(tum_afm^3)+
                         I(pos_lymf^3)+
                         swk+
                         mari+
                         horm+
                         chemo+
                         rt+
                         uitgebr_chir_code,
                       x=T,y=T,surv=T,
                       time.inc=1500,
                       data=df2010_clinical)
fit_cal_1500<-rms::calibrate(rms_fit_1500,u=1500,cmethod='KM',m=500,B=1000)
plot(fit_cal_1500)
rms_fit_2000<-rms::cph(S ~
                         I(leeft^3)+
                         incjr+
                         diffgrad+
                         Stage+
                         er_stat_fact+
                         pr_stat_fact+
                         her2_stat_fact+
                         I(tum_afm^3)+
                         I(pos_lymf^3)+
                         swk+
                         mari+
                         horm+
                         chemo+
                         rt+
                         uitgebr_chir_code,
                       x=T,y=T,surv=T,
                       time.inc=2000,
                       data=df2010_clinical)
fit_cal_2000<-rms::calibrate(rms_fit_2000,u=2000,cmethod='KM',m=500,B=1000)
plot(fit_cal_2000)

## Run CoxPH model for assessment - did not incorporate any regularization 
df2010_clinical_compl<-df2010_clinical%>%tidyr::drop_na()
dim(df2010_clinical)
DataExplorer::plot_missing(df2010_clinical)
dim(df2010_clinical_compl)

controlsurv=coxph.control(iter.max=10000)
fit<-coxph(Surv(vit_stat_int, 
                vit_stat) ~
             splines::ns(leeft,3)+
             incjr+
             diffgrad+
             Stage+
             er_stat_fact+
             pr_stat_fact+
             her2_stat_fact+
             splines::ns(tum_afm,3)+
             splines::ns(pos_lymf,3)+
             swk+
             horm+
             chemo+
             rt+
             uitgebr_chir_code,
           na.action = na.exclude,
           data=df2010_clinical,
           control=controlsurv)
## Assess model fit via residuals
broom::tidy(fit)
broom::glance(fit)
summary(fit)
concordance(fit)
car::vif(fit)
ftest<-cox.zph(fit)
ggcoxzph(ftest)
ggcoxdiagnostics(fit, type="deviance", ox.scale="linear.predictions")
ggcoxdiagnostics(fit, type = "deviance",linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(fit, type="martingale", ox.scale="linear.predictions")
plot(survfit(fit))
ggsurvplot(survfit(fit, data=df2010_clinical),  
           c("#E7B800"),
           ggtheme = theme_minimal())
fit %>%gtsummary::tbl_regression(exp = TRUE) 
ggforest(fit) + theme_bw()

## Assessment by creating fake clinical groups and extracting survival probabilities
levels(df2010_clinical$horm)
levels(df2010_clinical$chemo)
levels(df2010_clinical$rt)
levels(df2010_clinical$target)
levels(df2010_clinical$uitgebr_chir_code)
new_df<-with(df2010_clinical,
                data.frame(leeft              = c(30,40,59),
                           incjr              = c(2019,2019,2019),
                           diffgrad           = c("Intermediate grade","Intermediate grade","Intermediate grade"),
                           Stage              = c("1","2","2"),
                           er_stat_fact       = c("1","1","1"),
                           pr_stat_fact       = c("1","1","1"),
                           her2_stat_fact     = c("0","0","1"),
                           tum_afm            = c("15","20","25"), 
                           pos_lymf           = c("0", "3","6"), 
                           horm               = c("Post-surgery","Post-surgery","Post-surgery"),
                           chemo              = c("No","No","No"),
                           rt                 = c("No","Post-surgery","Post-surgery"),
                           target             = c("No","No","No"),
                           uitgebr_chir_code  = c("Lumpectomie (with OKD)","Lumpectomie (with OKD)","Lumpectomie (with OKD)")))
new_df<-new_df%>%mutate(ID=row_number())

pfit <- survfit(fit, newdata = new_df)
ggsurvplot(pfit, data=new_df, censor.shape="|", censor.size = 1)

ggsurvplot(pfit, 
           data=new_df,
           risk.table = TRUE,  
           pval = TRUE,             
           conf.int = TRUE,
           palette = c("#E7B800", "#2E9FDF","#fe0a00"),
           xlab = "Time in days",   
           break.time.by = 500,
           ggtheme = theme_light(),
           risk.table.y.text.col = T,
           risk.table.height = 0.25, 
           risk.table.y.text = FALSE,
           ncensor.plot = TRUE,      
           ncensor.plot.height = 0.25,
           surv.median.line = "hv",
           censor = FALSE,
           legend.labs =c("30","40", "59"))

data.frame(surv = pfit$surv,lower= pfit$lower, upper = pfit$upper,time= pfit$time)%>%
  reshape2::melt(., id="time")%>%
  separate(col = variable, into = c("type", "ID"), sep = "\\.") %>%
  spread(key = type, value = value)%>%
  mutate(ID=recode_factor(ID, 
                `1` = "30", 
                `2` = "40", 
                `3` ="59"))%>%
  mutate(Survprob=if_else(time==365, surv, 
                      if_else(time==365*2, surv, 
                              if_else(time==365*3, surv, 
                                      if_else(time==365*4, surv, NA)))))%>%
  mutate(Timeprob=if_else(!is.na(Survprob)==TRUE, time, NA))%>%
  ggplot() +
  geom_line(aes(x = time, y = surv, group = ID, colour = ID),
            linewidth = 3.5,
            linetype = 1) +
  geom_ribbon(aes(x = time, ymin = lower, ymax = upper, group = ID, fill = ID),
              alpha = 0.5, show.legend = FALSE) +
  geom_point(aes(x=time, y=Survprob, group=ID), col="black")+
  scale_colour_manual(values = c("#E7B800", "#2E9FDF", 'red')) +
  scale_fill_manual(values = c("#E7B800", "#2E9FDF", 'red')) +
  scale_x_continuous(limits = c(0, 4000),
                     breaks = seq(0, 4000,500)) +
  theme(legend.position = "top",
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.key.size = unit(3, "line"),
        legend.title = element_blank()) +
  labs(y="Survival probability", 
       x="Time since diagnosis", 
       fill="Age",
       title="Survival probability for groups based on clinical characteristics", 
       subtitle="Clinical groups differ more than by age", 
       caption="Source: IKNL-synthetic data")+
  facet_grid(~ID)+
  geom_segment(aes(x=0,xend=Timeprob,y=Survprob,yend=Survprob, group=ID), lty=2, col="black")+
  geom_segment(aes(x=Timeprob,xend=Timeprob,y=0,yend=Survprob, group=ID), lty=2, col="black") +
  theme_bw()





df2010_clinical_sub<-df2010_clinical%>%
  dplyr::select(vit_stat_int,
                vit_stat,
                leeft,
                incjr,
                diffgrad,
                Stage,
                er_stat_fact,
                pr_stat_fact,
                her2_stat_fact,
                tum_afm,
                pos_lymf,
                horm,
                chemo,
                rt,
                uitgebr_chir_code)%>%filter(complete.cases(.))
controlsurv=coxph.control(iter.max=10000)
fit<-coxph(Surv(vit_stat_int, 
                vit_stat) ~
             splines::ns(leeft,3)+
             incjr+
             diffgrad+
             Stage+
             er_stat_fact+
             pr_stat_fact+
             her2_stat_fact+
             splines::ns(tum_afm,3)+
             splines::ns(pos_lymf,3)+
             horm+
             chemo+
             rt+
             uitgebr_chir_code,
           data=df2010_clinical_sub,
           control=controlsurv)
concordance(fit)
DynNom::DynNom(fit, df2010_clinical_sub)


df2010_clinical_sub<-df2010_clinical%>%
  dplyr::select(vit_stat_int,
                vit_stat,
                leeft,
                incjr,
                diffgrad,
                Stage,
                er_stat_fact,
                pr_stat_fact,
                her2_stat_fact,
                tum_afm,
                pos_lymf,
                horm,
                chemo,
                rt,
                uitgebr_chir_code)%>%filter(complete.cases(.))
controlsurv=coxph.control(iter.max=10000)
fit_sub<-coxph(Surv(vit_stat_int, 
                vit_stat) ~
             splines::ns(leeft,3)+
             incjr+
             diffgrad+
             Stage+
             er_stat_fact+
             pr_stat_fact+
             her2_stat_fact+
             tum_afm+
             pos_lymf+
             horm+
             chemo+
             rt+
             uitgebr_chir_code,
           data=df2010_clinical_sub,
           control=controlsurv)
concordance(fit_sub)
concordance(fit)


DynNom::DynNom(fit_sub, df2010_clinical_sub)

df2010_clinical%>%
  mutate(incjr=factor(incjr), 
         Stage=factor(Stage),
         vit_stat=factor(vit_stat))%>%
  group_by(incjr, Stage, vit_stat)%>%
  summarise(medsurv = median(vit_stat_int)/365.25)%>%
  ggplot()+
  geom_bar(aes(x=incjr, y=medsurv), stat="identity")+
  facet_grid(vit_stat~Stage)+
  theme_bw()+
  labs(x="Imclusion Year", 
       y="Median Survival time (years)", 
       title="Median survival time",
       subtitle="by inclusion year, stage, and vital status", 
       caption="Source: IKNL - synthetic data")
  


surv_all <- Surv(df2010_clinical$vit_stat_int, df2010_clinical$vit_stat)
ggsurvevents(surv_all, normalized = TRUE)

surv_1<- Surv(df2010_clinical$vit_stat_int[df2010_clinical$Stage=="1"], 
                 df2010_clinical$vit_stat[df2010_clinical$Stage=="1"])
ggsurvevents(surv_1, normalized = TRUE)

surv_2<- Surv(df2010_clinical$vit_stat_int[df2010_clinical$Stage=="2"], 
              df2010_clinical$vit_stat[df2010_clinical$Stage=="2"])
ggsurvevents(surv_2, normalized = TRUE)

surv_3<- Surv(df2010_clinical$vit_stat_int[df2010_clinical$Stage=="3"], 
              df2010_clinical$vit_stat[df2010_clinical$Stage=="3"])
ggsurvevents(surv_3, normalized = TRUE)

surv_4<- Surv(df2010_clinical$vit_stat_int[df2010_clinical$Stage=="4"], 
              df2010_clinical$vit_stat[df2010_clinical$Stage=="4"])
ggsurvevents(surv_4, normalized = TRUE)

surv_M<- Surv(df2010_clinical$vit_stat_int[df2010_clinical$Stage=="M"], 
              df2010_clinical$vit_stat[df2010_clinical$Stage=="M"])
ggsurvevents(surv_M, normalized = TRUE)








