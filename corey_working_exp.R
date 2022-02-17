## ----setup, include=FALSE----------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----------------------------------------------------------------------
# Database interaction imports
library(odbc)

# for data manipulation/visualization
library(tidyverse)

# scaling data, calculating percentages, overriding default graphing
library(scales)

# add weights to data
library(survey)

library(ggplot2)
library(plyr)

library(sandwich)
library(lmtest)


## ----------------------------------------------------------------------
con <- DBI::dbConnect(odbc::odbc(),
                     Driver = "SQL Server",
                     Server = "msssql01.c7bdq4o2yhxo.us-gov-west-1.rds.amazonaws.com",
                     Trusted_Connection = "True")


## ---- eval=FALSE-------------------------------------------------------
##
## dbListTables(con)
##


## ----------------------------------------------------------------------
#ds_nsf_ncses.dbo.nsf_sed

query<-"
select  *
from ds_nsf_ncses.dbo.nsf_sed
where phdfy >= 2015
"

queryb<-"
select  *
from ds_nsf_ncses.dbo.nsf_sed_bacc
"


query2<-"
select  *
from ds_nsf_ncses.dbo.nsf_herd
"


# query3<-"
# select  *
# from ds_iris_umetrics.dbo.umetrics_nsf_grants
# where fy in ( '2010', '2011', '2012','2013', '2014', '2015')
#"


query3<-"
select  *
from ds_iris_umetrics.dbo.umetrics_nsf_grants
where fy >2009
"


query4<-"
select  *
from ds_iris_umetrics.dbo.iris_herd_xwalk
"


df<-dbGetQuery(con, query)

dfb<-dbGetQuery(con, queryb)
# dfb <- dfb%>%
#   filter(as.numeric(bayear) < 2015)

herd<-dbGetQuery(con, query2)

umet15 <- dbGetQuery(con, query3)
xwalk <- dbGetQuery(con, query4)



## ----------------------------------------------------------------------

um1<- merge(umet15, xwalk,by.x="institution_id", by.y="iris_inst_id", all.x=F, all.y=F )





## ----------------------------------------------------------------------
hsi<- readr::read_csv("P:/tr-uncf-excelencia/2015-HSI_List_Excelencia.csv")
hsi<-hsi%>%
  distinct(UnitID, .keep_all = T)



## ----eval=FALSE--------------------------------------------------------
## md2<- merge(um1, hsi,by.x = "ipeds_inst_id", by.y = "UnitID", all.x=F )
##
## length(unique(md2$institution_id))
##
## #table(md2$`Institution Name`)
##


## ----------------------------------------------------------------------
df<-merge(df, dfb, by = "drf_id")

#table(df$phdfield_name)
class(df$phdfield_name)
#table(df$phdhbcu)

#table(herd$hbcu_flag, herd$hhe_flag)


## ----------------------------------------------------------------------
#head(herd)

#herdmsi<- herd%>%
#  filter(hhe_flag=="T"|hbcu_flag=="T")

herd$pwi<-ifelse(herd$hhe_flag!="T" & herd$hbcu_flag!="T", 1, 0)

herd_un<- herd%>%
  distinct( ncses_inst_id, .keep_all = T)



## ----------------------------------------------------------------------

herdpwi <-herd_un%>%
  filter(pwi==1)

herdmsi<-herd_un%>%
  filter(hhe_flag=="T"|hbcu_flag=="T")

herdhsi<-herd_un%>%
  filter(hhe_flag=="T")

herdmsis<- hsi$UnitID[which(unique(hsi$UnitID)%in%unique(herdhsi$ipeds_inst_id))]



## ---- eval=FALSE-------------------------------------------------------
## length(herdmsis)/length(hsi$UnitID)
##
## #testhsi<- hsi%>%
## #  filter(!(UnitID %in% unique(herdhsi$ipeds_inst_id)))
##
## #table(herd$pwi)


## ----------------------------------------------------------------------

corey<- df%>%
  filter(phdfield_name%in% c("Agriculture (Life Sciences)", "Biological/Biomedical Sciences (Life Sciences)", "Engineering", "Mathematics", "Physical Sciences ") )%>%
  mutate(race_eth = car::Recode(as.numeric(race),
                                recodes ="1:2='aian';3='asian'; 4='aian';5 = 'black'; 6:9 = 'hisp'; 10='white'; 11:12='other'; else=NA" ))



## ----------------------------------------------------------------------

corey_msi<-merge(corey, herdmsi,
              by.x = "phdnid",
              by.y = "ncses_inst_id",
              all.x=F, all.y=F)
corey_msi$msi<-1

coreywhite<-merge(corey, herdpwi,
                  by.x = "phdnid",
                  by.y = "ncses_inst_id",
                  all.x=F, all.y=F)
coreywhite$msi<-0

corey_all<-rbind(corey_msi, coreywhite)


## ----------------------------------------------------------------------
cls<- readRDS("P:/tr-uncf-excelencia/Team 4_UTSA/ipedcl.rds")

corey_all$bainst<- as.numeric(corey_all$bainst)
corey_all<-left_join(corey_all, cls, by = c("bainst" ="unitid"))



## ---- eval=FALSE-------------------------------------------------------
## library(plyr)
##
## corey_msi%>%
##   filter(is.na(race_eth)==F)%>%
##   group_by(race_eth)%>%
##   dplyr::summarise(num = round_any(n(),  10), ninst = round_any(n_distinct(phdnid), 10))%>%
##   mutate(freq = num/sum(num))%>%
##   mutate(freq = round(freq, 2))%>%
##   filter (n>10)%>%
##   ggplot()+geom_bar(aes(y = freq,
##                         x=race_eth,
##                         fill=race_eth),
##                     stat="identity")+
##   labs(title = "Relative Frequency of Race/Ethnicity",
##        subtitle = "Among MSI Institutions Recipients")+
##   ylim(c(0, .6))
##
##
## coreywhite%>%
##   filter(is.na(race_eth)==F)%>%
##   group_by(race_eth)%>%
##   dplyr::summarise(num = round_any(n(), 10), ninst = round_any(n_distinct(phdnid),  10))%>%
##   mutate(freq = num/sum(num))%>%
##   mutate(freq = round(freq, 2))%>%
##   filter (n>10)%>%
##   knitr::kable()
##
## coreywhite%>%
##   filter(is.na(race_eth)==F)%>%
##   group_by(race_eth)%>%
##   dplyr::summarise(num = round_any(n(), 10), ninst = round_any(n_distinct(phdnid),  10))%>%
##   mutate(freq = num/sum(num))%>%
##   mutate(freq = round(freq, 2))%>%
##   filter (n>10)%>%
##   ggplot()+geom_bar(aes(y = freq,
##                         x=race_eth,
##                         fill=race_eth),
##                     stat="identity")+
##   labs(title = "Relative Frequency of Race/Ethnicity",
##        subtitle = "Among PWI Institutions Recipients")+
##   ylim(c(0, .6))
##
##


## ---- fig.width = 12, fig.height=8-------------------------------------
corey_all<-corey_all%>%
  filter(is.na(race_eth)==F)%>%
  mutate(msi2 = ifelse(hbcu_flag=="T", "HBCU",
                       ifelse(hhe_flag=="T", "HSI", "PWI")))


fig1dat<-corey_all%>%
  group_by(race_eth, msi2, .drop=F)%>%

  dplyr::summarise(num = round_any(n(),  10), ninst = round_any(n_distinct(phdnid),  10))%>%
 # mutate(freq = num/sum(num))%>%
 # mutate(freq = round(freq, 2))%>%

  #summarise(n = n(), ninst = n_distinct(phdnid))%>%
  ungroup()%>%
  group_by(msi2, .drop = FALSE)%>%
  dplyr::mutate(freq = round((num/sum(num)), 2))%>%
  filter(num >=10)

write.csv(fig1dat, file = "U:/Corey.Sparks.T00104//fig1dat.csv")

fig1datUR<-corey_all%>%
  group_by(race_eth, msi2, .drop=F)%>%

  dplyr::summarise(num =n(), ninst =n_distinct(phdnid))%>%
 # mutate(freq = num/sum(num))%>%
 # mutate(freq = round(freq, 2))%>%

  #summarise(n = n(), ninst = n_distinct(phdnid))%>%
  ungroup()%>%
  group_by(msi2, .drop = FALSE)%>%
  dplyr::mutate(freq =(num/sum(num)))%>%
  filter(num >=10)

write.csv(fig1datUR, file = "U:/Corey.Sparks.T00104//fig1datUNrounded.csv")


## ----------------------------------------------------------------------
# corey_all%>%
#   group_by(race_eth, msi2, .drop=F)%>%
#   summarise(n = n(), ninst = n_distinct(phdnid))%>%
#   ungroup()%>%
#   group_by(msi2, .drop = FALSE)%>%
#   mutate(freq = n/sum(n))%>%
#   filter(n >10)%>%
  #filter (n>10)%>%
  #mutate(msi = ifelse(msi ==1, "MSI", "PWI"))%>%
 fig1datUR%>% ggplot()+
  geom_bar(aes(y = freq,
                        x=race_eth,
                        fill=race_eth),
                    stat="identity")+
  facet_wrap(~msi2,scales = "fixed" )+
  scale_fill_brewer(palette = "Accent")+
  ylab("% receiving STEM PhD")+
  labs(title = "Relative Frequency of PhD Recipients by Race/Ethnicity & Institution Type",
       subtitle = "2015 SED", caption = "Calculations by Dr. Corey S. Sparks")

ggsave("U:/Corey.Sparks.T00104//corey_fig1.png", dpi = "print", width = 10, height = 8)


## ----------------------------------------------------------------------
corey_all$fed<- ifelse(as.numeric(corey_all$edfather) %in% c(4,5,6,7), 1, 0 )
corey_all$med<- ifelse(as.numeric(corey_all$edmother) %in% c(4,5,6,7), 1, 0 )


corey_all$race_eth<- relevel (factor(corey_all$race_eth), ref = "white")


corey_all$msi2<- relevel (factor(corey_all$msi2), ref = "PWI")



## ----------------------------------------------------------------------
corey_sub<- corey_all%>%
  select(race_eth, msi2, cl,bainst, phdinst)
corey_sub$cl_b<-corey_sub$cl
corey_sub$phdinst<- as.numeric(corey_sub$phdinst)
corey_sub<-  left_join(corey_sub, cls, by =c("phdinst"= "unitid") )

#table(corey_sub$cl_b, corey_sub$cl.y)

#table(corey_sub$msi2, corey_sub$cl_b)



## ---- eval = FALSE-----------------------------------------------------
## aggregate(as.numeric(ttddoc) ~ msi2+race_eth, FUN=mean, na.rm=T, data=corey_all)
##
## aggregate(as.numeric(ttdgephd) ~ msi2+race_eth, FUN=mean, na.rm=T, data=corey_all)
##
## aggregate(as.numeric(agedoc) ~ msi2+race_eth, FUN=mean, na.rm=T, data=corey_all)
##
## aggregate(fed ~ msi+race_eth, FUN=mean, na.rm=T, data=corey_all)
##
## ((.48)/(1-.48)) / ((.45)/(1-.45))
##
## aggregate(med ~ msi+race_eth, FUN=mean, na.rm=T, data=corey_all)
##


## ---- fig.width=10, fig.height=8---------------------------------------
corey_all$yearsft<-as.numeric(corey_all$yearsft)
corey_all$tt<-as.numeric(corey_all$ttddoc)

corey_all<-corey_all%>%
  filter(race_eth %in% c("white"  ,"asian", "black",  "hisp" ))

test<-corey_all%>%
  filter(complete.cases(tt, yearsft))

fig2dat<-corey_all%>%
  group_by(msi2,race_eth,  I(gdebtlvl == "0"))%>%
  dplyr::summarise(n = n())%>%
 dplyr::mutate(freq =n/sum(n))%>%
  filter (`I(gdebtlvl == "0")`==TRUE)%>%
  filter(n >=10)

write.csv(fig2dat, file = "U:/Corey.Sparks.T00104/fig2dat.csv")

fig2datUR<-corey_all%>%
  group_by(msi2,race_eth,  I(gdebtlvl == "0"))%>%
  dplyr::summarise(n = n())%>%
 dplyr::mutate(freq =n/sum(n))%>%
  filter (`I(gdebtlvl == "0")`==TRUE)%>%
  filter(n >=10)

write.csv(fig2dat, file = "U:/Corey.Sparks.T00104//fig2datUNrounded.csv")


fig2dat%>%
  # group_by(msi2, race_eth, I(gdebtlvl == "0"))%>%
  # dplyr::summarise(n = round_any(n(), f=ceiling, 10))%>%
  # mutate(freq = round(n/sum(n), 2))%>%
  #
  # filter (`I(gdebtlvl == "0")`==TRUE&n>=10)%>%
  ggplot()+geom_bar(aes(y = freq,
                        x=race_eth,
                        fill=race_eth),
                    stat="identity")+
  facet_wrap(~msi2)+
  scale_fill_brewer(palette = "Accent")+
   ylab("% receiving STEM PhD")+
  labs(title = "Proportion Graduating with no Debt by Race/Ethnicity",
       subtitle = "Among MSI Institutions Recipients",
       caption= "Calculations by Dr. Corey S. Sparks")


  ggsave("U:/Corey.Sparks.T00104//corey_fig2.png", dpi = "print", width = 10, height = 8)


## ----------------------------------------------------------------------

corey_all<-corey_all%>%
  filter(race_eth %in% c("white"  ,"asian", "black",  "hisp" ))

#table(corey_all$cl)
m1<-glm(fed ~ msi+factor(cl)+race_eth*msi, data=corey_all, family=binomial)

#summary(m1)
#coeftest(m1, vcovCL(m1, cluster = corey_all$ipeds_inst_id, type = "HC0"))
#anova(m1, test = "Chisq")

#sjPlot::plot_model(m1)
#total time in doc degree
m20<-glm(as.numeric(ttddoc) ~scale(agedoc)+race_eth+ msi2+fed+med+sex+scale(total_rd)+scale(federal_rd), data=corey_all)

m2<-glm(as.numeric(ttddoc) ~scale(agedoc)+ msi2*race_eth+fed+med+sex+scale(total_rd)+scale(federal_rd), data=corey_all)

#summary(m2)
#coeftest(m2, vcovCL(m2, cluster = corey_all$ipeds_inst_id, type = "HC0"))
anova(m2, test = "Chisq")

#time from entry  to grad school and degree
m30<-glm(as.numeric(ttdgephd) ~ scale(agedoc)+msi2+race_eth+fed+med+sex+scale(total_rd)+scale(federal_rd), data=corey_all)

m3<-glm(as.numeric(ttdgephd) ~ scale(agedoc)+msi2+race_eth*msi2+fed+med+sex+scale(total_rd)+scale(federal_rd), data=corey_all)

#summary(m3)
#coeftest(m3, vcovCL(m3, cluster = corey_all$ipeds_inst_id, type = "HC0"))
anova(m2, test = "Chisq")


#age at degree
m40<-glm(as.numeric(agedoc) ~ msi2+race_eth+fed+med+sex+scale(total_rd)+scale(federal_rd), data=corey_all)

m4<-glm(as.numeric(agedoc) ~ msi2+race_eth*msi2+race_eth+fed+med+sex+scale(total_rd)+scale(federal_rd), data=corey_all)


#summary(m4)
#coeftest(m4, vcovCL(m4, cluster = corey_all$ipeds_inst_id, type = "HC0"))
anova(m2, test = "Chisq")

m50<- glm(I(gdebtlvl == "0")~scale(agedoc)+ msi2+race_eth+fed+med+sex+scale(total_rd)+scale(federal_rd), data=corey_all)

m5<- glm(I(gdebtlvl == "0")~scale(agedoc)+ msi2+race_eth+race_eth*msi2+race_eth+fed+med+sex+scale(total_rd)+scale(federal_rd), data=corey_all)
#summary(m5)
#coeftest(m5, vcovCL(m5, cluster = corey_all$ipeds_inst_id, type = "HC0"))
anova(m5, test = "Chisq")


## ----------------------------------------------------------------------
#m1
n1<-length(m2$residuals)
ni1<-length(unique(corey_all$pdocnid[is.na(corey_all$ttddoc)==F ]))

write.csv(data.frame(n1 = n1, ninst1 = ni1),file = "P:/tr-uncf-excelencia/Team 4_UTSA/figure3n.csv" , row.names = F)
#m2
n2<-length(m4$residuals)
ni2<-length(unique(corey_all$pdocnid[is.na(corey_all$agedoc)==F ]))
write.csv(data.frame(n2 = n2, ninst2 = ni2),file = "P:/tr-uncf-excelencia/Team 4_UTSA/figure4n.csv" , row.names = F)

#m3
n3<-length(m5$residuals)
ni3<-length(unique(corey_all$pdocnid[is.na(corey_all$gdebtlvl)==F ]))
write.csv(data.frame(n3 = n3, ninst3 = ni3),file = "P:/tr-uncf-excelencia/Team 4_UTSA/figure5n.csv" , row.names = F)



## ---- eval=FALSE-------------------------------------------------------
## library(sjPlot)
## p10<-plot_model(m20, sort.est = F, transform = NULL, axis.title = "", title = "Model 1 - Time to degree completion s", show.intercept = F ,show.p = T )
## save_plot(p10, filename="coreyfig3noint.png")
##
## p1<- plot_model(m2, sort.est = F, transform = NULL, axis.title = "", title = "Model 1 - Time to degree completion", show.intercept = F ,show.p = T )
## save_plot(p1, filename="coreyfig3.png")
##
##
## p20<-plot_model(m40, sort.est = F, transform = NULL, axis.title = "", title = "Model 2 - Age at degree completion", show.intercept = F ,show.p = T )
## p2<-plot_model(m4, sort.est = F, transform = NULL, axis.title = "", title = "Model 2 - Age at degree completion", show.intercept = F ,show.p = T )
## save_plot(p20, filename="coreyfig4noint.png")
## save_plot(p2, filename="coreyfig4.png")
##
## p30<-plot_model(m50, sort.est = F,  axis.title = "", title = "Model 3 - Graduation without Debt", show.intercept = F ,show.p = T )
## p3<-plot_model(m5, sort.est = F,  axis.title = "", title = "Model 3 - Graduation without Debt", show.intercept = F ,show.p = T )
## save_plot(p30, filename="coreyfig5noint.png")
## save_plot(p3, filename="coreyfig5.png")
##


## ---- fig.width=10, fig.height=8---------------------------------------
library(emmeans)

nd1<-ref_grid(m2)
em1<-emmeans(nd1, ~race_eth:msi2)

em1<-em1%>%as.data.frame%>%
  mutate(rndmean = round(emmean, 2),
         rndl = round(asymp.LCL, 2),rndu = round(asymp.UCL, 2))%>%
  #select (!emmean, !asymp.LCL, !asymp.UCL)%>%
  select(1,2,8,9,10)

em1%>%
  write_csv(file = "U:/Corey.Sparks.T00104//fig3est.csv")

nd2<-ref_grid(m4)
em2<-emmeans(nd2, ~race_eth:msi2)
em2<-em2%>%as.data.frame%>%
  mutate(rndmean = round(emmean, 2),
         rndl = round(asymp.LCL, 2),rndu = round(asymp.UCL, 2))%>%
  #select (!emmean, !asymp.LCL, !asymp.UCL)%>%
  select(1,2,8,9,10)
em2%>%
  write_csv(file = "U:/Corey.Sparks.T00104//fig4est.csv")


nd3<-ref_grid(m5)
em3<-emmeans(nd3, ~race_eth:msi2, type="response")
em3<-em3%>%as.data.frame%>%
  mutate(rndmean = round(response, 2),
         rndl = round(asymp.LCL, 2),rndu = round(asymp.UCL, 2))%>%
  #select (!emmean, !asymp.LCL, !asymp.UCL)%>%
  select(1,2,8,9,10)
em3%>%
  write_csv(file = "U:/Corey.Sparks.T00104/fig5est.csv")


p4<- em1%>%
  ggplot()+
  #geom_point(aes(x=paste(msi2, race_eth, sep = "-"), y=rndmean, color=msi2))+
  geom_pointrange(aes(x=paste(msi2, race_eth, sep = "-"), y=rndmean, ymin = rndl, ymax=rndu, color=msi2))+
 coord_flip()
p4<-p4+ggtitle("Estimated time to degree by MSI type")
ggsave(p4, filename = "coreyfig3.png")

p5<-em2%>%
  ggplot()+
  #geom_point(aes(x=paste(msi2, race_eth, sep = "-"), y=rndmean, color=msi2))+
  geom_pointrange(aes(x=paste(msi2, race_eth, sep = "-"), y=rndmean, ymin = rndl, ymax=rndu, color=msi2))+
 coord_flip()
p5<-p5+ggtitle("Estimated Age at degree by MSI type")
ggsave(p5, filename = "coreyfig4.png")

p6<-em3%>%
  ggplot()+
  #geom_point(aes(x=paste(msi2, race_eth, sep = "-"), y=rndmean, color=msi2))+
  geom_pointrange(aes(x=paste(msi2, race_eth, sep = "-"), y=rndmean, ymin = rndl, ymax=rndu, color=msi2))+
 coord_flip()
p6<-p6+ggtitle("Estimated Probability of having no Debt by MSI type")
ggsave(p6, filename = "coreyfig5.png")


## ----eval=FALSE, results='asis'----------------------------------------
##
## #library(stargazer)
##
## #stargazer(m1, m2, m3, m4,m5, type = "html", style = "demography",
## #          out ="P:/tr-uncf-excelencia/Team 4_UTSA/corey_regoutput.html"  )


## ---- eval=FALSE-------------------------------------------------------
##
## library(lme4)
## lm1<- glmer(as.numeric(ttddoc) ~factor(cl)+ msi2+race_eth+fed+med+sex+cut(agedoc, breaks=5)+scale(total_rd)+I(gdebtlvl=="0")+(1|ipeds_inst_id),
##             family=gaussian,
##             data=corey_all)
##
## summary(lm1)
##
##
## lm2<- glmer(as.numeric(ttdgephd) ~factor(cl)+ msi2+race_eth+fed+med+sex+cut(agedoc, breaks=5)+scale(total_rd)+(1|ipeds_inst_id),
##             family=gaussian,
##             data=corey_all)
##
## summary(lm2)
##

