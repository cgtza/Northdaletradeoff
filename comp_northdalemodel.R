data<-read.csv("northdalesurveythreatdata.csv",
               header = TRUE)

attach(data)

df1<-data.frame(Cnt_qnt_B,Cnt_qnt_IS)
df2<-data.frame(Cnt_neg_IS1,Cnt_neg_IS2,Cnt_neg_IS3,Cnt_neg_B)
df3<-data.frame(Cnt_pos_B,Cnt_pos_IS1)
df4<-data.frame(Cnt_Qul_IS1,Cnt_Qul_IS2,Cnt_Qul_IS3)
df5<-data.frame(Empath1,Empath2,Empath3)




cor(df1,use="pairwise.complete.obs")
cor(df2,use="pairwise.complete.obs")
cor(df3,use="pairwise.complete.obs")
cor(df4,use="pairwise.complete.obs")
cor(df5,use="pairwise.complete.obs")
cor(df5,df4,use="pairwise.complete.obs")

closeness<-(crowflies_address_infstlmnt)/1000 + Cnt_IS_see + 
    Cnt_IS_smell+ Cnt_IS_hear

#   Threat
threatphy<-Thrt_Phy1 + Thrt_Phy2 + Thrt_Phy3

#   Empathy
empathy<-Empath1 + Empath2 + Empath3

#   Prejudice
prejudice<-Att_IS1  + Att_IS2 + Att_IS4

#   contact_quant
contactquant<-Cnt_pos_B+Cnt_pos_IS1

#   Contact_pos
contactpos<-Cnt_Qul_IS1+Cnt_Qul_IS2+Cnt_Qul_IS3

#   contactavoid
df2<-data.frame(cnt_avd1,cnt_avd2,cnt_avd3,cnt_avd4)
pairs.panels(df2)
contactavoid<-cnt_avd1+cnt_avd2+cnt_avd3+cnt_avd4

df<-data.frame(closeness,empathy,prejudice,contactpos,threatphy,contactavoid)

interacmod1<-lm(prejudice~closeness*contactpos+threatphy*closeness+empathy)
interacmod1<-lm(prejudice~threatphy+contactquant+contactavoid)

summary(interacmod1)
library(psych)
pairs.panels(df)
hist(log(crowflies_address_infstlmnt))
hist(closeness)
detach(data)
describe(closeness)
closeness
hist(contactavoid)
