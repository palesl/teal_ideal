dat<-readRDS('working_data/house_votes.rds')
library(MCMCpack);library(pscl);library(tidyverse)



model_mat <- dat|>dplyr::select(starts_with('div'))|>as.matrix()

model_mat[model_mat=="aye"] <- 1
model_mat[model_mat=="no"] <- 0
rownames(model_mat)<-paste(dat$name.first,dat$name.last,sep = "_")



#cleaning matrix

rownames(model_mat) <- gsub("\\-","",rownames(model_mat))





# now with some model tuning, including only MPs who have voted more than 50 times
# this removes milton dick, who is the Speaker

cutoff <- 50
model_mat <- model_mat[rowSums(!is.na(model_mat)) >= cutoff,]

head(sort(rowSums(!is.na(model_mat))))

posterior2d <- MCMCirtKd(model_mat, dimensions=2,
                         mcmc=15000, burnin=5000, thin=3,
                         theta.start=NA, alpha.start=NA, beta.start=NA,
                         t0=0, T0=1, a0=0, A0=0.25, b0=0, B0=0.25,
                         seed=NA, verbose=0, store.item=FALSE,
                         store.ability=TRUE, drop.constant.items=TRUE)

idealpt1 <- colMeans(posterior2d[,seq(1, ncol(posterior2d), by=2)])
idealpt2 <- colMeans(posterior2d[,seq(2, ncol(posterior2d), by=2)])

# diagnostics for the tuned model...

plot(posterior2d)

# vizualisation

# post run df

ip_frame<- bind_cols(dat|>dplyr::select(person_id:teal_plus)|>filter(name.last!="Dick"), 
                     idealpt1=idealpt1, idealpt2=idealpt2)

#partyColours

ip_frame$party[ip_frame$party=="Liberal Party"]<-"Liberal Party of Australia"
ip_frame$party[ip_frame$party=="Liberal National Party"]<-"Liberal Party of Australia"

ip_frame$party[ip_frame$party=="National Party"]<-"National Party of Australia"

ip_frame$party[ip_frame$party=="CWM"]<-"National Party of Australia"
ip_frame$party[ip_frame$party=="SPK"]<-"Australian Labor Party"



ip_frame<- ip_frame|>  left_join(ausPH::getParties(), by=join_by(party==Name))


# giving label names

ip_frame$name<-paste0(ip_frame$name.first," ",ip_frame$name.last,
                            " (", ip_frame$party, ")")

# tuned plot for teals


p_teal<-ggplot(data = ip_frame, aes(-idealpt2,idealpt1))+
  geom_point(col=ip_frame$Colour,size=2.2, alpha =0.5)+
  geom_point(shape=23,size=2.2,aes(col=teal, fill=teal, text=name))+
  scale_fill_manual(values=c("#00000000","#6bc2c3"))+
  scale_colour_manual(values=c("#00000000","black"))+
  theme_minimal()+
  ylab('progressive dimension')+xlab('gov - opp dimension')+
  theme(legend.position = "none")  +
  ggtitle('Teals')



# tuned plot for teals plus re-elected 


p_teal_plus<-ggplot(data = ip_frame,aes(-idealpt2,idealpt1))+
  geom_point(col=ip_frame$Colour,size=2.2, alpha = 0.5)+
  geom_point(shape=23,size=2.2,aes(col=teal_plus, fill=teal_plus, text=name))+
  scale_fill_manual(values=c("#00000000","#6bc2c3"))+
  scale_colour_manual(values=c("#00000000","black"))+
  theme_minimal()+
  ylab('progressive dimension')+xlab('gov - opp dimension')+
  theme(legend.position = "none")+
  ggtitle('Teals plus re-elected independents')

gridExtra::grid.arrange(p_teal,p_teal_plus, ncol=2)

# plotly

library(plotly)
plotly_teal<-ggplotly(p_teal, tooltip = "name")

plotly_teal_plus<-ggplotly(p_teal_plus, tooltip = "name")

# convex hulls...

ip_frame$group_teal<-NA
ip_frame$group_teal[ip_frame$Abbrev=="NPA"]<-'Coalition'
ip_frame$group_teal[ip_frame$Abbrev=="LIB"]<-'Coalition'
ip_frame$group_teal[ip_frame$Abbrev=="ALP"]<-'ALP'
ip_frame$group_teal[ip_frame$Abbrev=="GRN"]<-'Green'

ip_frame$group_teal[ip_frame$teal==T]<-'Teal'

ip_frame$group_teal_plus<-NA
ip_frame$group_teal_plus[ip_frame$Abbrev=="NPA"]<-'Coalition'
ip_frame$group_teal_plus[ip_frame$Abbrev=="LIB"]<-'Coalition'
ip_frame$group_teal_plus[ip_frame$Abbrev=="ALP"]<-'ALP'
ip_frame$group_teal_plus[ip_frame$Abbrev=="GRN"]<-'Green'

ip_frame$group_teal_plus[ip_frame$teal_plus==T]<-'Teal'



chull_coalition<- ip_frame|>filter(group_teal=="Coalition")|>
  slice(chull(-idealpt2,idealpt1))
chull_alp<- ip_frame|>filter(group_teal=="ALP")|>
  slice(chull(-idealpt2,idealpt1))
chull_grn<- ip_frame|>filter(Abbrev=="GRN")|>
  slice(chull(-idealpt2,idealpt1))
chull_teal<- ip_frame|>filter(group_teal=="Teal")|>
  slice(chull(-idealpt2,idealpt1))
chull_teal_plus<- ip_frame|>filter(group_teal_plus=="Teal")|>
  slice(chull(-idealpt2,idealpt1))

# independents minus Russell Broadbent, who was lib for most of the parliament
chull_independent<- ip_frame|>filter(name.last!="Broadbent")|>
  filter(party=="Independent"|name.last=="Katter")|>
  slice(chull(-idealpt2,idealpt1))


# plots of the major groupings
p_teal_hull<-ggplot(data = ip_frame,aes(-idealpt2,idealpt1))+
  geom_polygon(data=chull_coalition,alpha = 0.5,fill='#004694')+
  geom_polygon(data=chull_alp,alpha = 0.5,fill='#BB1313')+
  geom_polygon(data=chull_grn,alpha = 0.5,fill='#07A800')+
  geom_polygon(data=chull_teal,alpha = 0.5,fill='#6bc2c3')+
  geom_point(col=ip_frame$Colour,size=2.2, alpha = 0.7)+
  geom_point(shape=23,size=2.2,aes(col=teal, fill=teal, text=name))+
  scale_fill_manual(values=c("#00000000","#6bc2c3"))+
  scale_colour_manual(values=c("#00000000","black"))+
  theme_minimal()+
  ylab('progressive dimension')+xlab('gov - opp dimension')+
  theme(legend.position = "none")+
  ggtitle("Convex hulls of 'party' groupings")

# plot with hulls for teals_plus and all independents
p_teal_plus_hull<-ggplot(data = ip_frame,aes(-idealpt2,idealpt1))+
  geom_polygon(data=chull_coalition,alpha = 0.5,fill='#004694')+
  geom_polygon(data=chull_alp,alpha = 0.5,fill='#BB1313')+
  geom_polygon(data=chull_grn,alpha = 0.5,fill='#07A800')+
  geom_polygon(data=chull_independent,alpha = 0.3,fill='#9E18DF')+
  geom_polygon(data=chull_teal_plus,lty=2,alpha = 1,fill='#6bc2c3',col="black")+
  geom_polygon(data=chull_teal,alpha = 1,fill='#6bc2c3',col="black")+
  geom_point(col=ip_frame$Colour,size=2.2, alpha = 0.7)+
  geom_point(shape=23,size=2.2,aes(col=teal, fill=teal, text=name))+
  scale_fill_manual(values=c("#00000000","#6bc2c3"))+
  scale_colour_manual(values=c("#00000000","black"))+
  theme_minimal()+
  ylab('progressive dimension')+xlab('gov - opp dimension')+
  theme(legend.position = "none")+
  ggtitle('Teals, teals plus, other independents')
  
  
  

ggplotly(p_teal_hull, tooltip = "name")

ggplotly(p_teal_plus_hull, tooltip = "name")


