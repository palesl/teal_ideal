# vizualisation

# loading in data
ip_frame <- readRDS("~/Dropbox/teal_ideal/working_data/oc_out_2d.rds")

# giving label names
ip_frame$name <- rownames(ip_frame)

#partyColours

ip_frame$party[ip_frame$party=="Liberal Party"]<-"Liberal Party of Australia"
ip_frame$party[ip_frame$party=="Liberal National Party"]<-"Liberal Party of Australia"

ip_frame$party[ip_frame$party=="National Party"]<-"National Party of Australia"

ip_frame$party[ip_frame$party=="CWM"]<-"National Party of Australia"
ip_frame$party[ip_frame$party=="SPK"]<-"Australian Labor Party"



ip_frame<- ip_frame|>  left_join(ausPH::getParties(), by=join_by(party==Name))



# plot for teals


p_teal<-ggplot(data = ip_frame, aes(coord1D,coord2D))+
  geom_point(col=ip_frame$Colour,size=2.2, alpha =0.5)+
  geom_point(shape=23,size=2.2,aes(col=teal, fill=teal, text=name))+
  scale_fill_manual(values=c("#00000000","#6bc2c3"))+
  scale_colour_manual(values=c("#00000000","black"))+
  theme_minimal()+
  ylab("'policy' dimension")+xlab('gov - opp dimension')+
  theme(legend.position = "none")  +
  ggtitle('Teals')



# tuned plot for teals plus re-elected 


p_teal_plus<-ggplot(data = ip_frame,aes(coord1D,coord2D))+
  geom_point(col=ip_frame$Colour,size=2.2, alpha = 0.5)+
  geom_point(shape=23,size=2.2,aes(col=teal_plus, fill=teal_plus, text=name))+
  scale_fill_manual(values=c("#00000000","#6bc2c3"))+
  scale_colour_manual(values=c("#00000000","black"))+
  theme_minimal()+
  theme(legend.position = "none")+
  ylab("'policy' dimension")+xlab('gov - opp dimension')+
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
ip_frame$group_teal[is.na(ip_frame$rank)]<-'Speaker'
ip_frame$group_teal[ip_frame$Abbrev=="GRN"]<-'Green'

ip_frame$group_teal[ip_frame$teal==T]<-'Teal'

ip_frame$group_teal_plus<-NA
ip_frame$group_teal_plus[ip_frame$Abbrev=="NPA"]<-'Coalition'
ip_frame$group_teal_plus[ip_frame$Abbrev=="LIB"]<-'Coalition'
ip_frame$group_teal_plus[ip_frame$Abbrev=="ALP"]<-'ALP'
ip_frame$group_teal_plus[is.na(ip_frame$rank)]<-'Speaker'
ip_frame$group_teal_plus[ip_frame$Abbrev=="GRN"]<-'Green'

ip_frame$group_teal_plus[ip_frame$teal_plus==T]<-'Teal'



chull_coalition<- ip_frame|>filter(group_teal=="Coalition")|>
  slice(chull(coord1D,coord2D))
chull_alp<- ip_frame|>filter(group_teal=="ALP")|>
  slice(chull(coord1D,coord2D))
chull_grn<- ip_frame|>filter(Abbrev=="GRN")|>
  slice(chull(coord1D,coord2D))
chull_teal<- ip_frame|>filter(group_teal=="Teal")|>
  slice(chull(coord1D,coord2D))
chull_teal_plus<- ip_frame|>filter(group_teal_plus=="Teal")|>
  slice(chull(coord1D,coord2D))

# independents minus Russell Broadbent, who was lib for most of the parliament
chull_independent<- ip_frame|>filter(name.last!="Broadbent")|>
  filter(party=="Independent"|name.last=="Katter")|>
  slice(chull(coord1D,coord2D))

# plots of the major groupings
p_teal_hull<-ggplot(data = ip_frame,aes(coord1D,coord2D))+
  geom_polygon(data=chull_coalition,alpha = 0.5,fill='#004694')+
  geom_polygon(data=chull_alp,alpha = 0.5,fill='#BB1313')+
  geom_polygon(data=chull_grn,alpha = 0.5,fill='#07A800')+
  geom_polygon(data=chull_teal,alpha = 0.5,fill='#6bc2c3')+
  geom_point(col=ip_frame$Colour,size=2.2, alpha = 0.7)+
  geom_point(shape=23,size=2.2,aes(col=teal, fill=teal, text=name))+
  scale_fill_manual(values=c("#00000000","#6bc2c3"))+
  scale_colour_manual(values=c("#00000000","black"))+
  theme_minimal()+
  ylab("'policy' dimension")+xlab('gov - opp dimension')+
  theme(legend.position = "none")+
  ggtitle("\n'Party' groupings in the House of Representatives 47th Parliament")

# plot with hulls for teals_plus and all independents
p_teal_plus_hull<-ggplot(data = ip_frame,aes(coord1D,coord2D))+
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
  ylab("'policy' dimension")+xlab('gov - opp dimension')+
  theme(legend.position = "none")+
  ggtitle("\nParties, Teals, teals plus, other independents")

ggplotly(p_teal_hull, tooltip = "name")

ggplotly(p_teal_plus_hull, tooltip = "name")



