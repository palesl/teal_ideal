# vizualisation
library(tidyverse);library(plotly)

# loading in data
ip_frame <- readRDS("~/Dropbox/teal_ideal/working_data/oc_out_2d.rds")

# giving label names
ip_frame$name <- paste(ip_frame$DisplayName,ip_frame$partyName,sep="\n")

#party data

ip_frame<-ip_frame|>left_join(ausPH::getParties()|>select(Name,Abbrev), by=join_by('partyName'=="Name"))

# plot for teals
p_teal<-ggplot(data = ip_frame, aes(coord1D,coord2D))+
  geom_point(col=ip_frame$partyColour,size=2.2, alpha =0.5)+
  geom_point(shape=23,size=2.2,aes(col=teal, fill=teal, text=name))+
  scale_fill_manual(values=c("#00000000","#6bc2c3"))+
  scale_colour_manual(values=c("#00000000","black"))+
  theme_minimal()+
  ylab("alternative policy")+xlab('gov - opp policy')+
  theme(legend.position = "none")  +
  ggtitle('Teals')



# tuned plot for teals plus re-elected


p_teal_plus<-ggplot(data = ip_frame,aes(coord1D,coord2D))+
  geom_point(col=ip_frame$partyColour,size=2.2, alpha = 0.5)+
  geom_point(shape=23,size=2.2,aes(col=teal_plus, fill=teal_plus, text=name))+
  scale_fill_manual(values=c("#00000000","#6bc2c3"))+
  scale_colour_manual(values=c("#00000000","black"))+
  theme_minimal()+
  theme(legend.position = "none")+
  ylab("alternative policy")+xlab('gov - opp policy')+
  ggtitle('Teals plus re-elected independents')

gridExtra::grid.arrange(p_teal,p_teal_plus, ncol=2)

# plotly

library(plotly)
plotly_teal<-ggplotly(p_teal, tooltip = "name")

plotly_teal_plus<-ggplotly(p_teal_plus, tooltip = "name")

# convex hulls...

ip_frame$group_teal<-NA
ip_frame$group_teal[ip_frame$Abbrev=="NP "]<-'Coalition'
ip_frame$group_teal[ip_frame$Abbrev=="LIB"]<-'Coalition'
ip_frame$group_teal[ip_frame$partyName=="Liberal National Party of Queensland"]<-"Coalition"
ip_frame$group_teal[ip_frame$Abbrev=="ALP"]<-'ALP'
ip_frame$group_teal[ip_frame$Abbrev=="GRN"]<-'Green'

ip_frame$group_teal[ip_frame$teal==T]<-'Teal'

ip_frame$group_teal_plus<-NA
ip_frame$group_teal_plus[ip_frame$Abbrev=="NP "]<-'Coalition'
ip_frame$group_teal_plus[ip_frame$Abbrev=="LIB"]<-'Coalition'
ip_frame$group_teal_plus[ip_frame$partyName=="Liberal National Party of Queensland"]<-"Coalition"
ip_frame$group_teal_plus[ip_frame$Abbrev=="ALP"]<-'ALP'
ip_frame$group_teal_plus[ip_frame$Abbrev=="GRN"]<-'Green'

ip_frame$group_teal_plus[ip_frame$teal_plus==T]<-'Teal'



chull_coalition<- ip_frame|>filter(group_teal=="Coalition", !is.na(coord1D))|>
  slice(chull(coord1D,coord2D))
chull_alp<- ip_frame|>filter(group_teal=="ALP",!is.na(coord1D))|>
  slice(chull(coord1D,coord2D))
chull_grn<- ip_frame|>filter(Abbrev=="GRN")|>
  slice(chull(coord1D,coord2D))
chull_teal<- ip_frame|>filter(group_teal=="Teal")|>
  slice(chull(coord1D,coord2D))
chull_teal_plus<- ip_frame|>filter(group_teal_plus=="Teal")|>
  slice(chull(coord1D,coord2D))

# independents minus Russell Broadbent, who was lib for most of the parliament
chull_independent<- ip_frame|>filter(!grepl("BROADBENT,", DisplayName))|>
  filter(partyName=="Independent"|grepl("KATTER,",DisplayName))|>
  slice(chull(coord1D,coord2D))

# plots of the major groupings
p_teal_hull<-ggplot(data = ip_frame,aes(coord1D,coord2D))+
  geom_polygon(data=chull_coalition,alpha = 0.5,fill='#004694')+
  geom_polygon(data=chull_alp,alpha = 0.5,fill='#BB1313')+
  geom_polygon(data=chull_grn,alpha = 0.5,fill='#07A800')+
  geom_polygon(data=chull_teal,alpha = 0.5,fill='#6bc2c3')+
  geom_point(col=ip_frame$partyColour,size=2.2, alpha = 0.7)+
  geom_point(shape=23,size=2.2,aes(col=teal, fill=teal, text=name))+
  scale_fill_manual(values=c("#00000000","#6bc2c3"))+
  scale_colour_manual(values=c("#00000000","black"))+
  theme_minimal()+
  ylab("alternative policy")+xlab('gov - opp policy')+
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
  geom_point(col=ip_frame$partyColour,size=2.2, alpha = 0.7)+
  geom_point(shape=23,size=2.2,aes(col=teal, fill=teal, text=name))+
  scale_fill_manual(values=c("#00000000","#6bc2c3"))+
  scale_colour_manual(values=c("#00000000","black"))+
  theme_minimal()+
  ylab("alternative policy")+xlab('gov - opp policy')+
  theme(legend.position = "none")+
  xlim(-1,1)+ylim(-1,1)



ggplotly(p_teal_plus_hull, tooltip = "name")



## working out the meaning of the dimensions...

# extracting cutlines...
source("R/00_helpers.R")
oc_cutlines_2d <- readRDS("working_data/oc_cutlines_2d.rds")[,6:8]

cutline_coords<-apply(oc_cutlines_2d,1,cutline)|>bind_rows()

# load divisions dat and sort to make sure it matches the rc data
divisions <- readRDS("working_data/divisions.rds")
divisions<- divisions|>arrange(as.numeric(divisionNumber))
divisions_dat<-cbind(divisions, cutline_coords)


# calculating turnout

divisions_dat$turnout<-(as.numeric(divisions_dat$ayes)+
                          as.numeric(divisions_dat$noes))
# slope
divisions_dat$slope<-(divisions_dat$yend-divisions_dat$y)/
  (divisions_dat$xend-divisions_dat$x)

#absolute value of the cutting angle (0-90)
divisions_dat$abs_angle<-abs(atan(divisions_dat$slope)*180/pi)

# whether the proposition was negatived
divisions_dat$negatived<-as.numeric(divisions_dat$noes) >as.numeric(divisions_dat$ayes)
divisions_dat$negatived[divisions_dat$negatived==F]<-"Agreed to"
divisions_dat$negatived[divisions_dat$negatived==T]<-"Negatived"

# title of the division made easier to read
divisions_dat$title<-stringr::str_wrap(divisions_dat$title, width = 40)

# bringing in the party of the introducer...

party_mp<-dat|>select(parliamentarianId,partyName)|>
  group_by(parliamentarianId)|>
  summarise(partyName=paste(partyName, collapse = "/"))

divisions_dat<-divisions_dat|>
  left_join(party_mp, by=join_by("mover.parliamentarianId"=="parliamentarianId"))

divisions_dat$introducer_lab<-paste0(divisions_dat$mover.name,", ", divisions_dat$partyName)

divisions_dat$introducer_lab<-stringr::str_wrap(divisions_dat$introducer_lab, width = 40)

# all together

divisions_dat$name<-paste0(divisions_dat$title,"\nID: ",divisions_dat$divisionId,"\nDate: ",
                           lubridate::date(divisions_dat$date) ,"\nIntroduced by: ",
                           divisions_dat$introducer_lab, "\nResult: ",
                           divisions_dat$negatived,". Ayes: ",
                           divisions_dat$ayes, ". Noes: ",
                           divisions_dat$noes,
                           '\nTurnout: ',
                           divisions_dat$turnout)




# plotting




angle_v_turnout<-ggplot(data=divisions_dat,
                        aes(turnout,abs_angle, text=name))+
  geom_point(alpha=.5)+theme_minimal()+
  scale_y_continuous(breaks = c(15,30,45,60,75,90))+
  ylab("alternative←|cutpoint angle|→gov-opp")

angle_v_turnout_plotly<-ggplotly(angle_v_turnout, tooltip = "name")


divisions_dat_hl <- highlight_key(divisions_dat, ~name)

p_cutlines<-ggplot(data = ip_frame, aes(coord1D,coord2D))+
  geom_polygon(data=chull_coalition,alpha = 0.5,fill='#004694')+
  geom_polygon(data=chull_alp,alpha = 0.5,fill='#BB1313')+
  geom_polygon(data=chull_grn,alpha = 0.5,fill='#07A800')+
  geom_polygon(data=chull_teal_plus,lty=2,alpha = 1,fill='#6bc2c3',col="black")+
  geom_polygon(data=chull_teal,alpha = 1,fill='#6bc2c3',col="black")+
  geom_segment(data=divisions_dat_hl,
               aes(x=x,y=y,xend=xend,yend=yend, text=name),
               alpha=0.3, linewidth=0.2, col='grey')+
  geom_point(col=ip_frame$partyColour,size=2.2, alpha =0.5)+
  geom_point(shape=23,size=2.2,aes(col=teal, fill=teal, text=name))+
  scale_fill_manual(values=c("#00000000","#6bc2c3"))+
  scale_colour_manual(values=c("#00000000","black"))+
  theme_minimal()+
  ylab("minor party ←| dimension 2 |→ major party")+xlab('government ←| dimension 1 |→ opposition')+
  theme(legend.position = "none")+
  annotate("path",
           x=cos(seq(0,2*pi,length.out=100)),
           y=sin(seq(0,2*pi,length.out=100)))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

p_cutline_plotly<-ggplotly(p_cutlines, tooltip = "name")|>
  plotly::highlight(on = "plotly_hover", off = "plotly_deselect", color = '#FF0000')|>
  layout(hovermode = 'closest', hoverlabel=list(bgcolor='rgba(0,0,0,.1)',
                                                font=list(size=10)
                                                ))

p_cutline_plotly

# little regression explaining cutline angle loading to dimension 1

lm(abs_angle~partyName + turnout +negatived,data=divisions_dat)|>summary()




cor.test(divisions_dat$abs_angle,divisions_dat$turnout)

plot.OCcoords(oc_teal_2dim, plotBy = 'partyName')

