# 04_stretch_vis.R


source("R/03_visualising.R")

# trying

xfactor<-1
yfactor<-1



# stretch x
divisions_dat$x_ext <- divisions_dat$x * xfactor
divisions_dat$xend_ext <- divisions_dat$xend * xfactor
ip_frame$coord1D_new <- ip_frame$coord1D * xfactor

# stretch y

divisions_dat$y_ext <- divisions_dat$y * yfactor
divisions_dat$yend_ext <- divisions_dat$yend * yfactor
ip_frame$coord2D_new <- ip_frame$coord2D * yfactor




ggplot(divisions_dat)+geom_segment(aes(x=x_ext,
                                       y=y_ext,
                                       xend=xend_ext,
                                       yend=yend_ext),color='grey', alpha=0.3)+
  geom_point(data=ip_frame, aes(coord1D_new,coord2D_new), col='blue')+
  # scale_x_continuous(limits = c(-1,1))+
  # scale_y_continuous(limits = c(-1,1))+
  annotate("path",
           x=cos(seq(0,2*pi,length.out=100)),
           y=sin(seq(0,2*pi,length.out=100)))




# replotting segments to the unit circle


dx <- divisions_dat$xend_ext - divisions_dat$x_ext
dy <- divisions_dat$yend_ext - divisions_dat$y_ext

a <- dx^2 + dy^2
b <- 2 * (divisions_dat$x_ext * dx + divisions_dat$y_ext * dy)
c <- divisions_dat$x_ext^2 + divisions_dat$y_ext^2 - 1

discriminant <- b^2 - 4 * a * c
t1 <- (-b - sqrt(discriminant)) / (2 * a)
t2 <- (-b + sqrt(discriminant)) / (2 * a)

# Projected endpoints
divisions_dat$x_new <- divisions_dat$x_ext + t1 * dx
divisions_dat$y_new <- divisions_dat$y_ext + t1 * dy
divisions_dat$xend_new <- divisions_dat$x_ext + t2 * dx
divisions_dat$yend_new <- divisions_dat$y_ext + t2 * dy



ggplot(divisions_dat)+
  geom_segment(aes(x=x_new,
                   y=y_new,
                   xend=xend_new,
                   yend=yend_new),color='grey', alpha=0.3)+
  # geom_segment(aes(x=x_ext,
  #                  y=y_ext,
  #                  xend=xend_ext,
  #                  yend=yend_ext),color='blue', alpha=0.3)+
  geom_point(data=ip_frame, aes(coord1D_new,coord2D_new), col='red')+
  scale_x_continuous(limits = c(-1,1))+
  scale_y_continuous(limits = c(-1,1))+
  annotate("path",
           x=cos(seq(0,2*pi,length.out=100)),
           y=sin(seq(0,2*pi,length.out=100)))+
  coord_fixed()+theme_void()



# outputs


divisions_dat<-divisions_dat|>
  select(-(x_ext:yend_ext))|>
  select(-(x :yend ))

ip_frame<-ip_frame|>
  select(-(coord1D:coord2D))



write_csv(divisions_dat, file="working_data/dividingLinesNew.csv")
write_csv(ip_frame, file="working_data/pointsNew.csv")


