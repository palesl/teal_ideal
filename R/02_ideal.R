# Purpose of this code. To run and evaluate fit for some ideal point models
# making use of:
# Armstrong, David A., et al.2020.
# Analyzing Spatial Models of Choice and Judgment. 2nd ed.
# New York: Chapman and Hall/CRC. doi:10.1201/9781315197609.

set.seed(123)

dat<-readRDS('working_data/house_votes.rds')
library(pscl);library(tidyverse);library(oc);library(politicsR)



# rice scores.

rice_dat<-dat
rice_dat[rice_dat=="aye"]<-'Yay'
rice_dat[rice_dat=="no"]<-'Nay'
rice_dat[rice_dat=="abstain"]<-NA
rice_dat[rice_dat=="out"]<-NA

teal_rice<- rice_dat|>filter(teal==T)|>
  select(9:282)|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ rice(.x)))|>
  rowMeans()

teal_plus_rice<- rice_dat|>filter(teal_plus==T)|>
  select(9:282)|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ rice(.x)))|>
  rowMeans()

alp_rice<-rice_dat|>filter(party=="Australian Labor Party")|>
  select(9:282)|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ rice(.x)))|>
  rowMeans()

lib_rice<-rice_dat|>filter(party=="Liberal Party"|
                             party=="Liberal National Party")|>
  select(9:282)|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ rice(.x)))|>
  rowMeans()

nat_rice<-rice_dat|>filter(party=="National Party")|>
  select(9:282)|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ rice(.x)))|>
  rowMeans()

green_rice <-rice_dat|>filter(party=="Australian Greens")|>
  select(9:282)|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ rice(.x)))|>
  rowMeans()

rice_scores <- bind_rows(teal=teal_rice,
                        teal_plus=teal_plus_rice,
                        alp=alp_rice,
                        lib=lib_rice,
                        nat=nat_rice,
                        green=green_rice)


# agreement index scores (Hix, Noury and Roland)
# (max{Yi, Ni, Ai} - 1/2*[(Yi + Ni + Ai) - max{Yi, Ni, Ai}] )/(Yi + Ni + Ai)


ai<- function(x){
  y<-length(x[x=='aye'])
  n<-length(x[x=='no'])
  a<-length(x[x=='abstain'])

  out<- (max(y,n,a) - 0.5*((y+n+a)-max(y,n,a)))/(y+n+a)

  return(out)
}

teal_ai<- dat|>filter(teal==T)|>
  select(9:282)|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ ai(.x)))|>
  rowMeans()

teal_plus_ai<- dat|>filter(teal_plus==T)|>
  select(9:282)|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ ai(.x)))|>
  rowMeans()

alp_ai<-dat|>filter(party=="Australian Labor Party")|>
  select(9:282)|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ ai(.x)))|>
  rowMeans()

lib_ai<-dat|>filter(party=="Liberal Party"|
                      party=="Liberal National Party")|>
  select(9:282)|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ ai(.x)))|>
  rowMeans()

nat_ai<-dat|>filter(party=="National Party")|>
  select(9:282)|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ ai(.x)))|>
  rowMeans()

green_ai <-dat|>filter(party=="Australian Greens")|>
  select(9:282)|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ ai(.x)))|>
  rowMeans()

ai_scores <- bind_rows(teal=teal_ai,
                       teal_plus=teal_plus_ai,
                       alp=alp_ai,
                       lib=lib_ai,
                       nat=nat_ai,
                       green=green_ai)

# model_mat <- dat|>dplyr::select(starts_with('div'))|>as.matrix()

# model_mat[model_mat=="aye"] <- 1
# model_mat[model_mat=="no"] <- 0
# rownames(model_mat)<-paste(dat$name.first,dat$name.last,sep = "_")
#
#
# #cleaning matrix
#
# rownames(model_mat) <- gsub("\\-","",rownames(model_mat))
#

# first with bayesian IRT...

# now with some model tuning, including only MPs who have voted more than 50 times
# this removes milton dick, who is the Speaker

# cutoff <- 50
# model_mat <- model_mat[rowSums(!is.na(model_mat)) >= cutoff,]
#
# head(sort(rowSums(!is.na(model_mat))))
#
# posterior2d <- MCMCirtKd(model_mat, dimensions=2,
#                          mcmc=15000, burnin=15000, thin=3,
#                          theta.start=NA, alpha.start=NA, beta.start=NA,
#                          t0=0, T0=1, a0=0, A0=0.25, b0=0, B0=0.25,
#                          seed=NA, verbose=0, store.item=FALSE,
#                          store.ability=TRUE, drop.constant.items=TRUE)
#
#
# idealpt1 <- colMeans(posterior2d[,seq(1, ncol(posterior2d), by=2)])
# idealpt2 <- colMeans(posterior2d[,seq(2, ncol(posterior2d), by=2)])
#
#
# # diagnostics for the tuned model...
#
# plot(posterior2d)
#
#
# theta1<-posterior2d[,seq(1, ncol(posterior2d), by=2)]
# theta2<-posterior2d[,seq(2, ncol(posterior2d), by=2)]
#
# sort(abs(geweke.diag(theta1)$z), decreasing=T)
# sort(abs(geweke.diag(theta2)$z), decreasing=T)
#
# geweke<-as.mcmc(theta1[,"theta.Joanne_Ryan.1"])

# the diagnostics fail pretty badly here to produce something that looks stable


# Now try with optimal classification

library(oc)

rc <- rollcall(data=dat[,9:ncol(dat)],
               yea="aye",
               nay="no",
               missing	= 'abstain',
               notInLegis = 'out',
               legis.names=paste0(dat$name.first," ",dat$name.last,
                                  " (", dat$party, ")"),
               vote.names=colnames(dat[,9:ncol(dat)]),
               legis.data=dat[,1:8],
               vote.data=NULL,
               desc="47th Parliament of the Australian House of Representatives")



oc_teal_1dim <- oc(rc, dims=1, minvotes=50, lop=0.05,
                   polarity=c(102), verbose=T)

oc_teal_2dim <- oc(rc, dims=2, minvotes=50, lop=0.05,
              polarity=c(102,102), verbose=T)

# checking fits

summary(oc_teal_2dim)
summary(oc_teal_1dim)

fits <- cbind(oc_teal_1dim$fits, oc_teal_2dim$fits)

colnames(fits) <- c("1 Dim", "2 Dim")
rownames(fits) <- c("% Correct", "APRE")

fits

# clearly we have a winner in terms of fit, both average proportional
# reduction of error and %correct are excellent

# how about the cutlines...

cutlines <-oc_teal_2dim$rollcalls




# output data


ip_frame <- oc_teal_2dim[["legislators"]]


saveRDS(ip_frame, 'working_data/oc_out_2d.rds')
saveRDS(cutlines, 'working_data/oc_cutlines_2d.rds')
