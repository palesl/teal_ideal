# Purpose of this code. To run and evaluate fit for some ideal point models
# making use of:
# Armstrong, David A., et al.2020.
# Analyzing Spatial Models of Choice and Judgment. 2nd ed.
# New York: Chapman and Hall/CRC. doi:10.1201/9781315197609.

set.seed(1234)

dat<-readRDS('working_data/house_votes.rds')
divisions<-readRDS( 'working_data/divisions.rds')
library(pscl);library(tidyverse);library(oc);library(politicsR)



# rice scores.

rice_dat<-cbind(dat[,1:6],apply(dat[7:ncol(dat)],2,as.character))


rice_dat[rice_dat=='1']<-'Yay'
rice_dat[rice_dat=='2']<-'Nay'
rice_dat[rice_dat=='5']<-NA
rice_dat[rice_dat=='9']<-NA

teal_rice<- rice_dat|>filter(teal==T)|>
  select(7:ncol(rice_dat))|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ rice(.x)))|>
  rowMeans()

teal_splits<-rice_dat|>filter(teal==T)|>
  select(7:ncol(rice_dat))|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ rice(.x)))|>
  t() |>as.data.frame()

teal_splits$division_name<-rownames(teal_splits)
names(teal_splits)[1]<-"Rice_teal"

divisions$division_name<-paste0("div",divisions$divisionNumber,"_",
                                as.Date(divisions$date)|>format("%d%b%Y"))

divisions<-divisions|>left_join(teal_splits)

divisions$url<-paste0("https://www.aph.gov.au/Parliamentary_Business/Chamber_documents/HoR/Divisions/Details?id=",
                      divisions$divisionId)




teal_plus_rice<- rice_dat|>filter(teal_plus==T)|>
  select(7:ncol(rice_dat))|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ rice(.x)))|>
  rowMeans()


teal_plus_splits<-rice_dat|>filter(teal_plus==T)|>
  select(7:ncol(rice_dat))|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ rice(.x)))|>
  t() |>as.data.frame()

teal_plus_splits$division_name<-rownames(teal_plus_splits)


names(teal_plus_splits)[1]<-"Rice_teal_plus"

divisions<-divisions|>left_join(teal_plus_splits)
divisions$date<-as.Date(divisions$date)
ggplot(data=divisions, aes(date, Rice_teal))+geom_smooth()+geom_point()
teal_split_table<-divisions|>filter(Rice_teal<1)|>
  select(title, question)|>
  mutate(division_type=case_when(grepl("Fair Work",title)~"Bills, Fair Work Legislation",
                                 grepl("Bill",title)~"Bills, other",
                                 grepl("Suspension of", title)~"Suspension of Standing Orders, other procedure",
                                 grepl("Hamas",title)~"Israel Gaza Conflict",
                                 grepl("Two-state",title)~"Israel Gaza Conflict",
                                 .default = "Motions, other")
  )|>group_by(division_type)|>
  summarise(n=n(),
            teals =paste0(n()," (",round(100*n()/nrow(divisions|>filter(Rice_teal_plus<1)),1),"%)")
  )|>arrange(-n)|>select(-n)

teal_plus_split_table<-divisions|>filter(Rice_teal_plus<1)|>
  select(title, question)|>
  mutate(division_type=case_when(grepl("Fair Work",title)~"Bills, Fair Work Legislation",
                                 grepl("Bill",title)~"Bills, other",
                                 grepl("Suspension of", title)~"Suspension of Standing Orders, other procedure",
                                 grepl("Hamas",title)~"Israel Gaza Conflict",
                                 grepl("Two-state",title)~"Israel Gaza Conflict",
                                 .default = "Motions, other")
  )|>group_by(division_type)|>
  summarise(n=n(),
            teals_plus=paste0(n()," (",round(100*n()/nrow(divisions|>filter(Rice_teal_plus<1)),1),"%)")
            )|>arrange(-n)|>select(-n)

split_tab<-teal_split_table|>left_join(teal_plus_split_table)

alp_rice<-rice_dat|>filter(partyName=="Australian Labor Party")|>
  select(7:ncol(rice_dat))|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ rice(.x)))|>
  rowMeans()

lib_rice<-rice_dat|>filter(partyName=="Liberal Party of Australia"|
                             partyName=="Liberal National Party of Queensland")|>
  select(7:ncol(rice_dat))|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ rice(.x)))|>
  rowMeans()

nat_rice<-rice_dat|>filter(partyName=="The Nationals")|>
  select(7:ncol(rice_dat))|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ rice(.x)))|>
  rowMeans()

green_rice <-rice_dat|>filter(partyName=="Australian Greens")|>
  select(7:ncol(rice_dat))|>
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
  y<-length(x[x==1])
  n<-length(x[x==2])
  a<-length(x[x==5])

  out<- (max(y,n,a) - 0.5*((y+n+a)-max(y,n,a)))/(y+n+a)

  return(out)
}

teal_ai<- dat|>filter(teal==T)|>
  select(7:ncol(rice_dat))|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ ai(.x)))|>
  rowMeans()

teal_plus_ai<- dat|>filter(teal_plus==T)|>
  select(7:ncol(rice_dat))|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ ai(.x)))|>
  rowMeans()

alp_ai<-dat|>filter(partyName=="Australian Labor Party")|>
  select(7:ncol(rice_dat))|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ ai(.x)))|>
  rowMeans()

lib_ai<-dat|>filter(partyName=="Liberal Party of Australia"|
                      partyName=="Liberal National Party of Queensland")|>
  select(7:ncol(rice_dat))|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ ai(.x)))|>
  rowMeans()

nat_ai<-dat|>filter(partyName=="The Nationals")|>
  select(7:ncol(rice_dat))|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ ai(.x)))|>
  rowMeans()

green_ai <-dat|>filter(partyName=="Australian Greens")|>
  select(7:ncol(rice_dat))|>
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

rc <- rollcall(data=dat[,7:ncol(dat)],
               yea=1,
               nay=2,
               missing	= 5,
               notInLegis = 9,
               legis.names=dat$DisplayName,
               vote.names=names(dat[,7:ncol(dat)]),
               legis.data=dat[,1:6],
               vote.data=NULL,
               desc="47th Parliament of the Australian House of Representatives")


dutton_no<- grep('DUTTON,',dat$DisplayName)

oc_teal_1dim <- oc(rc, dims=1, minvotes=25, lop=0.05,
                   polarity=c(dutton_no), verbose=T)

oc_teal_2dim <- oc(rc, dims=2, minvotes=25, lop=0.05,
                   polarity=c(dutton_no,dutton_no), verbose=T)

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

cutlines <- oc_teal_2dim$rollcalls




# output data


ip_frame <- oc_teal_2dim[["legislators"]]


saveRDS(ip_frame, 'working_data/oc_out_2d.rds')
saveRDS(cutlines, 'working_data/oc_cutlines_2d.rds')
