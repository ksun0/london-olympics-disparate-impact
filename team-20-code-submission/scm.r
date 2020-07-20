library(readr)
library(glmnet)
library(dplyr)
library(purrr)
library(compare)

gdpz = read_csv('gdp_with_international_cities.csv')
london_br = list('barking dagenham',
  'barnet',
  'bexley',
  'brent',
  'bromley',
  'camden',
  'city of london',
  'croydon',
  'ealing',
  'enfield',
  'greenwich',
  'hackney',
  'hammersmith fulham',
  'haringey',
  'harrow',
  'havering',
  'hillingdon',
  'hounslow',
  'islington',
  'kensington chelsea',
  'kingston upon thames',
  'lambeth',
  'lewisham',
  'merton',
  'newham',
  'redbridge',
  'richmond upon thames',
  'southwark',
  'sutton',
  'tower hamlets',
  'waltham forest',
  'wandsworth',
  'westminster')

uk_br = list('enfield',
              'tower hamlets',
              'barking dagenham',
              'north east lincolnshire',
              'hounslow',
              'swindon',
              'wirral',
              'flintshire',
              'portsmouth',
              'plymouth',
              'greenwich',
              'bridgend',
              'blackburn with darwen',
              'birmingham',
              'torbay',
              'luton',
              'newham',
              'stoke-on-trent',
              'leicester',
              'cornwall',
              'sandwell',
              'calderdale',
              'kingston upon hull',
              'peterborough',
              'cardiff',
              'westminster',
              'blackpool',
              'coventry',
              'barnsley',
              'neath port talbot',
              'redbridge',
              'milton keynes',
              'walsall',
              'sheffield',
              'southampton',
              'camden',
              'swansea',
              'manchester',
              'telford wrekin',
              'wakefield',
              'southwark',
              'hackney',
              'harrow',
              'dudley',
              'kirklees',
              'ealing',
              'stockton-on-tees',
              'sefton',
              'northumberland',
              'medway',
              'wolverhampton',
              'powys',
              'liverpool',
              'bradford',
              'islington',
              'derby',
              'barnet',
              'solihull',
              'hillingdon',
              'sutton',
              'thurrock',
              'bromley',
              'southend-on-sea',
              'croydon',
              'haringey',
              'rotherham',
              'merton',
              'south gloucestershire',
              'brighton hove',
              'kensington chelsea',
              'sunderland',
              'leeds',
              'north lincolnshire',
              'havering',
              'york',
              'hammersmith fulham',
              'hartlepool',
              'doncaster',
              'nottingham',
              'east riding of yorkshire',
              'richmond upon thames',
              'rutland',
              'newport',
              'darlington',
              'monmouthshire',
              'city of london',
              'herefordshire',
              'gwynedd',
              'bexley',
              'isle of wight',
              'warrington',
              'wrexham',
              'wandsworth',
              'lewisham',
              'denbighshire',
              'brent',
              'conwy',
              'bristol',
              'lambeth',
              'waltham forest',
              'north somerset',
              'kingston upon thames',
              'wiltshire')
names(gdpz) = sapply(FUN = function(x) {gsub(' ','_',x)},names(gdpz))

london_br = sapply(FUN = function(x) {gsub(' ','_',x)},london_br)
uk_br = sapply(FUN = function(x) {gsub(' ','_',x)},uk_br)
uk_br = setdiff(uk_br,london_br)

num = length(london_br)
num_uk = length(uk_br)
set.seed(7172020)
margin = c()
absol = c()
for (i in c(1:num)){
  name <- london_br[[i]]
  uk_filtered = unlist(setdiff(uk_br, names(gdpz)[cor(gdpz)[name,] == 1]))

  cv_fit = cv.glmnet(y = as.matrix(gdpz %>% filter(year <= 2011) %>% select(london_br[i])),
               x = as.matrix(gdpz %>% filter(year <= 2011) %>% select(uk_filtered)),
               alpha = 1, weights = seq(1:11))
  
  lam = cv_fit$lambda.min
  
  fit = glmnet(y = as.matrix(gdpz %>% filter(year <= 2011) %>% select(london_br[i])),
               x = as.matrix(gdpz %>% filter(year <= 2011) %>% select(uk_filtered)),
               alpha = 1,lambda = lam, weights = c(1:11))
  coef(fit, s= 1)
  
  plot(c(2001:2016),as.matrix(gdpz %>% select(london_br[i])), col = 'black', lty=1, lwd=3, type = 'l',
       xlim = c(2001,2016), ylim = c(min(gdpz %>% select(london_br[i])),max(cbind(gdpz %>% select(london_br[i])),predict(fit,as.matrix(gdpz %>% select(uk_filtered))))),
       ylab = 'predicted gdp', main = london_br[i], xlab = 'years')
  lines(c(2001:2016),predict(fit,as.matrix(gdpz %>% select(uk_filtered))), lty=3, lwd=3)
  abline(v = 2011, lwd=3, col = 'black',lty = 6)
  legend(2001, max(cbind(gdpz %>% select(london_br[i])),predict(fit,as.matrix(gdpz %>% select(uk_filtered)))), legend=c("Synthetic Control", "Actual"),
         col=c("black", "black"), lty=c(3,1), cex=0.8)
  
  margin = c(margin,(gdpz[[london_br[i]]] - predict(fit,as.matrix(gdpz %>% select(uk_filtered)))[,1])/predict(fit,as.matrix(gdpz %>% select(uk_filtered)))[,1])
  absol = c(absol,(gdpz[[london_br[i]]] - predict(fit,as.matrix(gdpz %>% select(uk_filtered)))[,1]))
}


margin2 = c()
absol2 = c()
for (i in c(1:num_uk)){
  name <- uk_br[[i]]
  uk_filtered = unlist(setdiff(uk_br, names(gdpz)[cor(gdpz)[name,] == 1]))
  
  cv_fit = cv.glmnet(y = as.matrix(gdpz %>% filter(year <= 2011) %>% select(uk_br[i])),
                     x = as.matrix(gdpz %>% filter(year <= 2011) %>% select(uk_filtered)),
                     alpha = 1, weights = seq(1:11))
  
  lam = cv_fit$lambda.min
  
  fit = glmnet(y = as.matrix(gdpz %>% filter(year <= 2011) %>% select(uk_br[i])),
               x = as.matrix(gdpz %>% filter(year <= 2011) %>% select(uk_filtered)),
               alpha = 1,lambda = lam, weights = c(1:11))
  coef(fit, s= 1)
  
  plot(c(2001:2016),as.matrix(gdpz %>% select(uk_br[i])), col = 'black', lty=1, lwd=3, type = 'l',
       xlim = c(2001,2016), ylim = c(min(gdpz %>% select(uk_br[i])),max(cbind(gdpz %>% select(uk_br[i])),predict(fit,as.matrix(gdpz %>% select(uk_filtered))))),
       ylab = 'predicted gdp', main = uk_br[i], xlab = 'years')
  lines(c(2001:2016),predict(fit,as.matrix(gdpz %>% select(uk_filtered))), lty=3, lwd=3)
  abline(v = 2011, lwd=3, col = 'black',lty = 6)
  legend(2001, max(cbind(gdpz %>% select(uk_br[i])),predict(fit,as.matrix(gdpz %>% select(uk_filtered)))), legend=c("Synthetic Control", "Actual"),
         col=c("black", "black"), lty=c(3,1), cex=0.8)
  
  plot(c(2001:2016),as.matrix(gdpz %>% select(uk_br[i])), col = 'red', lty=1, lwd=3, type = 'l',
       xlim = c(2001,2016), ylim = c(min(gdpz %>% select(uk_br[i])),max(cbind(gdpz %>% select(uk_br[i])),predict(fit,as.matrix(gdpz %>% select(uk_filtered))))),
       ylab = 'predicted gdp', main = uk_br[i])
  lines(c(2001:2016),predict(fit,as.matrix(gdpz %>% select(uk_filtered))), lty=2, lwd=3)
  lines(c(2011:2016),predict(fit,as.matrix(gdpz %>% filter(year>=2011) %>% select(uk_filtered))), col = 'green',lty=2, lwd=3)
  
  margin2 = c(margin2,(gdpz[[uk_br[i]]] - predict(fit,as.matrix(gdpz %>% select(uk_filtered)))[,1])/predict(fit,as.matrix(gdpz %>% select(uk_filtered)))[,1])
  absol2 = c(absol2,(gdpz[[uk_br[i]]] - predict(fit,as.matrix(gdpz %>% select(uk_filtered)))[,1]))
}

num_year = 16
plot(c(2001:2016),rep(0,num_year), type = 'l', ylim = c(-0.2,0.2), lwd=3, col = 'green')

for (i in seq(1,length(margin2),by = num_year)){
  lines(c(2001:2016),margin2[i:(i+num_year-1)],lwd=2, col = 'grey')
}

for (i in seq(1,length(margin),by = num_year)){
  lines(c(2001:2016),margin[i:(i+num_year-1)],lwd=2)
}

meanz = c()
for (i in seq(1,length(margin2),by = num_year)){
  meanz = c(meanz,mean(margin2[i:(i+num_year-1)][1:11]))
}

margin2_pruned = c()
absol2_pruned = c()
uk_pruned_names = c()
j = 1
for (i in seq(1,length(margin2),by = num_year)){
  if ((abs(meanz) < 0.005)[j]){
    margin2_pruned = c(margin2_pruned,margin2[i:(i+num_year-1)])
    absol2_pruned = c(absol2_pruned,absol2[i:(i+num_year-1)])
    uk_pruned_names = c(uk_pruned_names, uk_br[as.integer(i/16) + 1])
  }
  j = j+ 1
}

uk_pruned_names = sapply(FUN = function(x) {gsub('-','_',x)}, uk_pruned_names)

plot(c(2001:2016),rep(0,num_year), type = 'l', ylim = c(-0.3,0.3), lwd=3, col = 'green')
for (i in seq(1,length(margin2_pruned),by = num_year)){
  lines(c(2001:2016),margin2_pruned[i:(i+num_year-1)],lwd=2, col = 'grey')
}
for (i in seq(1,length(margin),by = num_year)){
  lines(c(2001:2016),margin[i:(i+num_year-1)],lwd=2)
}

# margin
# for (i in seq(1,length(margin),by = num_year)){
#   print(margin[i:(i+num_year-1)][num_year-7:num_year])
# }

## Code up Annuity Formula
## Code up P-value

#use two-sided test
diff = 4
margin_split_og = split(margin, ceiling(seq_along(margin)/num_year))
margin_split = lapply(FUN = function(x) {abs(x[(num_year-diff):num_year])}, margin_split_og)
margin_split_full = lapply(FUN = function(x) {(x[(num_year-diff):num_year])}, margin_split_og)

prune_split_og = split(margin2_pruned, ceiling(seq_along(margin2_pruned)/num_year))
prune_split = lapply(FUN = function(x) {abs(x[(num_year-diff):num_year])}, prune_split_og)
prune_split_full = lapply(FUN = function(x) {(x[(num_year-diff):num_year])}, prune_split_og)

absol_split = split(absol, ceiling(seq_along(absol)/num_year))
absol_split = lapply(FUN = function(x) {(x[(num_year-diff):num_year])}, absol_split) %>% data.frame()
names(absol_split) = london_br
absol_split = mutate(absol_split, year = c(2012:2016))

absol_uk = split(absol2_pruned, ceiling(seq_along(absol2_pruned)/num_year))
absol_uk = lapply(FUN = function(x) {x[(num_year-diff):num_year]}, absol_uk) %>% data.frame()
names(absol_uk) = uk_pruned_names
absol_uk = mutate(absol_uk, year = c(2012:2016))

p_val <- matrix(0, ncol = num, nrow = 5) %>% data.frame() 
names(p_val) <- london_br
p_val <- p_val %>% mutate(year = c(2012:2016))
p_val_raw <- p_val


#only 32 control borough
for (county in 1:num) {
  pval_year = c()
  for (year in 1:5) {
    pval_year <- c(pval_year, sum(margin_split[[county]][year] > (unlist(map(prune_split, year)))))
  }
  p_val_raw[county] = pval_year
  p_val[county] = (length(prune_split) - pval_year)/length(prune_split)
}

#Bayesian update
#Beta(0.5, 0.5) prior

#success: #boroughs with smaller margin, failure: #boroughs with larger margin
#varable of interest: p, probability of getting a higher margin

bayes <- matrix(0, ncol = num, nrow = 5) %>% data.frame() 
names(bayes) <- london_br
bayes <- bayes %>% mutate(year = c(2012:2016))

for (county in 1:num) {
  alpha <- c()
  beta <- c()
  for (year in 1:5) {
    alpha <- c(alpha, sum(p_val_raw[[county]][1:year]))
    beta <- c(beta, length(prune_split) * year - alpha[length(alpha)])
    if (year == 1) {
      alpha <- alpha + 0.5
      beta <- beta + 0.5
    }
  }
  bayes[county] = round(alpha/(alpha+beta),3)
}

#annuity
value_raw = absol_split
borough_value_raw = absol_uk

inflate <- read.csv('UK_inflation.csv', header = T) 
names(inflate) = c('year', 'inflation' , 'multiplier')
inflate <- inflate %>% filter(year >= 2012 & year <= 2016) %>% arrange(year)

#use 2018 as baseline cause that's what the data is
value_inflate = 
  data.frame(apply(value_raw, MARGIN = 2, FUN = function(x) {x / inflate[['multiplier']]})) %>% mutate(year = c(2012:2016))

borough_value_inflate = 
  data.frame(apply(borough_value_raw, MARGIN = 2, FUN = function(x) {x / inflate[['multiplier']]})) %>% mutate(year = c(2012:2016))

value_aggre = value_inflate
borough_value_aggre = borough_value_inflate
for(i in 1:5) {
  for(county in 1:dim(value_aggre)[2]) {
    value_aggre[[county]][i] = sum(value_inflate[[county]][1:i])
  }
  for(uk_county in 1:dim(borough_value_aggre)[2]) {
    borough_value_aggre[[uk_county]][i] = sum(borough_value_inflate[[uk_county]][1:i])
  }
}
value_aggre[['year']] = c(2012:2016)
borough_value_aggre[['year']] = c(2012:2016)

#perecent positive, summary stat
positive <- matrix(0, nrow = 5, ncol = 2) %>% data.frame()
names(positive) = c('positive_london', 'positive_uk')
percent_london <- c()
percent_uk <- c()
for (i in 1:5) {
  percent_london = c(percent_london, sum(unlist(map(margin_split_full, i)) > 0) / length(margin_split_full))
  percent_uk = c(percent_uk, sum(unlist(map(prune_split_full, i)) > 0) / length(prune_split_full))
}
positive['positive_london'] = percent_london
positive['positive_uk'] = percent_uk
positive['year'] = c(2012:2016)

#average value margin comparison, summary stat
data.frame(ID=value_aggre[['year']], Means=rowMeans(value_aggre %>% select(-year)))
data.frame(ID=borough_value_aggre[['year']], Means=rowMeans(borough_value_aggre %>% select(-year)))

#average

write.csv(value_aggre, "GDP_Margin_Aggregate.csv")
write.csv(borough_value_aggre, "GDP_Margin_Aggregate_uk.csv")
write.csv(value_inflate, "GDP_Margin_Year.csv")
