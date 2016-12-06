## ----setup, echo = FALSE, message = FALSE, warning = FALSE---------------
options(scipen = 9, digits = 4)
library(knitr)
library(dplyr)
library(tidyr)

## ---- echo = FALSE, message = FALSE, warning = FALSE---------------------
# # Data was downloaded from http://www.britishelectionstudy.com/custom/uploads/2016/10/BES2015_W9_Panel_v1.0.zip
# # Then manually opened in Stata and saved as a csv file, so that only the required variables can be imported.
# bregret.all <- read.csv("data/BES2015_W9_Panel_v1.0/bregret.csv", nrows=1)

## looking for vairables:
# search.pattern <- "education"
# names(bregret.all)[grep(search.pattern,names(bregret.all))]
# grep(search.pattern,names(bregret.all))
# 
# stencil <- c(rep("NULL",3879))
# stencil[c(18, #  = wt_full_W6
#           26, # = wt_full_W9
#           2218, # = regretsIHaveAFewW6
#           2636, # = euRefExpectationW7
#           3148, # = euRefExpectationW8
#           3464, # = econPersonalRetroW8
#           3531, # = euRefTurnoutRetroW9
#           3533, # = euRefVoteW9
#           3535, # = regretsIHaveAFewW9
#           3752, # = ageGroup
#           3758, # = ns_secW1W2W3W4W5
#           3759, # = ns_sec_analyticW1W2W3W4W5
#           3762, # = gender
#           3766, # = gor
#           3767 # = education
#           )] <- NA
# 
# bregret.raw <- read.csv("data/BES2015_W9_Panel_v1.0/bregret.csv", colClasses = stencil)
# write.table(bregret.raw, file = "data/bregret.raw.csv")


bregret.raw <- read.table( "../data/bregret.raw.csv")
regrets <- xtabs(wt_full_W9~ regretsIHaveAFewW9 + euRefVoteW9 , data = bregret.raw)

names(attributes(regrets)$dimnames) <- c( "Regrets", "Reported vote")
colnames(regrets) <- c("Remain", "Leave", "Don't remember")
rownames(regrets) <- c("Regrets - No", "Regrets - Yes", "Regrets - Not sure")
regrets.prop <- apply(regrets, 2, function(x) x/sum(x))
N <- sum(xtabs(~ regretsIHaveAFewW9 + euRefVoteW9,data = bregret.raw))
kable(regrets.prop*100, digits = 2, caption = paste("Table 1: Percentage regretting their vote in each EU referendum camp (N =", N  , ")"))

## ---- echo = FALSE, message = FALSE, warning = FALSE---------------------
# # Manually get actual referendum result. 
# http://www.electoralcommission.org.uk/find-information-by-subject/elections-and-referendums/past-elections-and-referendums/eu-referendum/electorate-and-count-information
remain <- 16141241
leave <- 17410742


data.frame(regrets = c("no", "yes", "dont know"),
                       Remain = regrets.prop[,1]* remain,
                       Leave = regrets.prop[,2]* leave, row.names = NULL) %>%
  gather(vote, count, 2:3, convert = TRUE) %>%
  mutate(vote2 = c("Remain 2", "Leave 2", "Remain 2", "Leave 2", "Remain 2","Leave 2"))%>%
  gather(time, vote,c(2,4), convert = TRUE) %>%
  group_by(time, vote) %>%
  summarise(result = sum(count)) %>%
  ungroup() %>%
  group_by(time) %>%
  mutate(prop = result/sum(result)*100) ->
  results

kable(results[1:2,2:4], digits = 4, caption = "Table 2: Actual EU referendum results")

# results1 <- data.frame(regrets = c("0", "1", "0"),
#                        remain = regrets.prop[,1]* remain,
#                        leave = regrets.prop[,2]* leave, row.names = NULL) 
# results1 %>% 
#   group_by(regrets) %>% 
#   summarize(remain = sum(remain), leave = sum(leave))%>% 
#   gather(N1, Value, 2:3, convert = TRUE) %>%
#   mutate(N2 = c("remain2", "leave2",  "leave2", "remain2"),
#          edgecol = c("red", "red", "orange", "green")) %>%
#   arrange(N1, N2)->
#   edges
# 
# nodes <- data.frame(ID = unique(c(edges$N1, edges$N2)), x = c(1,1,2,2),
#                      col= c( "yellow", "blue", "yellow", "blue" ),
#                     stringsAsFactors= FALSE )
# 
# r <- makeRiver(nodes, as.data.frame(edges))
# plot(r, node_margin = 0)



## ---- echo = FALSE, message = FALSE, warning = FALSE---------------------


kable(data.frame(Remain = regrets.prop[,1]* remain,
                Leave = regrets.prop[,2]* leave),
      caption = "Table 3: Hypothetical numbers of voters regretting their decision"
)

## ---- echo = FALSE, message = FALSE, warning = FALSE---------------------


kable(results[3:4,2:4], digits = 2, caption = "Table 4: Hypothetical referendum results with regretters switching camp")


## ---- echo = FALSE, message = FALSE, warning = FALSE---------------------

comp <- cbind(elections2015 = prop.table(xtabs(wt_full_W6~ regretsIHaveAFewW6 , data = bregret.raw))*100,
      referendum2016 = prop.table(xtabs(wt_full_W9~ regretsIHaveAFewW9 , data = bregret.raw))*100)
rownames(comp) <- c("Regrets - No", "Regrets - Yes", "Regrets - Not sure")
options(scipen = 9, digits = 2)


## ---- echo = FALSE, message = FALSE, warning = FALSE---------------------
options(scipen = 9, digits = 3)

kable(comp, digits = 2, caption = "Table 5: Comparing regret levels with 2015 elections",
      col.names = c("Elections 2015", "Referendum 2016"))
# pt <- cbind(elections2015 = xtabs(wt_full_W6~ regretsIHaveAFewW6 , data = bregret.raw),
  #    referendum2016 = xtabs(wt_full_W9~ regretsIHaveAFewW9 , data = bregret.raw))

# prop.test(c(pt[1,1], pt[1,2]), c(sum(pt[,1]), sum(pt[,2]))) 

