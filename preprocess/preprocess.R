
#####################################################################################
#######################      package      #############################################
#####################################################################################


library('tidyverse')
library('reshape2')

#####################################################################################
#######################   import data  #############################################
#####################################################################################


train_label= read_csv("../Raw/train_label.csv")
train_activity  = read_csv("../Raw/train_activity.csv")
train_combat = read_csv("../Raw/train_combat.csv")
train_payment = read_csv("../Raw/train_payment.csv")
train_pledge = read_csv("../Raw/train_pledge.csv")
train_trade = read_csv("../Raw/train_trade.csv")


test1_activity  = read_csv("../Raw/test1_activity.csv")
test1_combat = read_csv("../Raw/test1_combat.csv")
test1_payment = read_csv("../Raw/test1_payment.csv")
test1_pledge = read_csv("../Raw/test1_pledge.csv")
test1_trade = read_csv("../Raw/test1_trade.csv")


test2_activity  = read_csv("../Raw/test2_activity.csv")
test2_combat = read_csv("../Raw/test2_combat.csv")
test2_payment = read_csv("../Raw/test2_payment.csv")
test2_pledge = read_csv("../Raw/test2_pledge.csv")
test2_trade = read_csv("../Raw/test2_trade.csv")



#####################################################################################
#######################      function      #############################################
#####################################################################################


conseq=function(series,value){
  days = unique(series)
  diff= days - lag(days)
  diff = ifelse(is.na(diff)==T,1,diff)
  tmp <- rle(diff);
  runs <- tmp$lengths[tmp$values == value]
  max <- max(runs)
  max_1 <- max(runs)+1
  ifelse(length(runs)==0,
         return(0),
         ifelse(which(tmp$lengths==max(tmp$length))==1,
                return(max),
                return(max_1)))
}


no_conseq=function(series,value){
  days = unique(series)
  diff= days - lag(days)
  diff = ifelse(is.na(diff)==T,1,diff)
  tmp <- rle(diff);
  ifelse(length(diff)==1 & diff==1, return(0), return(max(diff)+1))}

switch = function(series){
  days = unique(series)
  diff= days - lag(days)
  diff = ifelse(is.na(diff)==T,1,diff)
  tmp <- rle(diff)
  
  result=ifelse(tmp$values!=1, ifelse(tmp$lengths ==1, 1 , tmp$lengths),0)
  return(sum(result)*2)
}


#####################################################################################
#######################      train      #############################################
#####################################################################################


train_label = train_label %>%  
  mutate(acc_id = as.numeric(acc_id),
         churn=ifelse(survival_time<64,1,0),
         churn_week = ifelse(survival_time<8,1,
                             ifelse(survival_time<15,2,
                                    ifelse(survival_time<22,3,
                                           ifelse(survival_time<29,4,
                                                  ifelse(survival_time<36,5,
                                                         ifelse(survival_time<43,6,
                                                                ifelse(survival_time<50,7,
                                                                       ifelse(survival_time<57,8,
                                                                              ifelse(survival_time<64,9,0))))))))),
         label_spent=ifelse(amount_spent>0,1,0))


train_activity1 = train_activity%>%
  mutate(server = as.factor(server),
         char_id = as.factor(char_id),
         fishing = ifelse(playtime>0,fishing,0),
         private_shop= ifelse(playtime>0,private_shop,0),
         week_cate = ifelse(day<8,1,ifelse(day<15,2,ifelse(day<22,3,4)))) %>% 
  group_by(acc_id) %>% 
  summarize(login_cnt= n_distinct(day),
            char_cnt = n_distinct(char_id),
            total_playtime = sum(playtime),
            
            conseq_log = conseq(day,1),
            conseq_nolog = no_conseq(day,1),
            first_app = min(day),
            
            start_week = min(week_cate),
            per_log = login_cnt/(29-first_app),
            switchlog_cnt = switch(day),
            
            server_cnt = n_distinct(server),
            total_npckill=sum(npc_kill),
            mean_npckill=mean(npc_kill),
            
            total_soloexp = sum(solo_exp),
            total_partyexp=sum(party_exp),
            total_questexp=sum(quest_exp),
            
            soloexp_cnt= n_distinct(day[solo_exp!=0]),
            partyexp_cnt=n_distinct(day[party_exp!=0]),
            questexp_cnt=n_distinct(day[quest_exp!=0]),
            
            per_soloday = soloexp_cnt/login_cnt,
            per_partyday =partyexp_cnt/login_cnt,
            per_questday = questexp_cnt/login_cnt,
            
            total_bossattack = sum(rich_monster),
            bossattack_cnt = n_distinct(day[rich_monster!=0]),
            per_bossattack = bossattack_cnt/login_cnt,
            
            
            
            total_death= sum(death),
            total_revive = sum(revive),
            total_recovery = sum(exp_recovery),
            
            total_fishing = sum(fishing),
            per_fishing= total_fishing/total_playtime,
            
            total_privateshop = sum(private_shop),
            per_privateshop = total_privateshop / total_playtime,
            
            total_enchant= sum(enchant_count),
            total_moneychange = sum(game_money_change),
            total_moneychange_abs = sum(abs(game_money_change)))

train_activity2 = train_activity%>%
  mutate(server = as.factor(server),
         char_id = as.factor(char_id),
         fishing = ifelse(playtime>0,fishing,0),
         private_shop= ifelse(playtime>0,private_shop,0),
         week_cate = ifelse(day<8,1,ifelse(day<15,2,ifelse(day<22,3,4)))) %>%
  filter(week_cate == 4) %>% 
  group_by(acc_id,week_cate) %>% 
  summarize(login_cnt= n_distinct(day),
            total_playtime = sum(playtime),
            mean_playtime = mean(playtime),
            
            server_cnt = n_distinct(server),
            total_npckill=sum(npc_kill),
            mean_npckill=mean(npc_kill),
            
            
            total_soloexp = sum(solo_exp),
            soloexp_cnt= n_distinct(day[solo_exp!=0]),
            
            total_partyexp=sum(party_exp),
            partyexp_cnt=n_distinct(day[party_exp!=0]),
            
            total_questexp=sum(quest_exp),
            questexp_cnt=n_distinct(day[quest_exp!=0]),
            
            per_soloday = soloexp_cnt/login_cnt,
            per_partyday =partyexp_cnt/login_cnt,
            per_questday = questexp_cnt/login_cnt,
            
            total_bossattack = sum(rich_monster),
            bossattack_cnt = n_distinct(day[rich_monster!=0]),
            total_death= sum(death),
            per_bossattack = bossattack_cnt/login_cnt,
            
            total_revive = sum(revive),
            total_recovery = sum(exp_recovery),
            
            total_fishing = sum(fishing),
            per_fishing= total_fishing/total_playtime,
            
            total_privateshop = sum(private_shop),
            per_privateshop = total_privateshop / total_playtime,
            
            total_enchant= sum(enchant_count),
            total_moneychange = sum(game_money_change),
            total_moneychange_abs = sum(abs(game_money_change)),
            char_id = n_distinct(char_id)
  ) %>% 
  reshape2::melt(id= c("acc_id","week_cate")) %>% 
  reshape2::dcast(acc_id~variable+week_cate)


train_trade1 = train_trade%>%
  mutate(source_acc_id = source_acc_id,
         target_acc_id= target_acc_id,
         server = as.factor(server),
         hour =str_sub(time,1,2),
         week_cate = ifelse(day<8,1,ifelse(day<15,2,ifelse(day<22,3,4))),
         hour_cate = ifelse(hour<7,1,ifelse(hour<13,2,ifelse(hour<19,3,4))))%>%
  gather(key="seller_consumer_acc",value="acc_id",'source_acc_id','target_acc_id')%>% 
  as_tibble %>% 
  mutate(char_id=ifelse(seller_consumer_acc=='source_acc_id',source_char_id,target_char_id),
         source_char_id = as.numeric(source_char_id),
         target_char_id = as.factor(target_char_id)) %>% 
  select(-source_char_id,-target_char_id) %>% 
  filter(acc_id %in% unique(train_activity$acc_id)) %>% 
  group_by(acc_id,item_type) %>%  
  mutate(count_item_type=n()) %>%
  ungroup() %>% 
  group_by(acc_id,hour_cate) %>%  
  mutate(count_hour=n()) %>%
  ungroup() %>% 
  group_by(acc_id ) %>% 
  summarise(sell_cnt = n_distinct(time[seller_consumer_acc == "source_acc_id"]),
            buy_cnt = n_distinct(time[seller_consumer_acc == "target_acc_id"]),
            trade_cnt = n_distinct(time),
            per_sell = sell_cnt / trade_cnt,
            per_buy = buy_cnt / trade_cnt ,
            
            most_trade_item =
              ifelse(n_distinct(item_type[count_item_type== max(count_item_type)])==1,
                     item_type[count_item_type== max(count_item_type)],
                     item_type[count_item_type== max(count_item_type)][1]),
            indiv_trade_cnt = n_distinct(time[type == 1]),
            
            per_indiv_trade = indiv_trade_cnt / trade_cnt, 
            
            most_trade_time = ifelse(n_distinct(hour_cate[count_hour==max(count_hour)])==1,
                                     hour_cate[count_hour==max(count_hour)],
                                     hour_cate[count_hour==max(count_hour)][1])
  )


train_trade2 = train_trade%>%
  mutate(source_acc_id = source_acc_id,
         target_acc_id= target_acc_id,
         server = as.factor(server),
         hour =str_sub(time,1,2),
         week_cate = ifelse(day<8,1,ifelse(day<15,2,ifelse(day<22,3,4))),
         hour_cate = ifelse(hour<7,1,ifelse(hour<13,2,ifelse(hour<19,3,4))))%>%
  gather(key="seller_consumer_acc",value="acc_id",'source_acc_id','target_acc_id')%>% 
  as_tibble %>% 
  mutate(char_id=ifelse(seller_consumer_acc=='source_acc_id',source_char_id,target_char_id),
         source_char_id = as.numeric(source_char_id),
         target_char_id = as.factor(target_char_id)) %>% 
  select(-source_char_id,-target_char_id) %>% 
  filter(acc_id %in% unique(train_activity$acc_id)) %>% 
  group_by(acc_id,item_type) %>%  
  mutate(count_item_type=n()) %>%
  ungroup() %>% 
  group_by(acc_id,hour_cate) %>%  
  mutate(count_hour=n()) %>%
  ungroup() %>% 
  filter(week_cate==4) %>% 
  group_by(acc_id , week_cate) %>% 
  summarise(sell_cnt = n_distinct(time[seller_consumer_acc == "source_acc_id"]),
            buy_cnt = n_distinct(time[seller_consumer_acc == "target_acc_id"]),
            trade_cnt = n_distinct(time),
            
            indiv_trade_cnt = n_distinct(time[type == 1]),
            
            
  )%>% 
  reshape2::melt(id= c("acc_id","week_cate")) %>% 
  reshape2::dcast(acc_id~variable+week_cate)

train_combat1 = train_combat %>%  
  mutate(random_att_ratio = ifelse(random_attacker_cnt==0 & random_defender_cnt==0,
                                   0,
                                   random_attacker_cnt/(random_attacker_cnt+random_defender_cnt)),
         week_cate = ifelse(day<8,1,ifelse(day<15,2,ifelse(day<22,3,4))),
         all_zero= rowSums(train_combat[,7:13])) %>% 
  group_by(acc_id,char_id) %>% 
  mutate(max_level = max(level),
         diff_level=level-lag(level),
         diff_level = ifelse(is.na(diff_level)==T,0,diff_level),
         change_level = ifelse(diff_level==0,0,1)) %>% 
  ungroup() %>% 
  group_by(acc_id) %>% 
  summarise(random_att_ratio_avg=mean(random_att_ratio),
            max_level =max (max_level),
            combat_login_cnt= n_distinct(day[all_zero != 0]),
            n_char_id = n_distinct(char_id),
            diff_level_avg = sum(abs(diff_level))/n_char_id,
            change_level_avg = sum(change_level)/n_char_id,
            
            total_pledge_cnt=sum(pledge_cnt),
            total_random_attacker_cnt=sum(random_attacker_cnt),
            total_random_defender_cnt=sum(random_defender_cnt),
            total_temp_cnt=sum(temp_cnt),
            total_etc_cnt=sum(etc_cnt),
            total_num_opponent=sum(num_opponent)
  ) %>% 
  select(-n_char_id)

train_combat2 = train_combat %>%  
  mutate(random_att_ratio = ifelse(random_attacker_cnt==0 & random_defender_cnt==0,
                                   0,
                                   random_attacker_cnt/(random_attacker_cnt+random_defender_cnt)),
         week_cate = ifelse(day<8,1,ifelse(day<15,2,ifelse(day<22,3,4))),
         all_zero= rowSums(train_combat[,7:13])) %>%
  filter(week_cate == 4) %>% 
  group_by(acc_id,char_id) %>% 
  mutate(max_level = max(level),
         diff_level=level-lag(level),
         diff_level = ifelse(is.na(diff_level)==T,0,diff_level),
         change_level = ifelse(diff_level==0,0,1)) %>% 
  ungroup() %>% 
  group_by(acc_id , week_cate) %>% 
  summarise(total_pledge_cnt=sum(pledge_cnt),
            total_random_attacker_cnt=sum(random_attacker_cnt),
            total_random_defender_cnt=sum(random_defender_cnt),
            total_temp_cnt=sum(temp_cnt),
            total_etc_cnt=sum(etc_cnt),
            total_same_pledge_cnt =sum(same_pledge_cnt),
            total_num_opponent=sum(num_opponent),
            combat_login_cnt= n_distinct(day[all_zero != 0])
            
  ) %>% 
  reshape2::melt(id= c("acc_id","week_cate")) %>% 
  reshape2::dcast(acc_id~variable+week_cate)


train_payment1=merge(train_payment,
                     train_activity1%>% select(acc_id,first_app,login_cnt),
                     by="acc_id",all.x=T) 

train_payment1 = train_payment1 %>%
  group_by(acc_id) %>% 
  summarise(pay_yn = ifelse(sum(amount_spent)!=0,1,0),
            total_pay_amount = sum(amount_spent),
            mean_pay_amount = mean(amount_spent),
            
            max_pay_amount= max(amount_spent),
            min_pay_amount = min(amount_spent),
            last_pay_amount = amount_spent[day==max(day) ],
            
            first_app =min(first_app),
            login_cnt= min(login_cnt),
            
            
            first_pay_day= min(day),
            days_since_last_pay = 29-max(day),
            first_pay_from_start = first_pay_day-first_app,
            
            pay_day_cnt= n_distinct(day),
            avail_day= 29- first_app,
            
            pmday_availday_ratio = pay_day_cnt/avail_day,
            pmday_playday_ratio = pay_day_cnt/login_cnt
            
  ) %>% 
  select(-first_app,-login_cnt)

train_payment2 = train_payment %>% 
  mutate(week_cate = ifelse(day<8,1,ifelse(day<15,2,ifelse(day<22,3,4)))) %>% 
  filter(week_cate == 4) %>% 
  group_by(acc_id , week_cate) %>% 
  summarise(total_payment = sum(amount_spent),
            payment_cnt = n_distinct(day)
  ) %>% 
  reshape2::melt(id= c("acc_id","week_cate")) %>% 
  reshape2::dcast(acc_id~variable+week_cate)


train_pledge=merge(train_pledge,
                   train_activity%>%
                     select(acc_id,char_id,server,day,playtime),
                   by=c("acc_id","char_id","server","day"),all.x=T) 

train_pledge1 = train_pledge %>%
  mutate(week_cate = ifelse(day<8,1,ifelse(day<15,2,ifelse(day<22,3,4)))) %>% 
  group_by(acc_id,char_id) %>% 
  mutate(n_day= n_distinct(day)) %>% 
  ungroup() %>% 
  group_by(acc_id) %>% 
  summarise(main_char_id = ifelse(n_distinct(char_id[playtime == max(playtime)]==1),
                                  char_id[playtime == max(playtime)],
                                  char_id[n_day==max(n_day)]),
            pl_play_char_cnt = mean(play_char_cnt[char_id==main_char_id]),
            pl_combat_char_cnt = mean(combat_char_cnt[char_id == main_char_id]),
            
            pl_pledge_combat_cnt = mean(pledge_combat_cnt[char_id == main_char_id]),
            pl_random_attacker_cnt = mean(random_attacker_cnt[char_id == main_char_id]),
            pl_random_defender_cnt = mean(random_defender_cnt[char_id == main_char_id]),
            pl_same_pledge_cnt = mean(same_pledge_cnt[char_id == main_char_id]),
            pl_temp_cnt = mean(temp_cnt[char_id == main_char_id]),
            pl_etc_cnt= mean(etc_cnt[char_id == main_char_id]),
            pl_combat_play_time=mean(combat_play_time[char_id == main_char_id]),
            pl_combat_char_ratio = pl_combat_char_cnt/pl_play_char_cnt,
            pl_pledge_cnt= n_distinct(pledge_id)) %>% 
  select(-main_char_id)

train_activity3 = train_activity %>% 
  select(acc_id,playtime,solo_exp,party_exp,quest_exp,day) %>%
  mutate(tend_playtime =(playtime -  mean(playtime))/sd(playtime), 
         tend_soloexp = (solo_exp -  mean(solo_exp))/sd(solo_exp),
         tend_partyexp = (party_exp -mean(party_exp))/sd(party_exp),
         tend_questexp = (quest_exp -mean(quest_exp))/sd(quest_exp),
         max_time= max(playtime)
  ) %>% 
  group_by(acc_id) %>% 
  summarise(tend_playtime = mean(tend_playtime),
            tend_soloexp = mean(tend_soloexp),
            tend_partyexp = mean(tend_partyexp),
            tend_questexp = mean(tend_questexp),
            maxplay_cnt = n_distinct(day[playtime ==max_time]))


train_combat3 = train_combat %>% 
  select(acc_id,pledge_cnt,etc_cnt,num_opponent) %>%
  mutate(tend_pledge_cnt =(pledge_cnt -  mean(pledge_cnt))/sd(pledge_cnt), 
         tend_etc_cnt = (etc_cnt -  mean(etc_cnt))/sd(pledge_cnt),
         tend_num_opponent = (num_opponent -  mean(num_opponent))/sd(num_opponent)
  ) %>% 
  group_by(acc_id) %>% 
  summarise(tend_pledge_cnt = mean(tend_pledge_cnt),
            tend_etc_cnt = mean(tend_etc_cnt),
            tend_num_opponent=mean(tend_num_opponent))

train=merge(train_label,train_activity1,by="acc_id",all = T)
train=merge(train,train_trade1,by="acc_id",all = T)
train=merge(train,train_combat1,by="acc_id",all = T)
train=merge(train,train_payment1,by="acc_id",all = T)
train=merge(train,train_pledge1,by="acc_id",all = T)
train=merge(train,train_activity2,by="acc_id",all=T)
train=merge(train,train_activity3,by="acc_id",all = T)
train=merge(train,train_payment2,by="acc_id",all = T)
train=merge(train,train_trade2,by="acc_id",all = T)
train=merge(train,train_combat2,by="acc_id",all = T)
train=merge(train,train_combat3,by="acc_id",all = T)

colnames(train)
train = train  %>% 
  mutate(   most_trade_time =as.numeric( most_trade_time),
            most_trade_item = ifelse(most_trade_item=="etc",1,
                                     ifelse(most_trade_item=="accessory",2,
                                            ifelse(most_trade_item=="adena",3,
                                                   ifelse(most_trade_item=="enchant_scroll",4,
                                                          ifelse(most_trade_item=="spell",5,
                                                                 ifelse(most_trade_item=="weapon",6,
                                                                        ifelse(most_trade_item=="armor",7,0))))))),
            
           avail_day = ifelse(is.na(avail_day)==TRUE,
                              29-first_app,
                              avail_day),
           
           days_since_last_pay = ifelse(is.na(days_since_last_pay)==TRUE,
                                        29,
                                        days_since_last_pay),
           
           first_pay_from_start = ifelse(is.na(first_pay_from_start)==TRUE,
                                         -first_app,
                                         first_pay_from_start)
  ) %>% 
   mutate_at(vars(c(colnames(train))[c(1:67,70,72:ncol(train))]),function(x){ifelse(is.na(x)==TRUE,0,x)})

write.csv(train,file="train_preprocess.csv",row.names=FALSE)

#####################################################################################
#######################      test1      #############################################
#####################################################################################


test1_activity1 = test1_activity%>%
  mutate(server = as.factor(server),
         char_id = as.factor(char_id),
         fishing = ifelse(playtime>0,fishing,0),
         private_shop= ifelse(playtime>0,private_shop,0),
         week_cate = ifelse(day<8,1,ifelse(day<15,2,ifelse(day<22,3,4)))) %>% 
  group_by(acc_id) %>% 
  summarize(login_cnt= n_distinct(day),
            char_cnt = n_distinct(char_id),
            total_playtime = sum(playtime),
            
            conseq_log = conseq(day,1),
            conseq_nolog = no_conseq(day,1),
            first_app = min(day),
            
            start_week = min(week_cate),
            per_log = login_cnt/(29-first_app),
            switchlog_cnt = switch(day),
            
            server_cnt = n_distinct(server),
            total_npckill=sum(npc_kill),
            mean_npckill=mean(npc_kill),
            
            total_soloexp = sum(solo_exp),
            total_partyexp=sum(party_exp),
            total_questexp=sum(quest_exp),
            
            soloexp_cnt= n_distinct(day[solo_exp!=0]),
            partyexp_cnt=n_distinct(day[party_exp!=0]),
            questexp_cnt=n_distinct(day[quest_exp!=0]),
            
            per_soloday = soloexp_cnt/login_cnt,
            per_partyday =partyexp_cnt/login_cnt,
            per_questday = questexp_cnt/login_cnt,
            
            total_bossattack = sum(rich_monster),
            bossattack_cnt = n_distinct(day[rich_monster!=0]),
            per_bossattack = bossattack_cnt/login_cnt,
            
            
            
            total_death= sum(death),
            total_revive = sum(revive),
            total_recovery = sum(exp_recovery),
            
            total_fishing = sum(fishing),
            per_fishing= total_fishing/total_playtime,
            
            total_privateshop = sum(private_shop),
            per_privateshop = total_privateshop / total_playtime,
            
            total_enchant= sum(enchant_count),
            total_moneychange = sum(game_money_change),
            total_moneychange_abs = sum(abs(game_money_change)))

test1_activity2 = test1_activity%>%
  mutate(server = as.factor(server),
         char_id = as.factor(char_id),
         fishing = ifelse(playtime>0,fishing,0),
         private_shop= ifelse(playtime>0,private_shop,0),
         week_cate = ifelse(day<8,1,ifelse(day<15,2,ifelse(day<22,3,4)))) %>%
  filter(week_cate == 4) %>% 
  group_by(acc_id,week_cate) %>% 
  summarize(login_cnt= n_distinct(day),
            total_playtime = sum(playtime),
            mean_playtime = mean(playtime),
            
            server_cnt = n_distinct(server),
            total_npckill=sum(npc_kill),
            mean_npckill=mean(npc_kill),
            
            
            total_soloexp = sum(solo_exp),
            soloexp_cnt= n_distinct(day[solo_exp!=0]),
            
            total_partyexp=sum(party_exp),
            partyexp_cnt=n_distinct(day[party_exp!=0]),
            
            total_questexp=sum(quest_exp),
            questexp_cnt=n_distinct(day[quest_exp!=0]),
            
            per_soloday = soloexp_cnt/login_cnt,
            per_partyday =partyexp_cnt/login_cnt,
            per_questday = questexp_cnt/login_cnt,
            
            total_bossattack = sum(rich_monster),
            bossattack_cnt = n_distinct(day[rich_monster!=0]),
            total_death= sum(death),
            per_bossattack = bossattack_cnt/login_cnt,
            
            total_revive = sum(revive),
            total_recovery = sum(exp_recovery),
            
            total_fishing = sum(fishing),
            per_fishing= total_fishing/total_playtime,
            
            total_privateshop = sum(private_shop),
            per_privateshop = total_privateshop / total_playtime,
            
            total_enchant= sum(enchant_count),
            total_moneychange = sum(game_money_change),
            total_moneychange_abs = sum(abs(game_money_change)),
            char_id = n_distinct(char_id)
  ) %>% 
  reshape2::melt(id= c("acc_id","week_cate")) %>% 
  reshape2::dcast(acc_id~variable+week_cate)


test1_trade1 = test1_trade%>%
  mutate(source_acc_id = source_acc_id,
         target_acc_id= target_acc_id,
         server = as.factor(server),
         hour =str_sub(time,1,2),
         week_cate = ifelse(day<8,1,ifelse(day<15,2,ifelse(day<22,3,4))),
         hour_cate = ifelse(hour<7,1,ifelse(hour<13,2,ifelse(hour<19,3,4))))%>%
  gather(key="seller_consumer_acc",value="acc_id",'source_acc_id','target_acc_id')%>% 
  as_tibble %>% 
  mutate(char_id=ifelse(seller_consumer_acc=='source_acc_id',source_char_id,target_char_id),
         source_char_id = as.numeric(source_char_id),
         target_char_id = as.factor(target_char_id)) %>% 
  select(-source_char_id,-target_char_id) %>% 
  filter(acc_id %in% unique(test1_activity$acc_id)) %>% 
  group_by(acc_id,item_type) %>%  
  mutate(count_item_type=n()) %>%
  ungroup() %>% 
  group_by(acc_id,hour_cate) %>%  
  mutate(count_hour=n()) %>%
  ungroup() %>% 
  group_by(acc_id ) %>% 
  summarise(sell_cnt = n_distinct(time[seller_consumer_acc == "source_acc_id"]),
            buy_cnt = n_distinct(time[seller_consumer_acc == "target_acc_id"]),
            trade_cnt = n_distinct(time),
            per_sell = sell_cnt / trade_cnt,
            per_buy = buy_cnt / trade_cnt ,
            
            most_trade_item =
              ifelse(n_distinct(item_type[count_item_type== max(count_item_type)])==1,
                     item_type[count_item_type== max(count_item_type)],
                     item_type[count_item_type== max(count_item_type)][1]),
            indiv_trade_cnt = n_distinct(time[type == 1]),
            
            per_indiv_trade = indiv_trade_cnt / trade_cnt, 
            
            most_trade_time = ifelse(n_distinct(hour_cate[count_hour==max(count_hour)])==1,
                                     hour_cate[count_hour==max(count_hour)],
                                     hour_cate[count_hour==max(count_hour)][1])
  )


test1_trade2 = test1_trade%>%
  mutate(source_acc_id = source_acc_id,
         target_acc_id= target_acc_id,
         server = as.factor(server),
         hour =str_sub(time,1,2),
         week_cate = ifelse(day<8,1,ifelse(day<15,2,ifelse(day<22,3,4))),
         hour_cate = ifelse(hour<7,1,ifelse(hour<13,2,ifelse(hour<19,3,4))))%>%
  gather(key="seller_consumer_acc",value="acc_id",'source_acc_id','target_acc_id')%>% 
  as_tibble %>% 
  mutate(char_id=ifelse(seller_consumer_acc=='source_acc_id',source_char_id,target_char_id),
         source_char_id = as.numeric(source_char_id),
         target_char_id = as.factor(target_char_id)) %>% 
  select(-source_char_id,-target_char_id) %>% 
  filter(acc_id %in% unique(test1_activity$acc_id)) %>% 
  group_by(acc_id,item_type) %>%  
  mutate(count_item_type=n()) %>%
  ungroup() %>% 
  group_by(acc_id,hour_cate) %>%  
  mutate(count_hour=n()) %>%
  ungroup() %>% 
  filter(week_cate==4) %>% 
  group_by(acc_id , week_cate) %>% 
  summarise(sell_cnt = n_distinct(time[seller_consumer_acc == "source_acc_id"]),
            buy_cnt = n_distinct(time[seller_consumer_acc == "target_acc_id"]),
            trade_cnt = n_distinct(time),
            
            indiv_trade_cnt = n_distinct(time[type == 1]),
            
            
  )%>% 
  reshape2::melt(id= c("acc_id","week_cate")) %>% 
  reshape2::dcast(acc_id~variable+week_cate)

test1_combat1 = test1_combat %>%  
  mutate(random_att_ratio = ifelse(random_attacker_cnt==0 & random_defender_cnt==0,
                                   0,
                                   random_attacker_cnt/(random_attacker_cnt+random_defender_cnt)),
         week_cate = ifelse(day<8,1,ifelse(day<15,2,ifelse(day<22,3,4))),
         all_zero= rowSums(test1_combat[,7:13])) %>% 
  group_by(acc_id,char_id) %>% 
  mutate(max_level = max(level),
         diff_level=level-lag(level),
         diff_level = ifelse(is.na(diff_level)==T,0,diff_level),
         change_level = ifelse(diff_level==0,0,1)) %>% 
  ungroup() %>% 
  group_by(acc_id) %>% 
  summarise(random_att_ratio_avg=mean(random_att_ratio),
            max_level =max (max_level),
            combat_login_cnt= n_distinct(day[all_zero != 0]),
            n_char_id = n_distinct(char_id),
            diff_level_avg = sum(abs(diff_level))/n_char_id,
            change_level_avg = sum(change_level)/n_char_id,
            
            total_pledge_cnt=sum(pledge_cnt),
            total_random_attacker_cnt=sum(random_attacker_cnt),
            total_random_defender_cnt=sum(random_defender_cnt),
            total_temp_cnt=sum(temp_cnt),
            total_etc_cnt=sum(etc_cnt),
            total_num_opponent=sum(num_opponent)
  ) %>% 
  select(-n_char_id)

test1_combat2 = test1_combat %>%  
  mutate(random_att_ratio = ifelse(random_attacker_cnt==0 & random_defender_cnt==0,
                                   0,
                                   random_attacker_cnt/(random_attacker_cnt+random_defender_cnt)),
         week_cate = ifelse(day<8,1,ifelse(day<15,2,ifelse(day<22,3,4))),
         all_zero= rowSums(test1_combat[,7:13])) %>%
  filter(week_cate == 4) %>% 
  group_by(acc_id,char_id) %>% 
  mutate(max_level = max(level),
         diff_level=level-lag(level),
         diff_level = ifelse(is.na(diff_level)==T,0,diff_level),
         change_level = ifelse(diff_level==0,0,1)) %>% 
  ungroup() %>% 
  group_by(acc_id , week_cate) %>% 
  summarise(total_pledge_cnt=sum(pledge_cnt),
            total_random_attacker_cnt=sum(random_attacker_cnt),
            total_random_defender_cnt=sum(random_defender_cnt),
            total_temp_cnt=sum(temp_cnt),
            total_etc_cnt=sum(etc_cnt),
            total_same_pledge_cnt =sum(same_pledge_cnt),
            total_num_opponent=sum(num_opponent),
            combat_login_cnt= n_distinct(day[all_zero != 0])
            
  ) %>% 
  reshape2::melt(id= c("acc_id","week_cate")) %>% 
  reshape2::dcast(acc_id~variable+week_cate)


test1_payment1=merge(test1_payment,
                     test1_activity1%>% select(acc_id,first_app,login_cnt),
                     by="acc_id",all.x=T) 

test1_payment1 = test1_payment1 %>%
  group_by(acc_id) %>% 
  summarise(pay_yn = ifelse(sum(amount_spent)!=0,1,0),
            total_pay_amount = sum(amount_spent),
            mean_pay_amount = mean(amount_spent),
            
            max_pay_amount= max(amount_spent),
            min_pay_amount = min(amount_spent),
            last_pay_amount = amount_spent[day==max(day) ],
            
            first_app =min(first_app),
            login_cnt= min(login_cnt),
            
            
            first_pay_day= min(day),
            days_since_last_pay = 29-max(day),
            first_pay_from_start = first_pay_day-first_app,
            
            pay_day_cnt= n_distinct(day),
            avail_day= 29- first_app,
            
            pmday_availday_ratio = pay_day_cnt/avail_day,
            pmday_playday_ratio = pay_day_cnt/login_cnt
            
  ) %>% 
  select(-first_app,-login_cnt)

test1_payment2 = test1_payment %>% 
  mutate(week_cate = ifelse(day<8,1,ifelse(day<15,2,ifelse(day<22,3,4)))) %>% 
  filter(week_cate == 4) %>% 
  group_by(acc_id , week_cate) %>% 
  summarise(total_payment = sum(amount_spent),
            payment_cnt = n_distinct(day)
  ) %>% 
  reshape2::melt(id= c("acc_id","week_cate")) %>% 
  reshape2::dcast(acc_id~variable+week_cate)


test1_pledge=merge(test1_pledge,
                   test1_activity%>%
                     select(acc_id,char_id,server,day,playtime),
                   by=c("acc_id","char_id","server","day"),all.x=T) 

test1_pledge1 = test1_pledge %>%
  mutate(week_cate = ifelse(day<8,1,ifelse(day<15,2,ifelse(day<22,3,4)))) %>% 
  group_by(acc_id,char_id) %>% 
  mutate(n_day= n_distinct(day)) %>% 
  ungroup() %>% 
  group_by(acc_id) %>% 
  summarise(main_char_id = ifelse(n_distinct(char_id[playtime == max(playtime)]==1),
                                  char_id[playtime == max(playtime)],
                                  char_id[n_day==max(n_day)]),
            pl_play_char_cnt = mean(play_char_cnt[char_id==main_char_id]),
            pl_combat_char_cnt = mean(combat_char_cnt[char_id == main_char_id]),
            
            pl_pledge_combat_cnt = mean(pledge_combat_cnt[char_id == main_char_id]),
            pl_random_attacker_cnt = mean(random_attacker_cnt[char_id == main_char_id]),
            pl_random_defender_cnt = mean(random_defender_cnt[char_id == main_char_id]),
            pl_same_pledge_cnt = mean(same_pledge_cnt[char_id == main_char_id]),
            pl_temp_cnt = mean(temp_cnt[char_id == main_char_id]),
            pl_etc_cnt= mean(etc_cnt[char_id == main_char_id]),
            pl_combat_play_time=mean(combat_play_time[char_id == main_char_id]),
            pl_combat_char_ratio = pl_combat_char_cnt/pl_play_char_cnt,
            pl_pledge_cnt= n_distinct(pledge_id)) %>% 
  select(-main_char_id)

test1_activity3 = test1_activity %>% 
  select(acc_id,playtime,solo_exp,party_exp,quest_exp,day) %>%
  mutate(tend_playtime =(playtime -  mean(playtime))/sd(playtime), 
         tend_soloexp = (solo_exp -  mean(solo_exp))/sd(solo_exp),
         tend_partyexp = (party_exp -mean(party_exp))/sd(party_exp),
         tend_questexp = (quest_exp -mean(quest_exp))/sd(quest_exp),
         max_time= max(playtime)
  ) %>% 
  group_by(acc_id) %>% 
  summarise(tend_playtime = mean(tend_playtime),
            tend_soloexp = mean(tend_soloexp),
            tend_partyexp = mean(tend_partyexp),
            tend_questexp = mean(tend_questexp),
            maxplay_cnt = n_distinct(day[playtime ==max_time]))


test1_combat3 = test1_combat %>% 
  select(acc_id,pledge_cnt,etc_cnt,num_opponent) %>%
  mutate(tend_pledge_cnt =(pledge_cnt -  mean(pledge_cnt))/sd(pledge_cnt), 
         tend_etc_cnt = (etc_cnt -  mean(etc_cnt))/sd(pledge_cnt),
         tend_num_opponent = (num_opponent -  mean(num_opponent))/sd(num_opponent)
  ) %>% 
  group_by(acc_id) %>% 
  summarise(tend_pledge_cnt = mean(tend_pledge_cnt),
            tend_etc_cnt = mean(tend_etc_cnt),
            tend_num_opponent=mean(tend_num_opponent))

test1=merge(test1_activity1,test1_trade1,by="acc_id",all = T)
test1=merge(test1,test1_combat1,by="acc_id",all = T)
test1=merge(test1,test1_payment1,by="acc_id",all = T)
test1=merge(test1,test1_pledge1,by="acc_id",all = T)
test1=merge(test1,test1_activity2,by="acc_id",all=T)
test1=merge(test1,test1_activity3,by="acc_id",all = T)
test1=merge(test1,test1_payment2,by="acc_id",all = T)
test1=merge(test1,test1_trade2,by="acc_id",all = T)
test1=merge(test1,test1_combat2,by="acc_id",all = T)
test1=merge(test1,test1_combat3,by="acc_id",all = T)

test1 = test1 %>% 
  mutate(  most_trade_time =as.numeric( most_trade_time),
           most_trade_item = ifelse(most_trade_item=="etc",1,
                                    ifelse(most_trade_item=="accessory",2,
                                           ifelse(most_trade_item=="adena",3,
                                                  ifelse(most_trade_item=="enchant_scroll",4,
                                                         ifelse(most_trade_item=="spell",5,
                                                                ifelse(most_trade_item=="weapon",6,
                                                                       ifelse(most_trade_item=="armor",7,0))))))),
           
           
           
           
           avail_day = ifelse(is.na(avail_day)==TRUE,
                              29-first_app,
                              avail_day),
           
           days_since_last_pay = ifelse(is.na(days_since_last_pay)==TRUE,
                                        29,
                                        days_since_last_pay),
           
           first_pay_from_start = ifelse(is.na(first_pay_from_start)==TRUE,
                                         -first_app,
                                         first_pay_from_start)
  )  %>%
  mutate_at(vars(c(colnames(test1))[c(1:62,65,67:ncol(test1))]),function(x){ifelse(is.na(x)==TRUE,0,x)}) 

test1$most_trade_item
write.csv(test1,file="test1_preprocess.csv",row.names=FALSE)

#####################################################################################
#######################      test2      #############################################
#####################################################################################


test2_activity1 = test2_activity%>%
  mutate(server = as.factor(server),
         char_id = as.factor(char_id),
         fishing = ifelse(playtime>0,fishing,0),
         private_shop= ifelse(playtime>0,private_shop,0),
         week_cate = ifelse(day<8,1,ifelse(day<15,2,ifelse(day<22,3,4)))) %>% 
  group_by(acc_id) %>% 
  summarize(login_cnt= n_distinct(day),
            char_cnt = n_distinct(char_id),
            total_playtime = sum(playtime),
            
            conseq_log = conseq(day,1),
            conseq_nolog = no_conseq(day,1),
            first_app = min(day),
            
            start_week = min(week_cate),
            per_log = login_cnt/(29-first_app),
            switchlog_cnt = switch(day),
            
            server_cnt = n_distinct(server),
            total_npckill=sum(npc_kill),
            mean_npckill=mean(npc_kill),
            
            total_soloexp = sum(solo_exp),
            total_partyexp=sum(party_exp),
            total_questexp=sum(quest_exp),
            
            soloexp_cnt= n_distinct(day[solo_exp!=0]),
            partyexp_cnt=n_distinct(day[party_exp!=0]),
            questexp_cnt=n_distinct(day[quest_exp!=0]),
            
            per_soloday = soloexp_cnt/login_cnt,
            per_partyday =partyexp_cnt/login_cnt,
            per_questday = questexp_cnt/login_cnt,
            
            total_bossattack = sum(rich_monster),
            bossattack_cnt = n_distinct(day[rich_monster!=0]),
            per_bossattack = bossattack_cnt/login_cnt,
            
            
            
            total_death= sum(death),
            total_revive = sum(revive),
            total_recovery = sum(exp_recovery),
            
            total_fishing = sum(fishing),
            per_fishing= total_fishing/total_playtime,
            
            total_privateshop = sum(private_shop),
            per_privateshop = total_privateshop / total_playtime,
            
            total_enchant= sum(enchant_count),
            total_moneychange = sum(game_money_change),
            total_moneychange_abs = sum(abs(game_money_change)))

test2_activity2 = test2_activity%>%
  mutate(server = as.factor(server),
         char_id = as.factor(char_id),
         fishing = ifelse(playtime>0,fishing,0),
         private_shop= ifelse(playtime>0,private_shop,0),
         week_cate = ifelse(day<8,1,ifelse(day<15,2,ifelse(day<22,3,4)))) %>%
  filter(week_cate == 4) %>% 
  group_by(acc_id,week_cate) %>% 
  summarize(login_cnt= n_distinct(day),
            total_playtime = sum(playtime),
            mean_playtime = mean(playtime),
            
            server_cnt = n_distinct(server),
            total_npckill=sum(npc_kill),
            mean_npckill=mean(npc_kill),
            
            
            total_soloexp = sum(solo_exp),
            soloexp_cnt= n_distinct(day[solo_exp!=0]),
            
            total_partyexp=sum(party_exp),
            partyexp_cnt=n_distinct(day[party_exp!=0]),
            
            total_questexp=sum(quest_exp),
            questexp_cnt=n_distinct(day[quest_exp!=0]),
            
            per_soloday = soloexp_cnt/login_cnt,
            per_partyday =partyexp_cnt/login_cnt,
            per_questday = questexp_cnt/login_cnt,
            
            total_bossattack = sum(rich_monster),
            bossattack_cnt = n_distinct(day[rich_monster!=0]),
            total_death= sum(death),
            per_bossattack = bossattack_cnt/login_cnt,
            
            total_revive = sum(revive),
            total_recovery = sum(exp_recovery),
            
            total_fishing = sum(fishing),
            per_fishing= total_fishing/total_playtime,
            
            total_privateshop = sum(private_shop),
            per_privateshop = total_privateshop / total_playtime,
            
            total_enchant= sum(enchant_count),
            total_moneychange = sum(game_money_change),
            total_moneychange_abs = sum(abs(game_money_change)),
            char_id = n_distinct(char_id)
  ) %>% 
  reshape2::melt(id= c("acc_id","week_cate")) %>% 
  reshape2::dcast(acc_id~variable+week_cate)


test2_trade1 = test2_trade%>%
  mutate(source_acc_id = source_acc_id,
         target_acc_id= target_acc_id,
         server = as.factor(server),
         hour =str_sub(time,1,2),
         week_cate = ifelse(day<8,1,ifelse(day<15,2,ifelse(day<22,3,4))),
         hour_cate = ifelse(hour<7,1,ifelse(hour<13,2,ifelse(hour<19,3,4))))%>%
  gather(key="seller_consumer_acc",value="acc_id",'source_acc_id','target_acc_id')%>% 
  as_tibble %>% 
  mutate(char_id=ifelse(seller_consumer_acc=='source_acc_id',source_char_id,target_char_id),
         source_char_id = as.numeric(source_char_id),
         target_char_id = as.factor(target_char_id)) %>% 
  select(-source_char_id,-target_char_id) %>% 
  filter(acc_id %in% unique(test2_activity$acc_id)) %>% 
  group_by(acc_id,item_type) %>%  
  mutate(count_item_type=n()) %>%
  ungroup() %>% 
  group_by(acc_id,hour_cate) %>%  
  mutate(count_hour=n()) %>%
  ungroup() %>% 
  group_by(acc_id ) %>% 
  summarise(sell_cnt = n_distinct(time[seller_consumer_acc == "source_acc_id"]),
            buy_cnt = n_distinct(time[seller_consumer_acc == "target_acc_id"]),
            trade_cnt = n_distinct(time),
            per_sell = sell_cnt / trade_cnt,
            per_buy = buy_cnt / trade_cnt ,
            
            most_trade_item =
              ifelse(n_distinct(item_type[count_item_type== max(count_item_type)])==1,
                     item_type[count_item_type== max(count_item_type)],
                     item_type[count_item_type== max(count_item_type)][1]),
            indiv_trade_cnt = n_distinct(time[type == 1]),
            
            per_indiv_trade = indiv_trade_cnt / trade_cnt, 
            
            most_trade_time = ifelse(n_distinct(hour_cate[count_hour==max(count_hour)])==1,
                                     hour_cate[count_hour==max(count_hour)],
                                     hour_cate[count_hour==max(count_hour)][1])
  )


test2_trade2 = test2_trade%>%
  mutate(source_acc_id = source_acc_id,
         target_acc_id= target_acc_id,
         server = as.factor(server),
         hour =str_sub(time,1,2),
         week_cate = ifelse(day<8,1,ifelse(day<15,2,ifelse(day<22,3,4))),
         hour_cate = ifelse(hour<7,1,ifelse(hour<13,2,ifelse(hour<19,3,4))))%>%
  gather(key="seller_consumer_acc",value="acc_id",'source_acc_id','target_acc_id')%>% 
  as_tibble %>% 
  mutate(char_id=ifelse(seller_consumer_acc=='source_acc_id',source_char_id,target_char_id),
         source_char_id = as.numeric(source_char_id),
         target_char_id = as.factor(target_char_id)) %>% 
  select(-source_char_id,-target_char_id) %>% 
  filter(acc_id %in% unique(test2_activity$acc_id)) %>% 
  group_by(acc_id,item_type) %>%  
  mutate(count_item_type=n()) %>%
  ungroup() %>% 
  group_by(acc_id,hour_cate) %>%  
  mutate(count_hour=n()) %>%
  ungroup() %>% 
  filter(week_cate==4) %>% 
  group_by(acc_id , week_cate) %>% 
  summarise(sell_cnt = n_distinct(time[seller_consumer_acc == "source_acc_id"]),
            buy_cnt = n_distinct(time[seller_consumer_acc == "target_acc_id"]),
            trade_cnt = n_distinct(time),
            
            indiv_trade_cnt = n_distinct(time[type == 1]),
            
            
  )%>% 
  reshape2::melt(id= c("acc_id","week_cate")) %>% 
  reshape2::dcast(acc_id~variable+week_cate)

test2_combat1 = test2_combat %>%  
  mutate(random_att_ratio = ifelse(random_attacker_cnt==0 & random_defender_cnt==0,
                                   0,
                                   random_attacker_cnt/(random_attacker_cnt+random_defender_cnt)),
         week_cate = ifelse(day<8,1,ifelse(day<15,2,ifelse(day<22,3,4))),
         all_zero= rowSums(test2_combat[,7:13])) %>% 
  group_by(acc_id,char_id) %>% 
  mutate(max_level = max(level),
         diff_level=level-lag(level),
         diff_level = ifelse(is.na(diff_level)==T,0,diff_level),
         change_level = ifelse(diff_level==0,0,1)) %>% 
  ungroup() %>% 
  group_by(acc_id) %>% 
  summarise(random_att_ratio_avg=mean(random_att_ratio),
            max_level =max (max_level),
            combat_login_cnt= n_distinct(day[all_zero != 0]),
            n_char_id = n_distinct(char_id),
            diff_level_avg = sum(abs(diff_level))/n_char_id,
            change_level_avg = sum(change_level)/n_char_id,
            
            total_pledge_cnt=sum(pledge_cnt),
            total_random_attacker_cnt=sum(random_attacker_cnt),
            total_random_defender_cnt=sum(random_defender_cnt),
            total_temp_cnt=sum(temp_cnt),
            total_etc_cnt=sum(etc_cnt),
            total_num_opponent=sum(num_opponent)
  ) %>% 
  select(-n_char_id)

test2_combat2 = test2_combat %>%  
  mutate(random_att_ratio = ifelse(random_attacker_cnt==0 & random_defender_cnt==0,
                                   0,
                                   random_attacker_cnt/(random_attacker_cnt+random_defender_cnt)),
         week_cate = ifelse(day<8,1,ifelse(day<15,2,ifelse(day<22,3,4))),
         all_zero= rowSums(test2_combat[,7:13])) %>%
  filter(week_cate == 4) %>% 
  group_by(acc_id,char_id) %>% 
  mutate(max_level = max(level),
         diff_level=level-lag(level),
         diff_level = ifelse(is.na(diff_level)==T,0,diff_level),
         change_level = ifelse(diff_level==0,0,1)) %>% 
  ungroup() %>% 
  group_by(acc_id , week_cate) %>% 
  summarise(total_pledge_cnt=sum(pledge_cnt),
            total_random_attacker_cnt=sum(random_attacker_cnt),
            total_random_defender_cnt=sum(random_defender_cnt),
            total_temp_cnt=sum(temp_cnt),
            total_etc_cnt=sum(etc_cnt),
            total_same_pledge_cnt =sum(same_pledge_cnt),
            total_num_opponent=sum(num_opponent),
            combat_login_cnt= n_distinct(day[all_zero != 0])
            
  ) %>% 
  reshape2::melt(id= c("acc_id","week_cate")) %>% 
  reshape2::dcast(acc_id~variable+week_cate)


test2_payment1=merge(test2_payment,
                     test2_activity1%>% select(acc_id,first_app,login_cnt),
                     by="acc_id",all.x=T) 

test2_payment1 = test2_payment1 %>%
  group_by(acc_id) %>% 
  summarise(pay_yn = ifelse(sum(amount_spent)!=0,1,0),
            total_pay_amount = sum(amount_spent),
            mean_pay_amount = mean(amount_spent),
            
            max_pay_amount= max(amount_spent),
            min_pay_amount = min(amount_spent),
            last_pay_amount = amount_spent[day==max(day) ],
            
            first_app =min(first_app),
            login_cnt= min(login_cnt),
            
            
            first_pay_day= min(day),
            days_since_last_pay = 29-max(day),
            first_pay_from_start = first_pay_day-first_app,
            
            pay_day_cnt= n_distinct(day),
            avail_day= 29- first_app,
            
            pmday_availday_ratio = pay_day_cnt/avail_day,
            pmday_playday_ratio = pay_day_cnt/login_cnt) %>% 
  select(-first_app,-login_cnt)

test2_payment2 = test2_payment %>% 
  mutate(week_cate = ifelse(day<8,1,ifelse(day<15,2,ifelse(day<22,3,4)))) %>% 
  filter(week_cate == 4) %>% 
  group_by(acc_id , week_cate) %>% 
  summarise(total_payment = sum(amount_spent),
            payment_cnt = n_distinct(day)
  ) %>% 
  reshape2::melt(id= c("acc_id","week_cate")) %>% 
  reshape2::dcast(acc_id~variable+week_cate)


test2_pledge=merge(test2_pledge,
                   test2_activity%>%
                     select(acc_id,char_id,server,day,playtime),
                   by=c("acc_id","char_id","server","day"),all.x=T) 

test2_pledge1 = test2_pledge %>%
  mutate(week_cate = ifelse(day<8,1,ifelse(day<15,2,ifelse(day<22,3,4)))) %>% 
  group_by(acc_id,char_id) %>% 
  mutate(n_day= n_distinct(day)) %>% 
  ungroup() %>% 
  group_by(acc_id) %>% 
  summarise(main_char_id = ifelse(n_distinct(char_id[playtime == max(playtime)]==1),
                                  char_id[playtime == max(playtime)],
                                  char_id[n_day==max(n_day)]),
            pl_play_char_cnt = mean(play_char_cnt[char_id==main_char_id]),
            pl_combat_char_cnt = mean(combat_char_cnt[char_id == main_char_id]),
            
            pl_pledge_combat_cnt = mean(pledge_combat_cnt[char_id == main_char_id]),
            pl_random_attacker_cnt = mean(random_attacker_cnt[char_id == main_char_id]),
            pl_random_defender_cnt = mean(random_defender_cnt[char_id == main_char_id]),
            pl_same_pledge_cnt = mean(same_pledge_cnt[char_id == main_char_id]),
            pl_temp_cnt = mean(temp_cnt[char_id == main_char_id]),
            pl_etc_cnt= mean(etc_cnt[char_id == main_char_id]),
            pl_combat_play_time=mean(combat_play_time[char_id == main_char_id]),
            pl_combat_char_ratio = pl_combat_char_cnt/pl_play_char_cnt,
            pl_pledge_cnt= n_distinct(pledge_id)) %>% 
  select(-main_char_id)

test2_activity3 = test2_activity %>% 
  select(acc_id,playtime,solo_exp,party_exp,quest_exp,day) %>%
  mutate(tend_playtime =(playtime -  mean(playtime))/sd(playtime), 
         tend_soloexp = (solo_exp -  mean(solo_exp))/sd(solo_exp),
         tend_partyexp = (party_exp -mean(party_exp))/sd(party_exp),
         tend_questexp = (quest_exp -mean(quest_exp))/sd(quest_exp),
         max_time= max(playtime)) %>% 
  group_by(acc_id) %>% 
  summarise(tend_playtime = mean(tend_playtime),
            tend_soloexp = mean(tend_soloexp),
            tend_partyexp = mean(tend_partyexp),
            tend_questexp = mean(tend_questexp),
            maxplay_cnt = n_distinct(day[playtime ==max_time]))


test2_combat3 = test2_combat %>% 
  select(acc_id,pledge_cnt,etc_cnt,num_opponent) %>%
  mutate(tend_pledge_cnt =(pledge_cnt -  mean(pledge_cnt))/sd(pledge_cnt), 
         tend_etc_cnt = (etc_cnt -  mean(etc_cnt))/sd(pledge_cnt),
         tend_num_opponent = (num_opponent -  mean(num_opponent))/sd(num_opponent)) %>% 
  group_by(acc_id) %>% 
  summarise(tend_pledge_cnt = mean(tend_pledge_cnt),
            tend_etc_cnt = mean(tend_etc_cnt),
            tend_num_opponent=mean(tend_num_opponent))

test2=merge(test2_activity1,test2_trade1,by="acc_id",all = T)
test2=merge(test2,test2_combat1,by="acc_id",all = T)
test2=merge(test2,test2_payment1,by="acc_id",all = T)
test2=merge(test2,test2_pledge1,by="acc_id",all = T)
test2=merge(test2,test2_activity2,by="acc_id",all=T)
test2=merge(test2,test2_activity3,by="acc_id",all = T)
test2=merge(test2,test2_payment2,by="acc_id",all = T)
test2=merge(test2,test2_trade2,by="acc_id",all = T)
test2=merge(test2,test2_combat2,by="acc_id",all = T)
test2=merge(test2,test2_combat3,by="acc_id",all = T)


test2 = test2 %>% 
  mutate( most_trade_time =as.numeric( most_trade_time),
          most_trade_item = ifelse(most_trade_item=="etc",1,
                                   ifelse(most_trade_item=="accessory",2,
                                          ifelse(most_trade_item=="adena",3,
                                                 ifelse(most_trade_item=="enchant_scroll",4,
                                                        ifelse(most_trade_item=="spell",5,
                                                               ifelse(most_trade_item=="weapon",6,
                                                                      ifelse(most_trade_item=="armor",7,0))))))),
          
          avail_day = ifelse(is.na(avail_day)==TRUE,
                              29-first_app,
                              avail_day),
           
           days_since_last_pay = ifelse(is.na(days_since_last_pay)==TRUE,
                                        29,
                                        days_since_last_pay),
           
           first_pay_from_start = ifelse(is.na(first_pay_from_start)==TRUE,
                                         -first_app,
                                         first_pay_from_start)
  ) %>% 
  mutate_at(vars(c(colnames(test2))[c(1:62,65,67:ncol(test2))]),function(x){ifelse(is.na(x)==TRUE,0,x)})

write.csv(test2,file="test2_preprocess.csv",row.names=FALSE)
