#rule in loop - higher number is coded as over/under and lower number coded as spread

# adding 0 before date
str_pad(your_column, 4, pad = "0")
format(column, width = 4)
df$var1 <- ifelse(len(df$var1) = 3, paste("0", df$var), df$var)

#none above works


new_df = sqldf(`select var1, var2,... if(len(date)==3, concat("0",date),date) as date from my_df`)


#rename column names
colnames(data)[colnames(data)=="old_name"] <- "new_name"
colnames(nba_odds_17_18)[colnames(nba_odds_17_18) == "1st"] <- "First"
names(data)[3]<-"new_name"
#multiple column names at one
setnames(data, old=c("old_name","another_old_name"), new=c("new_name", "another_new_name"))
#Sql light for concatenating database
sqldf("SELECT CASE $myCol
                    WHEN 1 THEN 'Yes'
      WHEN 2 THEN 'No' 
      WHEN 99 THEN 'NA' 
      ELSE 'Name ittt' 
      END as newCol
      FROM datwe")

#SQL Lite replace
select  locationname || '<p>' from location;

#Apply function to add 0 to cells with strings less than 4 characters in DATE column 
apply(data, 1, function(x) {ifelse(any(x == 0), NA, length(unique(x)))})

sapply(df$col, function(x) sum(length(which(is.na(x)))))  

#Pad cells with 0's to length 4
str_pad(x, 8, pad = "0")
sprintf("%02d",DB$HOUR)
mutate(a = if_else(nchar(a)==2, str_pad(a,width=3,side="left",pad="0"), a))
mutate(nba_odds_17_18_Date = if_else((nchar(nba_odds_17_18_Date) == 3, str_pad(nba_odds_17_18_Date, width = 4, side = "left", pad = "0"), nba_odds_17_18_Date)))
X$A <- ifelse(nchar(X$A) < 5, paste(c(rep("0",5-nchar(X$A)), X$A), collapse=""), X$A)
X$A <- ifelse(nchar(X$A) < 5, paste("0", X$A, sep=""), X$A)

#all else above failed, this is the correct code that padded "0" to cells in nba_odds_17_18$Date 
nba_odds_17_18$Date <- ifelse(nchar(nba_odds_17_18$Date) < 4, paste("0", nba_odds_17_18$Date, sep = ""), nba_odds_17_18$Date)

#format nba_odds_17_18$Date as Date class
as.Date(x, format, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"),
        optional = FALSE, ...)

betterDates <- as.Date(dates,
                       format = "%m/%d/%y")

#
paste(my_name_vector, "2")
my_name_vector <-
  colnames(dataframe)
#used for once have list created
data_frame[list_of_cols] <- NA

#creating value with new columns from vector 
nba_odds_17_18[name_vector] <- NA

for(i in namevector)
  df[,i] <- NA

#new column to create differences between closing line vs actual result
df.res <- transform(df, new.col = U - mean)

#adding year to date column
nba_odds_17_18$Date <- ifelse(starts_with(nba_odds_17_18$Date) = "0", paste("18", nba_odds_17_18$Date, sep = ""), nba_odds_17_18$Date)

#Separating columns by V
visitors <- sqldf("select * from my_table where VH = 'V'")

#separating columns by H
home <- sqldf("select * from my_table where VH = 'H'")

#merge tables
flattened_table <- sqldf("select * from visitors join home on vistitors.my_index = home.my_index")
all_games <- sqldf("select * from visitors join home on visitors.game_index = home.game_index")


#creating an indexing column
data.frame$generated_uid <- 1:nrow(data.frame)
visitors$game_index <- 1:nrow(visitors)
home$game_index <- 1:nrow(home)

#visualization of difference from opening spread line to closing spread line


all_games$over_under_difference <- c(all_games$Open - all_games$Close)
View(all_games)


#swap cells that weren't correctly placed
df %>% mutate(swap_if(V2 == "b", V1, V3))
all_games %>% mutate(swap_if(all_games$Open < 100, all_games$Open, all_games$Open..24))

dt[V2=="b", c("V3", "V1") := .(V1, V3)]
all_games[all_games$Open < "100", c("all_games$Open..24", "all_games$Open") := .(all_games$Open, all_games$Open..24)]

#none of above worked
df <- within(df, est[b==0] <- (a[b==0]-5)/2.533)

df$est[df$b == 0] <- (df$a[df$b == 0] - 5)/2.533 
df %>%
  mutate(g = ifelse(a == 2 | a == 5 | a == 7 | (a == 1 & b == 4), 2,
                    ifelse(a == 0 | a == 1 | a == 4 | a == 3 |  c == 4, 3, NA)))

df %>% mutate(g = case_when(a == 2 | a == 5 | a == 7 | (a == 1 & b == 4) ~ 2,
                            a == 0 | a == 1 | a == 4 | a == 3 |  c == 4 ~ 3,
                            TRUE ~ NA_real_))

#none of these worked

df[df$V2 == "b", c("V1", "V3")] <- df[df$V2 == "b", c("V3", "V1")] 
df <- transform(df, V3 = ifelse(V2 == 'b', V1, V3), V1 = ifelse(V2 == 'b', V3, V1))
a = a + b
b = a - b
a = a - b
# 3 lines above would work if could put work on a condition

# loop to swap values into correct columns for spread and over/under 
for i in df:
  
  1) create 4 variables and save values from 4 wrong cells to those variables
2) reassign variables
for i in df:
  var 1 = df[wrong_var1][i]
  df[wrong_var1][i] = name_of_correct_variable1

  
for (i in all_games_2){
    o_u_open = all_games_2[all_games_2$Open][i]
    o_u_close = all_games_2[all_games_2$Close][i]
    spread_open = all_games_2[all_games_2$Open..24][i]
    spread_close = all_games_2[all_games_2$Close..25][i]
    ifelse(all_games_2$Open < 100, all_games_2$Open[i] == spread_open & all_games_2$Close[i] == spread_close & all_games_2$Open..24[i] == o_u_open & all_games_2$Close..25[i] == o_u_close, break)
}
  
  
  for (i in all_games_2){
    o_u_open = all_games_2[all_games_2$`OverUnder Open`][i]
    o_u_close = all_games_2[all_games_2$`OverUnder Close`][i]
    spread_open = all_games_2[all_games_2$'Spread Open'][i]
    spread_close = all_games_2[all_games_2$`Spread Close`][i]
    ifelse(all_games_2$`OverUnder Open` < 100, all_games_2$`OverUnder Open`[i] == spread_open & all_games_2$`OverUnder Close`[i] == spread_close & all_games_2$`Spread Open`[i] == o_u_open & all_games_2$`OverUnder Close`[i] == spread_close, break)
  }
  
 for (i in all_games_2){
    o_u_open = all_games_2$OverUnder_Open[i]
    o_u_close = all_games_2$OverUnder_Close[i]
    spread_open = all_games_2$Spread_Open[i]
    spread_close = all_games_2$Spread_Close[i]
    ifelse(all_games_2$OverUnder_Open < 100, all_games_2$OverUnder_Open[i] == spread_open & all_games_2$OverUnder_Close[i] == spread_close & all_games_2$Spread_Open[i] == o_u_open & all_games_2$OverUnder_Close[i] == spread_close, break)
    }
  
   for (i in all_games_2){
        o_u_open = all_games_2[all_games_2$`OverUnder_Open`][i]
        o_u_close = all_games_2[all_games_2$`OverUnder_Close`][i]
        spread_open = all_games_2[all_games_2$`Spread_Open`][i]
        spread_close = all_games_2[all_games_2$`Spread_Close`][i]
        ifelse(all_games_2$`OverUnder_Open` < 100, all_games_2$`OverUnder_Open`[i] == spread_open & all_games_2$`OverUnder_Close`[i] == spread_close & all_games_2$`Spread_Open`[i] == o_u_open & all_games_2$`OverUnder_Close`[i] == spread_close, break)
    }
  
#two error messages from line below, one regarding length of value in cell.  
  > for (i in all_games_2){
    +     o_u_open = all_games_2$OverUnder_Open[i]
    +     o_u_close = all_games_2$OverUnder_Close[i]
    +     spread_open = all_games_2$Spread_Open[i]
    +     spread_close = all_games_2$Spread_Close[i]
    +     if (all_games_2$OverUnder_Open[i] < 100.0) {
      +         all_games_2$OverUnder_Open[i] = spread_open 
      +         all_games_2$OverUnder_Close[i] = spread_close 
      +         all_games_2$Spread_Open[i] = o_u_open 
      +         all_games_2$OverUnder_Close[i] = spread_close}
    +     else {
      +         break}
    + }
  
#renaming column names from merging
  colnames(X)[2] <- "superduper"
  colnames(all_games_2)[24] <- "Spread Open"
  colnames(all_games_2)[25] <- "Spread Close"
  colnames(all_games_2)[10] <- "OverUnder Open"
  colnames(all_games_2)[11] <- "OverUnder Close"
  
  > colnames(all_games)[24] <- "Spread_Open"
  > colnames(all_games)[25] <- "Spread_Close"
  > colnames(all_games)[10] <- "OverUnder_Open"
  > colnames(all_games)[11] <- "OverUnder_Close"

  library("openxlsx", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
  write.xlsx(all_games_2, 'all_games_2.xlsx')
  
  #still trying to swap values
  df <- transform(df, V3 = ifelse(V2 == 'b', V1, V3), V1 = ifelse(V2 == 'b', V3, V1))
  #doesn't like "=" 
  df[df$V2 == "b", c("V1", "V3")] <- df[df$V2 == "b", c("V3", "V1")]
  
  
#warning message: the condition has length > 1 and only the first element will be used
for (i in all_games_2){
  o_u_open = all_games_2$OverUnder_Open[i]
  o_u_close = all_games_2$OverUnder_Close[i]
  spread_open = all_games_2$Spread_Open[i]
  spread_close = all_games_2$Spread_Close[i]
  if (all_games_2$ML < 0.0) {
    all_games_2$OverUnder_Open[i] = spread_open 
    all_games_2$OverUnder_Close[i] = spread_close 
    all_games_2$Spread_Open[i] = o_u_open 
    all_games_2$OverUnder_Close[i] = spread_close}
  else {
    break}
}

#creating new columns and renaming final scores for home and away team
  
  colnames(all_games_3)[9] <- "Visitor Final"
  colnames(all_games_3)[23] <- "Home Final"
  all_games_3$Game_Total <- all_games_3$`Visitor Final` + all_games_3$`Home Final`
  all_games_3$actual_over_under_difference <- all_games_3$OverUnder_Close - all_games_3$Game_Total
  
  
  > all_games_3$First <- NULL
  > View(all_games_3)
  > all_games_3$Second <- NULL
  > all_games_3$Third <- NULL
  > all_games_3$Fourth <- NULL
  > all_games_3$First..19 <- NULL
  > all_games_3$Second..20 <- NULL
  > all_games_3$Third..21 <- NULL
  > all_games_3$Fourth..22 <- NULL
  > View(all_games_3)
  > all_games_3$`2H` <- NULL
  > all_games_3$`2H..27` <- NULL
  > View(all_games_3)
  > colnames(all_games_3)[4] <- "Visitor Team"
  > colnames(all_games_3)[18] <- "Home Team"
  > View(all_games_3)
  > colnames(all_games_3)[18] <- "game_index_second_table"
  > colnames(all_games_3)[13] <- "Home Team"
  > View(all_games_3)
  > colnames(all_games_3)[11] <- "Rot_2nd_Table"
  > View(all_games_3)
  > colnames(all_games_3)[10] <- "Date_Continued"
  
  new.Freq <- with(mydata, ifelse(Var1 <= 1, -Freq, Freq))
  
  index <- mydata$Var1 <= 1
  mydata$Freq[index] = -abs(mydata$Freq[index])
  
#correctly aligning the spread to the favorite 
ml_index <- all_games_3$`Home ML` < 0
all_games_3$Spread_Open[ml_index] = -abs(all_games_3$Spread_Open[ml_index])

ml_index <- all_games_3$`Home ML` < 0
all_games_3$Spread_Close[ml_index] = -abs(all_games_3$Spread_Close[ml_index])
  

  
