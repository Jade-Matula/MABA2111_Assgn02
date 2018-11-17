
#**************R Assignment No. 1*******************************************
#**Jade A. Matula                                                         ** 
#** Due on Nov 7, 2018, 5PM                                               **
#**                                                                       **
#***************************************************************************

#1. The WHO Dataset

WHODATA<- read.csv('WHO.csv')

##Check first the first sets of rows of the data

head(WHODATA,6)

#d. Country with the lowest literacy


lOWEST_lit <- min(WHODATA$LiteracyRate, na.rm= TRUE)

lowest_lit

row_lowest_lit <- subset(WHODATA, LiteracyRate == lOWEST_lit)

print (row_lowest_lit$Country)


#e. Richest country in Europe based on GNI

Europe <- subset (WHODATA, Region == 'Europe')
print (Europe)

Max_GNI_Europe <-max(Europe$GNI, na.rm = TRUE)
Max_GNI_Europe

row_with_MAX_GNI_Europe <-subset(Europe, GNI == Max_GNI_Europe)

print(row_with_MAX-GNI_Europe$Country)


#f. Mean Life expectancy of coutries in Africa

AFRICA <- subset(WHODATA, Region == 'Africa')

print(AFRICA)

Mean_Life_Expectancy_Africa <-mean(AFRICA$LifeExpectancy, na.rm= TRUE)

print(Mean_Life_Expectancy_Africa)

#g.Number of countries with population greater than 10,000,000

Countries_W_Pop_Over_10M <- subset(WHODATA, Population > 10000 )

dim(Countries_W_Pop_Over_10M)[1]


#h. Top 5 countries in the Americas with the highest child mortality

AMERICAS <- subset(WHODATA, Region == 'Americas')

Child_mortality_index_inorder <- order(AMERICAS$ChildMortality, decreasing= TRUE)

AMERICAS_order_by_Child_Mortality <-AMERICAS[Child_mortality_index_inorder,]

Head_Top5_Child_Mortality_Americas <- head(AMERICAS_order_by_Child_Mortality,5)

Index_Head_Top5_Child_Mortality_Americas <- AMERICAS_order_by_Child_Mortality[1:5, ]

Index_Head_Top5_Child_Mortality_Americas$Country





#********************************************************************************************************************#

# 2. NBA dataset (Historical NBA Performance)

install.packages("readxl")

library('readxl')

#a. The year Bulls has the highest winning percentage

NBA_HISTORY <- read_excel('Historical NBA Performance.xlsx')

BULLS <- subset(NBA_HISTORY, Team == 'Bulls')

Highest_WinPer_Bulls <- max(BULLS$Winning Percentage')
                            
Row_Highest_WinPer_Bulls <- subset(BULLS, 'Winning Percentage' == Highest_WinPer_Bulls)
                            
Row_Highest_WinPer_Bulls$Year
                            
                            
 #b. Teams with an even win-loss record in a year
                            
    TEAM_w_Even_WinLoss <- subset(NBA_HISTORY, 'Winning Percentage' == 0.5)
    TEAM_w_Even_WinLoss
                            
#*******************************************************************************************************************#
                            
                            
#3. Seasons_Stats.csv
                            
    install.packages('tidyuniverse')
                            
                            
    SEASON_STAT_DATA <- read.csv ('SeasonS_Stats.csv')
                            
    head(SEASON_STAT_DATA)
                            
  #a. Player with the highest 3-pt attempt rate in a season
                            
     library ("tidyuniverse")
                            
     Combined_Season_Stats <- SEASON_STAT_DATA %>%
     select(Year, Player, X3PA, X2PA) %>%
    filter(Year > 1979) %>%
    group_by(Year, Player) %>%
    summarize(X3PA = sum(X3PA), X2PA = sum(X2PA)) %>%
    mutate(X3PAr= X3PA/ (X3PA + X2PA))
  subset(Combined_Season_Stats, X3PAr == max(Combined_Season_Stats, na.rm = TRUE))
                            
                            
#b. Plater with the highest free throw rate in a season
                            
    Combined_Season_Stats_Freethrow <- SEASON_STAT_DATA %>%
    select(Year, Player, FTA, FGA) %>%
    group_by (Year, Player) %>%
    summarize(FTA= sum(FTA), FGA = sum (FGA)) %>%
    mutate(FTr = FTA/FGA)
    Combined_Season_Stats_Freethrow <- Combined_Season_Stats_Freethrow[!is.infinite(Combined_Season_Stats_Freethrow),1]
  subset(Combined_Season_Stats_Freethrow, FTr == max (Combined_Season_Stats_Freethrow, na.rm == TRUE))
                            
                            
#c. What year/season does Lebron James scored the highest?
                            
    Lebron_James <- subset(SEASON_STAT_DATA, Player == 'LeBron James')
    Lebron_James_MaxPts <- max(Lebron_James$PTS, na.rm = TRUE)
    subset(Lebron_James, PTS == Lebron_James_MaxPts)$Year
                            
                            
#d. What year/season does Michael Jordan scored the highest?
                            
     Michael_Jordan <- subset(SEASON_STAT_DATA, Player == 'Michael Jordan*')
    subset(Michael_Jordan, PTS == max(Michael_Jordan$PTS)$PER)
                            
#e. Player efficiency rating of Kobe Bryant in the year where his MP is the lowest?
                            
    Kobe_Bryant <- subset(SEASON_STAT_DATA, Player == ' Lebron James')
    subset(Kobe_Bryant, MP == min(Kobe_Bryant$MP))$PER
                            
##*******************************************************************************************************************#
                            
#4. National Universities Rankings
                            
UNIVERSITIES_RANKING <- read.csv('National Universities Rankings.csv')

#a. University with the most number of undergrads

print(UNIVERSITIES_RANKING[which.max(UNIVERSITIES_RANKING$Undergrad.Enrollment), ]$Name)


#b. Average Tuition in the Top 10 University

TOP_10_UNIVERSITIES <- UNIVERSITIES_RANKING[order(UNIVERSITIES_RANKING$Rank),][1:10,]
TOP_10_UNIVERSITIES$tuition_no_dollar <- gsub(pattern = "\\$|\\,", replacement = "", TOP_10_UNIVERSITIES$Tuition.and.fees)
mean(as.numeric(TOP_10_UNIVERSITIES$tuition_no_dollar))

