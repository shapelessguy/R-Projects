library('ggplot2')
library('ggpubr')
library('dplyr')
library('stringr')
library('corrplot')
library('Hmisc')

rbind_rating_difference <- function(nominal_dataset, ch){
  # Return the given dataset with an extra column of 'rating difference' 
  # after printing the boxplot with the character ch
  nominal_dataset['rating_difference'] <- 
    nominal_dataset[['white_rating']] - nominal_dataset[['black_rating']]
  #print(ggboxplot(nominal_dataset, x = ch, y = "rating_difference", color = ch, 
  #                palette = c("blue", "red", "green")))
  return (nominal_dataset)
}

preparation <- function(dataset){
  new_dataset <- dataset
  levels(new_dataset$rated) <- c('FALSE', 'FALSE', 'TRUE','TRUE')  #Reassign levels by semantic
  new_dataset['game_time'] <- new_dataset[['turns']]
  new_dataset['increment_time'] <- new_dataset[['turns']]
  i=0
  for(ch in new_dataset$increment_code){
    i = i+1
    ch1 <- str_replace_all(ch, "[^[:alnum:]]", " ")
    ch2 <- strsplit(ch1, " ")
    ch3 <- unlist(ch2)
    new_dataset[i, 'game_time'] <- as.integer(ch3[[1]])
    new_dataset[i, 'increment_time'] <- as.integer(ch3[[2]])
  }
  
  
  new_dataset <- rbind_rating_difference(new_dataset, 'winner')
  new_dataset['winner_int'] <- new_dataset[['turns']]
  new_dataset$winner_int[new_dataset$winner == "white"] <- 1
  new_dataset$winner_int[new_dataset$winner == "draw"] <- 0
  new_dataset$winner_int[new_dataset$winner == "black"] <- -1
  #levels(new_dataset$winner_int) <- c(-1, 0, 1)
  
  new_dataset['group'] <- 'not expected'
  
  for(i in 1:length(new_dataset[[1]])){
    if(new_dataset[i, 'winner_int'] == 1 & new_dataset[i, 'rating_difference'] < 0) 
    {dataset[i, 'group'] <- 'not expected'}
    else if(new_dataset[i, 'winner_int'] == -1 & new_dataset[i, 'rating_difference'] < 0) 
    {new_dataset[i, 'group'] <- 'expected'}
    else if(new_dataset[i, 'winner_int'] == 1 & new_dataset[i, 'rating_difference'] > 0) 
    {new_dataset[i, 'group'] <- 'expected'}
    else if(new_dataset[i, 'winner_int'] == -1 & new_dataset[i, 'rating_difference'] > 0) 
    {new_dataset[i, 'group'] <- 'not expected'}
    else if(new_dataset[i, 'rating_difference'] == 0) {new_dataset[i, 'group'] <- 'uncertain'}
    
    if(new_dataset[i, 'rating_difference'] > -100 & new_dataset[i, 'rating_difference'] < 100) 
    {new_dataset[i, 'group'] <- 'uncertain'}
  }
  
  new_dataset['rating_difference_div100'] <- as.integer(new_dataset[['rating_difference']] / 100)
  new_dataset['game_result'] <- 
    strtoi(new_dataset[['winner_int']], base = 16L) / new_dataset[['turns']]
  
  return(new_dataset)
}

filter_newUsers <- function(dataset, max_difference, excluded_values){
  
  new_dataset <- dataset
  users <- c()
  for (name in dataset$white_id){
    a <- c()
    for (rating in dataset$white_rating[dataset$white_id == name]){
      a <- c(a, rating)
    }
    for (rating in dataset$black_rating[dataset$black_id == name]){
      a <- c(a, rating)
    }
    a <- sort(a)
    if(length(a) > 1){
      elem <- a[1]
      for (rat in a){
        if(rat - elem > max_difference) {users <- c(users, name); break; }
        excluded = FALSE
        for (value in excluded_values){
          if(elem == value) {users <- c(users, name); excluded = TRUE; }
          if(excluded) break;
        }
        elem <- rat
      }
    }
    else if (a[1] == 1500) {users <- c(users, name);}
  }
  
  dataset['new_user'] = 'Remaining distribution'
  for (name in users){
    dataset$new_user[dataset$white_id == name | dataset$black_id == name] <- 'Cut - removed users'
  }
  new_dataset <- dataset %>% filter (new_user == 'Remaining distribution')
  
  plot <- ggplot(data = new_dataset, mapping = aes(x=new_dataset[['white_rating']])) + 
    geom_histogram(color = "black", fill = "blue") + 
    xlab('white rating (new users removal)') + ylab('abs frequency')+
    geom_vline(xintercept = mean(new_dataset[['white_rating']]), linetype = "solid", color = "red", size = 0.3)+
    geom_vline(xintercept = median(new_dataset[['white_rating']]), linetype = "dashed", color = "red", size = 0.3);
  #print(plot)
  plot <- ggplot(data = new_dataset, mapping = aes(x=new_dataset[['black_rating']])) + 
    geom_histogram(color = "black", fill = "blue") + 
    xlab('black rating (new users removal)') + ylab('abs frequency')+
    geom_vline(xintercept = mean(new_dataset[['black_rating']]), linetype = "solid", color = "red", size = 0.3)+
    geom_vline(xintercept = median(new_dataset[['black_rating']]), linetype = "dashed", color = "red", size = 0.3);
  #print(plot)
  print(paste('Dataset with new-users removal length: ', length(new_dataset[[1]])))
  
  
  
  palette <- c("brown2", "chartreuse3")
  plot <- ggplot(dataset, aes(x=white_rating, fill=new_user, color = new_user)) + 
    geom_histogram(alpha=0.9, position = "stack") + xlab('white rating') +
    scale_x_continuous(breaks = seq(-10, 10, by = 1))+scale_fill_manual(values=palette)+
    scale_color_manual(values=palette)
  print(plot)
  plot <- ggplot(dataset, aes(x=black_rating, fill=new_user, color = new_user)) + 
    geom_histogram(alpha=0.9, position = "stack") + xlab('black rating') +
    scale_x_continuous(breaks = seq(-10, 10, by = 1))+scale_fill_manual(values=palette)+
    scale_color_manual(values=palette)
  print(plot)
  
  return(new_dataset)
}
filter_shortgames <- function(act_dataset, min_turns){
  new_dataset <- act_dataset
  new_dataset['not_short'] = '-Too short'
  new_dataset$not_short[new_dataset$turns > min_turns] <- 'Acceptable'
  palette = c('black', 'brown')
  print_coloredHistogram(new_dataset, 'not_short', 'white_rating', 'White rating', palette)
  print_coloredHistogram(new_dataset, 'not_short', 'black_rating', 'Black rating', palette)
  
  sub_set <- new_dataset %>% filter (not_short == '-Too short')
  sub_set_vec <- sub_set[['victory_status']];
  print(barplot(table(sub_set_vec), col = 'orange', ylab = 'abs frequency', 
                xlab = paste('Victory status, turns from 0 to ', min_turns), horiz = FALSE))
  new_dataset <- new_dataset %>% filter(turns > min_turns)
  print(paste('Dataset with short-games removal length: ', length(new_dataset[[1]])))
  return(new_dataset)
}

print_whitevsblackHistogram <- function(dataset){
  new_dataset <- dataset %>% filter(winner != 'draw')
  levels(new_dataset$winner) <- c('black', 'white', 'white') #remove draw character
  sub_set_vec <- new_dataset[['winner']];
  print(barplot(table(sub_set_vec), col = 'orange', ylab = 'abs frequency', 
                xlab = paste('Winner: White vs Black'), horiz = FALSE))
  print(summary(sub_set_vec))
}

binomialTest <- function(dataset = NULL, null_hyp_prob, alt_hyp_prob = 1, conf_level = 0.95, vect = c()){
  if(is.null(dataset)) {n = vect[1]+vect[2]; success = vect[1]}
  else{
    new_dataset <- dataset %>% filter(dataset$winner != 'draw')
    n = length(new_dataset[[1]])
    m <- new_dataset %>% filter(new_dataset$winner == 'white')
    success = length(m[[1]])
  }
  print(n)
  print(success)
  test <- binom.test(c(success,n-success), p = null_hyp_prob,
                     alternative = c("greater"),
                     conf.level = conf_level)
  print(test)
  
  if(test[3]<1-conf_level){ #when the null hypothesis is refuted
    prev_i <-0
    for(i in as.integer(n/2):success){
      act_i <- pbinom(i, n, null_hyp_prob)
      if(prev_i <= conf_level & act_i >= conf_level) break;
      prev_i <- act_i
    }
    beta <- pbinom(i, n, alt_hyp_prob)
    options(scipen = 0)
    options(digits = 2)
    result = 1-beta
    if(result < 0.0001) result <- formatC(1-beta, format = "e", digits = 3)
    else result <- formatC(1-beta, digits = 3)
    print(paste('Test power -> ', result))
    j <- i
    x <- seq(as.integer(n*null_hyp_prob), as.integer(n*((alt_hyp_prob- null_hyp_prob)*2+null_hyp_prob)), by=1)
    power <- c()
    for( j in x){
      power <- c(power, 1-pbinom(i, n, j/n))
    }
    for(i in 1:length(x)) x[[i]]<- x[[i]]/n
    plot <- ggplot(mapping =  aes(x=x, y = power)) + geom_point(size = 0.4) + 
      xlab('alternative hypothesis probability') + ylab('test power')
    rm(n); rm(success); rm(test)
    return(plot)
  }
}

Numerical_Dataset <- function(mydataset){
  numerical_dataset <- mydataset['turns']
  numerical_dataset <- cbind(numerical_dataset, mydataset['white_rating'])
  numerical_dataset <- cbind(numerical_dataset, mydataset['black_rating'])
  numerical_dataset <- cbind(numerical_dataset, mydataset['opening_ply'])
  numerical_dataset <- cbind(numerical_dataset, mydataset['game_time'])
  numerical_dataset <- cbind(numerical_dataset, mydataset['increment_time'])
  numerical_dataset <- cbind(numerical_dataset, mydataset['rating_difference'])
  numerical_dataset <- cbind(numerical_dataset, mydataset['winner_int'])
  numerical_dataset <- cbind(numerical_dataset, mydataset['rating_difference_div100'])
  numerical_dataset <- cbind(numerical_dataset, mydataset['game_result'])
  return (numerical_dataset)
}

make_classes <- function(mydataset, group1, group2){
  matr <- as.matrix(table(mydataset[[group1]], mydataset[[group2]]))
  inf <- 0; sup <- 0;
  i=0; for (name in colnames(matr)) {i=i+1; if(name=='-1') inf <- i; if(name=='1') sup <- i;}
  for(i in 1:3){matr[i,inf] <- sum(matr[i,1:inf]); matr[i,1:(inf-1)] <- 0 }
  for(i in 1:3){matr[i,sup] <- sum(matr[i,sup:length(matr[i,])]); matr[i,(sup+1):length(matr[i,])] <- 0 }
  matr <- matr[ , inf:sup]
  return(matr)
}

printHisto_byBins <- function(act_dataset, ch, bins){
  # Shows histogram of the character ch with a specified number of bins
  sub_set <- act_dataset[[ch]];
  plot <- ggplot(data = act_dataset[ch], mapping = aes(x=sub_set)) + 
    geom_histogram(bins = bins, color = "black", fill = "blue") + 
    xlab(paste(ch, ' - ',bins, ' bins')) + ylab('abs frequency')+
    geom_vline(xintercept = mean(sub_set), linetype = "solid", color = "red", size = 0.3)+
    geom_vline(xintercept = median(sub_set), linetype = "dashed", color = "red", size = 0.3);
  print(plot)
}
shapiroTest_3groups <- function(dataset){
  n <- length(dataset[[1]])
  groups <- 3
  step <- as.integer(n/groups)
  minus_dataset1 <- dataset[1:step, ]
  minus_dataset2 <- dataset[step+1:step*2, ]
  minus_dataset3 <- dataset[step*2 + 1:step*3, ]
  a <- shapiro.test(minus_dataset1[['white_rating']])
  b <- shapiro.test(minus_dataset1[['black_rating']])
  c <- shapiro.test(minus_dataset2[['white_rating']])
  d <- shapiro.test(minus_dataset2[['black_rating']])
  e <- shapiro.test(minus_dataset3[['white_rating']])
  f <- shapiro.test(minus_dataset3[['black_rating']])
  print(paste('Dataset1 - White',' W-statistic: W =', a[[1]], ' p-value: ', a[2]))
  print(paste('Dataset1 - Black',' W-statistic: W =', b[[1]], ' p-value: ', b[2]))
  print(paste('Dataset2 - White',' W-statistic: W =', c[[1]], ' p-value: ', c[2]))
  print(paste('Dataset2 - Black',' W-statistic: W =', d[[1]], ' p-value: ', d[2]))
  print(paste('Dataset3 - White',' W-statistic: W =', e[[1]], ' p-value: ', e[2]))
  print(paste('Dataset3 - Black',' W-statistic: W =', f[[1]], ' p-value: ', f[2]))
}

print_coloredScatterplot <- function(actual_dataset, color, x, y, xlab, ylab){
  plot <- ggplot(data = actual_dataset, mapping =  aes(x=mydataset[[x]], y = mydataset[[y]],
                                                       color = mydataset[[color]])) + 
    labs(color = "   Result") + geom_point(size = 0.8) + 
    xlab(xlab) + ylab(ylab) + theme(legend.title = element_text(color = "blue", size = 10)) + 
    geom_smooth(aes(color = NULL), method = lm, se = FALSE)
  print(plot)
  regression = lm(actual_dataset[[y]]~actual_dataset[[x]]); 
  print(summary(regression));
  rm(plot); rm(regression);
}

print_coloredHistogram <- function(mydataset, color, x, xlab, palette){
  plot <- ggplot(mydataset, aes(x=mydataset[[x]], fill=mydataset[[color]], color = mydataset[[color]])) + 
    geom_histogram(alpha=0.9, position = "stack") + xlab(xlab)+ theme(legend.title = element_blank()) +
    scale_x_continuous(breaks = seq(-10, 10, by = 1))+scale_fill_manual(values=palette)+
    scale_color_manual(values=palette)
  print(plot)
  rm(plot)
}

