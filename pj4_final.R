library(plotrix)



####PROBLEM-1####
#looked at this link for monopoly game simulation.
#https://csgillespie.github.io/efficientR/code-profiling.html

simulate_monopoly=function(n,d){
  #create two dices with d side,roll n times
  dice1 = sample(d,replace = TRUE,n)
  dice2 = sample(d,replace = TRUE,n)
  total<-dice1+dice2
  
  
#check if two roll are the same
  double = dice1 == dice2
  
  #CC card set up,randomly shuffled
  cc_card = sample(c(1:16))
  
  #CH card set up,randomly shuffled
  ch_card = sample(c(1:16))
  
  #discussed with Sangeetha Ramamurthy for how to start the game,clearified few things
  #she told me to use for loop to do 3rd round,and set up first and second round outside for loop 
  #discussed with wei-kuang lin as well, I taught him my approach for this question.
  
  #starting game
  current= numeric(n)
  #first value assigned to 0(Go)
  current[1] = 0
  
  #for loop to run from 2 to n
  for (i in 2:(n+1)){
    #add rolls as we go through the game, current if the square we at for each turn,
    #square can not go above 39
    current[i]=current[i-1]+total[i-1]
    current[i]= current[i] %% 40
    
    #start for 4th turn, we need to check if a player rolls doubles on three consecutive turns
    #professor taught me these line"i > 3 && all(double[(i-3): (i-1)])" to make TRUE/FALSE judgement
    if(i > 3 && all(double[(i-3): (i-1)])){
      current[i] <- 10 #if three doubles, move to jail
    }
  
     # NON - three consecutive double situation, continue
      if(current[i] == 30){ #go to jail
        current[i] <- 10
        
      }else if(current[i] == 2 | current[i] ==17 | current[i] ==33){   
        #CC card
        if(cc_card[1] == 1){
          current[i] <- 0  #go to GO-1
        }else if(cc_card[1] == 2){
          current[i] <- 10 #go to JAIL-10
        }
        cc_card = c(cc_card[-1],cc_card[1])#move first card to last
        
      }if(current[i] == 7 | current[i] ==22 | current[i] ==36){
        #CH card
        if(ch_card[1] == 1){
          current[i] <- 0  #go to GO-1
        }else if(ch_card[1] == 2){
          current[i] <- 10 #go to JAIL-10
        }else if(ch_card[1] == 3){
          current[i] <- 11 #go to C1-11
        }else if(ch_card[1] == 4){
          current[i] <- 24 #go to E3-24
        }else if(ch_card[1] == 5){
          current[i] <- 39 #go to H2-39
        }else if(ch_card[1] == 6){
          current[i] <- 5 #go to C1-11
        }else if(ch_card[1] == 7|ch_card[1]==8){
          if(current[i] == 7){
            current[i] <- 15 #CH1-7 postion goes to RR2-15
          }else if (current[i] == 22){
            current[i] <- 25 #CH2-22 postion goes to RR2-25
          }else if (current[i] == 36){
            current[i] <- 5 #CH3-36 postion goes to RR1-5
          }
        }else if(ch_card[1] == 9){
          if(current[i] == 7|current[i] == 36){
            current[i] <- 12 #CH1-7|CH3-36 postion goes to UT1-12
          }else if(current[i] == 22){
            current[i] <- 28 #CH2-22 postion goes to UT2-28
          }
        }else if(ch_card[1] == 10){
          current[i] <- current[i]-3
        }
        ch_card = c(ch_card[-1],ch_card[1])#move first card to last
      }
      
      #end of IF-ELSE statement
  }
  return(current) #end of for loop
}



####PROBLEM-2####
#function of turn probabily to each square
estimate_monopoly = function(n,d){
  #result of n turn,d side simulation
  result = simulate_monopoly(n,d)
  # probability of each square
  table= table(result)/length(result)
}

#get result of probability of each case
table6=estimate_monopoly(10000,6)
table5=estimate_monopoly(10000,5)
table4=estimate_monopoly(10000,4)
table3=estimate_monopoly(10000,3)

#rank the most likely square
sort(table6)
sort(table4)

#draw graph of probabiliy of each square
plot(table6,type="l",col = "red",lty=1, main = "probabilities of ending a turn on each Monopoly square",
     xlab = "Square",ylab="Probability",ylim = c(0,0.1))
lines(table3,type="l",col="black",lty=2)
lines(table4,type="l",col="blue",lty=3)
lines(table5,type="l",col="green",lty=4)
legend("topright",c("Side6","Side3","Side4","Side5"),
       lty = c(1:4),col = c("red","black","blue","green"))


####PROBLEM-3####
#each trial give probability of each square with 6 side dices for 10000 turns
#repeat each trial 1000times
longterm = replicate(1000, estimate_monopoly(10000, 6))

#probaility of end a turn in jail for 1000 trials
end10=longterm[11,]

#standare error of long-term probability of ending a turn in jail.
stderror10=std.error(end10)




####PROBLEM-4####
#helped by professor for his question
#function give probability of all sample draw from simulation
estimate_monopoly2 = function(x){
  df=sample(x, replace = TRUE)
  table= table(df)/length(df)
  table
}

#one trail with 6 sides, 10000 turns
onetrial=simulate_monopoly(10000,6)

#useless code,leave there for reference
#df=sample(test, replace = TRUE)
#samplevalue=estimate_monopoly2(test)

#draw 10000 samples from the simulation trial, since I use function,the function returns the probability of each square
#repeat for 1000 times
bootstrap=replicate(1000,estimate_monopoly2(test))
##probaility of end a turn in jail for 1000 bootstrap trials
end10_bootstrap=bootstrap[11,]

stderror10_bootstrap=std.error(end10_bootstrap)




####PROBLEM-5####
#for row side 6
#calculate standard error for each square
sd_error_side6=numeric(39)
for(i in 1:39){
  sd_error_side6[i] <- std.error(longterm[i,])
}

#for row side 3
#each trial give probability of each square with 3 side dices for 10000 turns
#repeat each trial 1000 times
longterm3 = replicate(1000, estimate_monopoly(10000, 3))
#calculate standard error for each square
sd_error_side3=numeric(39)
for(i in 1:39){
  sd_error_side3[i] <- std.error(longterm3[i,])
}

#for row side 4
#each trial give probability of each square with 4 side dices for 10000 turns
#repeat each trial 1000 times
longterm4 = replicate(1000, estimate_monopoly(10000, 4))
#calculate standard error for each square
sd_error_side4=numeric(39)
for(i in 1:39){
  sd_error_side4[i] <- std.error(longterm4[i,])
}


#for row side 5
#each trial give probability of each square with 5 side dices for 10000 turns
#repeat each trial 1000 times
longterm5 = replicate(1000, estimate_monopoly(10000, 5))
#calculate standard error for each square
sd_error_side5=numeric(39)
for(i in 1:39){
  sd_error_side5[i] <- std.error(longterm5[i,])
}

#plot the standard error in grpah
plot(sd_error_side6,type = "l",col = "red",lty=1, main = "Standard Error of Probability of each Monopoly Square",
     xlab = "Square",ylab="Standard Error",ylim = c(0.00002,0.0001))
lines(sd_error_side3,type="l",col="black",lty=2)
lines(sd_error_side4,type="l",col="blue",lty=3)
lines(sd_error_side5,type="l",col="green",lty=4)
legend("topright",c("Side6","Side3","Side4","Side5"),
       lty = c(1:4),col = c("red","black","blue","green"))




####PROBLEM-6####
#compare with result of problem 5-side5
#this simulation has 20000 turns in each trial, repeat for 1000 times
longterm5_higher_n = replicate(1000, estimate_monopoly(20000, 5))

#standard error of each square
sd_error_side5_higher_n=numeric(39)
for(i in 1:39){
  sd_error_side5_higher_n[i] <- std.error(longterm5_higher_n[i,])
}

#compare standard error of two side 5 dice game. one has 10000 turns, another has 20000 turns
sd_error_side5_higher_n < sd_error_side5









