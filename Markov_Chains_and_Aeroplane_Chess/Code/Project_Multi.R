#Aeroplane Chess Multiplayer
#Would be much easier by directly simulation than using Markov chain

N = 57 #total steps - Sensitivity Analysis

num = 4 #number of players

shortcut = F #if additional rule is applied - Sensitivity Analysis

#----------Set up P matrix----------#
P=matrix(0, nrow=N, ncol=N, byrow=T)

for (i in 1 : (N-6))
{
  for (j in 1 : 6)
  {
    P[i, i+j] = 1/6
  }
}

for (i in c(N-5, N-2))
{
  P[i, N-4] = 1/6
  P[i, N-3] = 1/6
  P[i, N-2] = 1/6
  P[i, N-1] = 1/3
  P[i, N] = 1/6
}

for (i in c(N-4, N-3))
{
  P[i, N-3] = 1/6
  P[i, N-2] = 1/3
  P[i, N-1] = 1/3
  P[i, N] = 1/6
}

for (j in (N-5):N)
{
  P[N-1, j] = 1/6
}
P[N, N] = 1

if (shortcut == T)
{
  P[13:18, 19] = 0/6
  P[13:18, 31] = 1/6
}
#----------------Done---------------#

s=c(1:N) #possible states
gamelength=matrix(0, nrow = num, ncol = 10000, byrow = T)
takeoff = vector("numeric", 4)
gameon = vector("numeric", 4)
turns = vector("numeric", 4)

for (i in 1:10000)
{
  current = matrix(0, nrow = num, ncol = 100, byrow = T)
  for (no in 1:num)
  {
    takeoff[no] = 0
    gameon[no] = 1
    turns[no] = 0
    
    #if not taken off, do nothing
    while (takeoff[no] == 0)
    {
      turns[no] = turns[no] + 1
      if(sample(1:6, 1) == 6)
      {
        takeoff[no] = 1
        current[no, (turns[no])] = 1
      }
    }
    
    #start moving
    while (gameon[no] == 1 && takeoff[no] == 1)
    {
      current[no, turns[no]+1]=sample(s,1,replace=TRUE,prob=P[current[no, turns[no]],])
      if (current[no, turns[no]+1] == N)
      {
        gameon[no] = 0
      }
      turns[no] = turns[no] + 1
    }
    gamelength[no, i] = turns[no] - 1
  }
}

#Find the minimum step among players
Gamelength = apply(gamelength, 2, min)

winner = vector()
for (i in 1 : 10000)
{
  winner[i] = match(Gamelength[i], gamelength[, i])
}

#histogram of length
summary(Gamelength)
hist(Gamelength, xlab = "Game Length", main = "")

#histogram of winners
h <- hist(winner, breaks = 0:num, xlab = "Winner", main = "", ylim = c(0, 12000/num))
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))








