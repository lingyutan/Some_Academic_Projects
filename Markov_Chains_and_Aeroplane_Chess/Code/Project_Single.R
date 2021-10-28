#Aeroplane Chess Single Player

N = 57 #total steps

shortcut = F #if additional rule is applied

#Set up P matrix
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

for (j in (52:N))
{
  P[N-1, j] = 1/6
}
P[N, N] = 1

if (shortcut == T)
{
  P[13:18, 19] = 0/6
  P[13:18, 31] = 1/6
}

s=c(1:N) #possible states
gamelength=vector()

for (i in 1:10000)
{
  takeoff = 0
  current = vector() 
  gameon = 1
  turns = 0
  
  #if not taken off, do nothing
  while (takeoff == 0)
  {
    turns = turns + 1
    if(sample(1:6, 1) == 6)
    {
      takeoff = 1
      current[turns] = 1
    }
  }
  
  #start moving
  while (gameon == 1 && takeoff == 1)
  {
    current[turns+1]=sample(s,1,replace=TRUE,prob=P[current[turns],])
    if (current[turns+1] == N)
    {
      gameon = 0
    }
    turns = turns+1
  }
  gamelength[i] = turns - 1
}

summary(gamelength)
hist(gamelength, xlab = "Game Length", main = "")










