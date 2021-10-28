#Aeroplane Chess Multiplayer
#Transition Matrix when using 4-sided die

#-------How to use------#
#1. Run this whole file to replace P matrix in Project_Multi.R
#2. Run Project_Multi.R from Line 50 (except P matrix part)
#----------END----------#

N = 57 #total steps - Sensitivity Analysis

num = 4 #number of players

shortcut = F #if additional rule is applied - Sensitivity Analysis

#----------Set up P matrix----------#
P=matrix(0, nrow=N, ncol=N, byrow=T)

for (i in 1 : (N-4))
{
  for (j in 1 : 4)
  {
    P[i, i+j] = 1/4
  }
}


for (i in c(N-3, N-2))
{
  P[i, N-2] = 1/4
  P[i, N-1] = 1/2
  P[i, N] = 1/4
}

P[N-1, N-3] = 1/4
P[N-1, N-2] = 1/4
P[N-1, N-1] = 1/4
P[N-1, N] = 1/4

P[N, N] = 1

if (shortcut == T)
{
  P[13:18, 19] = 0/6
  P[13:18, 31] = 1/6
}
#----------------Done---------------#

