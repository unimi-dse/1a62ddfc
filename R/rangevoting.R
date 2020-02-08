#STAR SYSTEM (SCORE THEN AUTOMATIC RUNOFF)


#####################################################

#'Random vote generator
#'
#'Generates a matrix of random votes from a uniform distribution and writes a csv file
#'
#'@param N "Number of voters"
#'@param candidates "Names of candidates"
#'@param parties "Names of parties associated to candidates"
#'
#'@return The data frame containing randomly generated votes
#'
#'@examples
#'N = 100
#'candidates = c('A','B','C')
#'parties = c('right','center','left')
#'randomvote(N,candidates,parties)
#'
#'@export
randomvote = function(N,candidates,parties){
  #INPUTS: number of voters N and arrays of candidates and parties they belong to
  #OUTPUTS: csv file with random votes from a uniform distribution and corresponding data frame

ballots = replicate(N, round(runif(length(candidates),min=0,max=5)))
votes = data.frame(candidates,ballots,row.names = parties)

write.csv2(votes, file="rvotes.csv")
return(votes)
}

#####################################################

#'Majority system calculator
#'
#'Reads a csv file containing votes and computes the winner of that costituency in a first-past-the-post system
#'
#'@param counter "Counter of total parliament seats for each party in previously analyzed costituencies"
#'
#'@return Updated counter of total parliament seats
#'
#'@examples
#'counter = c(1,0,3,2,0,5) #in the used dataset, 6 parties are running in the election
#'counter = majority(counter)
#'
#'@export
majority = function(counter){

  #1. reads a csv file containing votes for each candidate in a costituency
  votes=read.csv2(system.file("extdata/randomcostituency.csv", package = 'rangevoting'),header = TRUE)
  row.names(votes) = votes[,1]
  votes = votes[,-1]
  ballots=as.matrix(votes[,-1])

  names(counter) = row.names(votes)

  #2. computes means for every candidate and takes the candidate(s) with highest mean
  votes$avgs = as.vector(rowMeans(ballots,na.rm=TRUE))

  winner = votes$candidates[votes$avgs>=max(votes$avgs)]

  #3. there might be more than one winner: here the automatic runoff has its role
i=5
L=c()
l=c()
V=matrix()
while(length(winner)>1 & i>0){
  V = ballots[match(as.character(winner), votes$candidates),]
  for(j in 1:length(winner)){
    L = table(V[j,])
    l[j] = L[names(L)==i]
  }
  winner = winner[l>=max(l)]
  i=i-1
}

#4. updates the counter of seats
winpos = match(as.character(winner),votes$candidates)
winner_party = row.names(votes[winpos,])

counter[winner_party]=counter[winner_party] + 1

return(counter)
}

###########################################

#'Proportional system calculator
#'
#'Reads a csv file containing votes for each party in a costituency and assigns the corresponding number of seats
#'
#'@param seats "Number of seats assigned to the analyzed costituency"
#'@param counter "Named vector of total parliament seats won by each party in previously analyzed costituencies"
#'
#'@return Updated counter of parliament seats for each party
#'
#'@examples
#'seats = 10
#'counter = c(1,2,3,0,4,1) #in the used dataset, 6 parties are running in the election
#'counter = proportional(seats,counter)
#'
#'@export
proportional = function(seats,counter){

  #1. reads a csv file containing votes for each party in a costituency
  votes=read.csv2(system.file("extdata/randomcostituency.csv", package = 'rangevoting'),header = TRUE)
  row.names(votes) = votes[,1]
  votes = votes[,-1]
  ballots=as.matrix(votes[,-1])

  names(counter) = row.names(votes)

  #2. normalizes columns (so that any elector's vote has total value = 1)
  cores = detectCores()
  cl = makeCluster(cores-1)
  doParallel::registerDoParallel(cl)
  foreach(j=1:ncol(ballots)) %dopar%
    (ballots[,j] = ballots[,j] / sum(ballots[,j]))

  stopCluster(cl)

  #3. sums all the votes that a party got and normalizes the sum
  votes$sums = rowSums(ballots,na.rm=TRUE)
  votes$sums = votes$sums / sum(votes$sums)

  #4. assigns seats
  ranking = sort(votes$sums, decreasing= FALSE)
  not_first = 0
  for(i in 1:length(ranking)-1){
    winseats = floor(ranking[i]*seats)
    pos = match(ranking[i],votes$sums)
    party = row.names(votes[pos,])
    votes$sums[pos]=0   #necessary to avoid matching twice the same row
    counter[party] = counter[party] + winseats
    not_first = not_first + winseats
  }
  winseats = seats - not_first
  party = row.names(votes[votes$sums == ranking[length(ranking)],])
  counter[party] = counter[party] + winseats

  return(counter)
}

#################################

#'Histogram of votes
#'
#'Reads a csv file containing votes for each party in a costituency and plots the overlaid histogram of votes
#'
#'@return The grouped histogram of scores
#'
#'@usage votehist()
#'
#'@export
votehist = function(){

  #1. reads a csv file containing votes for each party in a costituency
  votes=read.csv2(system.file("extdata/randomcostituency.csv", package = 'rangevoting'),header = TRUE)
  row.names(votes) = votes[,1]
  votes = votes[,-1]
  ballots=as.matrix(votes[,-1])

  #2. plots a grouped histogram of ballots

  N = ncol(ballots)
  ballvec = as.vector(ballots)
  candvec = c()
  for(i in 1:nrow(ballots)){
    candvec = c(candvec,rep(votes$candidates[i],N))
  }
  df = data.frame(candvec,ballvec)

  x = list(
    title = "SCORE",
    showticklabels = TRUE
  )

  his = plot_ly(df, x=~ballvec, color=~candvec) %>%
    add_histogram() %>%
    layout(xaxis = x)

  return(his)
}

##########################
