library(RMySQL)
library(ggplot2)

username <- readline(prompt= "Please enter your user name: ")
pass <- readline(prompt= "Please enter your password: ")
db <- readline(prompt= "Please enter your database name: ")
gnome = dbConnect(MySQL(), user=username, password=pass, dbname=db, host='localhost')


repeat{
choice <- readline(prompt="Please select from following options: 
         \n0: Generate csv file
         \n1: Number of Commits 
         \n2: Number of Committers
         \n3: Number of Authors
         \n4: Number of Files
         \n5: Exit program")


print(choice)

## edited 1,2,3 ####
if(choice==0){
  overview = dbGetQuery(gnome,"select r.id as 'repository', count(distinct s.date) as 'commits', 
  					count(distinct s.committer_id) as 'committers', 
                        count(distinct s.author_id) as 'authors'      
                  from repositories r
                  left outer join scmlog s
                  on r.id = s.repository_id
                  group by r.id;")
  overview2 = dbGetQuery(gnome, "select r.id as 'repository', count(distinct f.file_name) as 'files'
                                from repositories r 
                                left outer join files f
                                on r.id = f.repository_id
                                group by r.id;")
  repo <- merge(x=overview, y=overview2)
  write.csv(file="repo.csv", x=repo)
  

  
}

if(choice==1){
  # fetch number of commits for each repository
  noOfCommits = dbGetQuery(gnome, "select r.id as 'repository', count(distinct s.date) as 'commits'
                                    from repositories r
                                    left outer join scmlog s
                                    on r.id = s.repository_id
                                    group by r.id;")
 
  #plot histogram for number of commits per repo
  commitsHist <- ggplot(noOfCommits, aes(commits)) + geom_histogram() + ylab("Frequency") +ggtitle("No. Of Commits per Repo")
  ggsave("commitHistogram.png")
  
  #plot boxplot for number of commits per repo
  commitsBox <- ggplot(noOfCommits, aes(repository, commits)) + geom_boxplot() + ylab("Number of Commits")+ ggtitle("No. Of Commits per Repo")
  ggsave("commitBoxPlot.png")
}
  

if(choice==2){
  #fetch number of committers for each repository
  noOfCommitters =dbGetQuery(gnome, "select r.id as 'repository', count(distinct s.committer_id) as 'committers'
from repositories r
left outer join scmlog s
on r.id = s.repository_id
group by r.id; ")
  
  #plot histogram for number of committers per repo
  committersHist <- ggplot(noOfCommitters, aes(committers)) + geom_histogram() + ylab("Frequency") +ggtitle("No. Of Committers per Repo")
  ggsave("committersHistogram.png")
  
  #plot box plot for number of committers per repo
  committersBox <- ggplot(noOfCommitters, aes(repository,committers)) + geom_boxplot()+ ylab("Number of Committers") + ggtitle("No. Of Committers per Repo")
  ggsave("committersBoxPlot.png")
}


if(choice==3){
  #fetch the number of authors for each repository
  noOfAuthors = dbGetQuery(gnome, "select r.id as 'repository', count(distinct s.author_id) as 'authors'
from repositories r
left outer join scmlog s
on r.id = s.repository_id
group by r.id;")
  
  #plot histogram for number of authors per repo
  authorsHist <- ggplot(noOfAuthors, aes(authors)) + geom_histogram() + ylab("Frequency") +ggtitle("No. Of Authors per Repo")
  ggsave("authorsHistogram.png")
  
  #plot boxplot for number of authors per repo
  authorsBox <- ggplot(noOfAuthors, aes(repository, authors)) + geom_boxplot() + ylab("Number of Authors") +ggtitle("No. Of Authors per Repo")
  ggsave("authorsBoxPlot.png")
}


if(choice==4){
  #fetch the number of files for each repository
  noOfFiles = dbGetQuery(gnome, "select r.id as 'repository', count(distinct f.file_name) as 'files'
from repositories r 
left outer join files f
on r.id = f.repository_id
group by r.id;")
  
  #plot histogram for number of files per repo
  filesHist <- ggplot(noOfFiles, aes(files)) + geom_histogram() + ylab("Frequency") +ggtitle("No. Of Files per Repo")
  ggsave("filesHistogram.png")
  
  #plot boxplot for number of files per repo
  filesBox <- ggplot(noOfFiles, aes(repository, files)) + geom_boxplot() + ylab("Number of Files") +ggtitle("No. Of Files per Repo")
  ggsave("filesBoxPlot.png")
}

if(choice==5){
  
  
  break;
  
}

}



#to view plots: 
#commitsHist : a histogram of number of commits for each repository
#commitsBox : a box plot of number of commits for each repository
#committersHist : a histogram of number of committers for each repository
#committersBox : a box plot of number of committers for each repository
#authorsHist : a histogram of number of authors for each repository
#authorsBox : a box plot of number of authors for each repository
#filesHist : a histogram of number of files for each repository
#filesHist : a box plot of number of files for each repository





