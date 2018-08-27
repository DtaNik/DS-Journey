
install.packages("data.table")
library(data.table)


# data.table is an R package that provides a high-performance version of base R's data.frame with syntax 
# and feature enhancements for ease of use, convenience and programming speed.


# Create a data.table and call it DT
set.seed(45L) 

DT <- data.table(V1=c(1L,2L), 
                   V2=LETTERS[1:3],
                   V3=round(rnorm(4),4),
                   V4=1:12)

# Subsetting

DT[3:5,]
DT[3:5]
DT[V2=="A"]
DT[V2 %in% c("A", "C")]

DT[,V2]

DT[,.(V2,V3)]
DT[, .(sum(V1), sd(V1))]

DT[, .(Aggregate = sum(V1), SD.V3 = sd(V3))] # The same as the above, with new names

DT[,.(V1,Sd.V3=sd(V3))] 

# Select column V2 and compute std. dev. of V3, which returns a single value and gets recycled , Print column V2 and plot V3

DT[,.(print(V2),
      plot(V3),
      NULL)]

#$$$$$$$$$$$$$$$$$$$$$$$$ DOING j BY Group $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$4

DT[,.(V4.Sum=sum(V4)),by=V1] # Calculate sum of V4 for every group in V1

DT[,.(V4.Sum=sum(V4)), by=.(V1,V2)] # Calculate sum of V4 for every group in V1 and V2

DT[,.(V4.Sum=sum(V4)), # Calculate sum of V4 for every group in sign(V1-1)
   by=sign(V1-1)]

DT[,.(V4.Sum=sum(V4)),                     # The same as the above, with new name
   by=.(V1.01=sign(V1-1))]                # for the variable you're grouping by

DT[1:5,.(V4.Sum=sum(V4)),               # Calculate sum of V4 for every group in V1
     by=V1]                             # after subsetting on the first 5 rows

DT[,.N,by=V1]                         #    Count number of rows for every group in


#$$$$$$$$$$$$$$$ Adding/Updating Columns By Reference in j Using := $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


DT[,V1:=round(exp(V1),2)] # V1 is updated by what is after := DT Return the result by calling DT
DT

DT[,c("V1","V2"):=list(round(exp(V1),2),    # Columns V1 and V2 are updated by
                       LETTERS[4:6])]


DT






