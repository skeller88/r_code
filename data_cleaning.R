### aggregate
# create a frequency matrix based on two variables
tapply(a$Freq, list(a$Gender, a$Admit), sum)
# or use the aggregate function on two variables
tbl <- aggregate(a$Freq~a$Gender+a$Admit, data=a, sum)

