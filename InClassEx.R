z <- bank$balance

a <- bank$age

b <- bank$job
b <- as.numeric(b)

c <- bank$marital
c <- as.numeric(c)

d <- bank$education
d <- as.numeric(d)

e <- bank$default
e <- as.numeric(e)

f <- bank$housing
f <- as.numeric(f)

g <- bank$loan
g <- as.numeric(g)

h <- bank$contact
h <- as.numeric(h)

i <- bank$day
i <- as.numeric(i)

j <- bank$month
j <- as.numeric(j)

k <- bank$duration
k <- as.numeric(k)

l <- bank$campaign
l <- as.numeric(l)

m <- bank$previous
m <- as.numeric(m)

n <- bank$y
n <- as.numeric(n)

reg <- lm(z ~ a + b + c + d + e + f + g + h + i + j + k + l + m + n)
coef(reg)



# age is significant
ftest_a <- var.test(a,  z)
# Job is signifiant
ftest_b <- var.test(b, z, )
# marital is signifiant
ftest_c <- var.test(c, z,)
# education is significant
ftest_d <- var.test(d, z, )
# default is significant
ftest_e <- var.test(e, z, )

ftest_f <- var.test(f, z, )

ftest_g <- var.test(g, z, )

ftest_h <- var.test(h, z, )

ftest_i <- var.test(i, z, )

ftest_j <- var.test(j, z, )

ftest_k <- var.test(k, z, )

ftest_l <- var.test(l, z, )

ftest_m <- var.test(m, z, )

ftest_n <- var.test(n, z, )

null_model <- lm(z ~ 1, data=bank)
full_model <- lm(z ~ a + b + c + d + e + f + g + h + i + j + k + l + m + n)
anova(null_model,full_model)


               
               
               
               
               
               
               
               
               
