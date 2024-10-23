#Standardizing test scores. Can be done for each specific year and gorup then can create a three average if desired from old test score varibales
te <- readRDS("full3.rds")

str(te)

#Can do ex post for specific years and groups perhaphs, but not over numerous years
t_a <- filter(te, Category =="All Students", Year == "2010", Grade == 9)

t_a$math_sco <- standardize(t_a$math_sco)

summary(scale(t_a$math_sco))

hist(scale(t_a$math_sco))
hist(scale(t_a$eng_sco))