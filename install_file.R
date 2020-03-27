# Initial File

# Here, we install the appropriate packages
install.packages(c("mvtnorm","loo","coda"), repos="https://cloud.r-project.org/",dependencies=TRUE)
options(repos=c(getOption('repos'), rethinking='http://xcelab.net/R'))
install.packages('rethinking',type='source')
