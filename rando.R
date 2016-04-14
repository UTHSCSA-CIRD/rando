### Random mathematical function generator
# We generate random functions that take one or more numeric inputs, and each 
# produces exactly one numeric output. These functions can then be mutated,
# recombined with each other, used to generate really weird distributions and
# simulated data, and probably other uses that will become apparent to us later

## 'New' Math ;-)
# First, though, we need to create some slightly more error-resistant versions
# of the basic math functions.

# A log wrapper that operates on absolute values
logabs <- function(e1,e2) log(abs(e1),abs(e2));

# a division wrapper that resists div by 0
div <- function(e1,e2) e1/max(e2,.Machine$double.eps);

# exponentiation wrapper that resist whatever the hell it is that R tries to do 
# when you attempt to take a fractional power of a negative number!
pow <- function(e1,e2) {
  if(is.na(e1)) return(e1); 
  if(e1<0) e1^round(e2) else e1^e2;
}

## Function to randomly replace the arguments of a call with things from a list
## of atomic functions
growcall <- function(callobj,atoms,probs,incr=32){
  # usage : growcall()
  # Output is an unevaluated call. Note that the call is not quite ready for
  # most uses because all the variables are going to be 'xx'. Suggest using 
  # subcall on this call (see below).
  
  # callobj : An unevaluated call with two arguments optional argument, in fact
  #           most common anticipated use case is to omit it
  # atoms   : List of unevaluated mathematical function calls each taking two 
  #           arguments. Optional, if you omit it, one will be generated for 
  #           you. The first item in this list will always be a `name` object
  #           xx (and `growcall()` will make sure of that for you)
  # probs   : List of probabilities the same length as atoms, Optional and if 
  #           unsure, recommend omitting.
  # incr    : How much to increase the first element of `probs` with each level
  #           of nesting. The purpose of this argument is to control the size 
  #           of the randomly generated calls. The larger this number, the 
  #           smaller the calls.
  
  # Generate default values of any missing optional arguments.
  # Not sure yet, but it may be more realistic (or at least fewer hassles) to 
  # limit `atoms` to just the first 5 values shown here.
  if(missing(atoms)) atoms<-sapply(list(
    'xx','`+`(yy,yy)','`-`(yy,yy)','div(yy,yy)','`*`(yy,yy)','pow(yy,yy)','logabs(yy,yy)'
    ),function(zz) parse(text=zz)[[1]]);
  if(as.character(atoms[[1]])!='xx') atoms <- c(as.name('xx'),atoms);
  if(missing(probs)) probs <- seq(1,1,len=length(atoms));
  if(missing(callobj)) callobj<-sample(atoms[-1],1,prob=probs[-1])[[1]]
  # Increment probabilities such that chance of selecting xx from the atoms is
  # increased when `growcall()` is called recursively
  probs[1]<-incr+probs[1];
  # We use `yy` as an indicator that a particular leaf node is allowed to keep
  # growing. It will sprout branches that will be either `xx` or some call that
  # will itself be permitted to sprout more branches. When all leaf nodes are 
  # `xx`, `growcall()` will exit.
  # You might ask, "what does callobj[[2]] mean?"
  # `callobj` is an R expression, from a preselected set that can work with
  # exactly two parameters. Unevaluated calls behave like lists, and so can be
  # subsetted with the `[[` operator. `callobj[[1]]` is the name of the 
  # function being called while `callobj[[2]]` and `callobj[[3]]` are the two
  # arguments. They both start out as `yy`.
  if((as.character(callobj[[2]])=='yy')[1]) {
    # If `callobj[[2]]` is `yy` by the time it gets to this level of recursion,
    # replace it with a randomly selected item from `atoms`.
    callobj[[2]]<-sample(atoms,1,prob=probs)[[1]];
    # If that item happens to not be `xx`, call `growcall()` recursively again
    if((as.character(callobj[[2]])!='xx')[1]) {
      callobj[[2]]<-growcall(callobj[[2]],atoms,probs);
    }
  }
  # Repeat the above for the other branch.
  if((as.character(callobj[[3]])=='yy')[1]) {
    callobj[[3]]<-sample(atoms,1,prob=probs)[[1]];
    if((as.character(callobj[[3]])!='xx')[1]) callobj[[3]]<-growcall(callobj[[3]],atoms,probs);
  }
  return(callobj);
}

## Function to replace the `xx`s in a call returned by `growcall()` with either 
## valid names of variables from some environment or with randomly generated
## variates.
subcall <- function(callobj,rfuns,vnames=setNames(runif(100,-1,1),letters),vtarget='xx'){
  # usage : subcall(growcall(),vnames=myvars<-setNames(runif(100,-1,1),letters));
  # ...actually subcall(growcall()) will run without error, but the above 
  # invocation also creates the variable `myvars` in the calling scope that
  # you can then use as the evaluation enivronment for the output of subcall()
  # output : unevaluated function call
  
  # callobj : unevaluated call returned by `growcall()`. Required.
  # rfuns   : A list of unevaluated calls that each return a single number
  #           and have hardcoded arguments. Optional.
  # vnames  : A named object, presumably the one that you wish to later use
  #           as an evaluation scope for `callobj`. Optional but recommended
  #           that you pass something in order for `subcall()` to actually be
  #           useful.
  # vtarget : Which placeholder variable to replace with randomly chosen names
  #           or parameter-less function calls... optional, unless you somehow
  #           managed to make something other than `xx` your placeholder 
  #           variable despite our attempts to keep you from doing that.
  
  # Generate default values of any missing optional arguments.
  # By the way, you might be wondering why we include random variable 
  # generators. Turns out, if you only use pre-defined numeric variables
  # (even if some of them get modified by the functions), the variables 
  # that get generated rapidly converge to some static value and stay there
  # forever. Random variables help break the functions out of that trajectory.
  if(missing(rfuns)) rfuns <- sapply(c(
    'rcauchy(1)','rchisq(1,1)','rt(1,1)','rnorm(1)','rlnorm(1)','rexp(1)'
    ),function(kk) parse(text=kk)[[1]]);
  # Replvals is a list of both variable names (of type `name`) and the above
  # unevaluated random generator calls.
  replvals <- c(sapply(names(vnames),as.name),rfuns);
  # Blow away any names that may be sticking to objects in `replvals`. Not yet
  # certain this is actually necessary. Doing it just in case for now.
  names(replvals)<-NULL;
  # If callobj[[2]] is vtarget (xx), replace it with a random number generator
  # or with a variable name.
  if((as.character(callobj[[2]])==vtarget)[1]) callobj[[2]]<-sample(replvals,1)[[1]] else {
    # Otherwise recursively pass it to `subcall()` so that the same thing can
    # be attempted on it's own [[2]] and [[3]].
    callobj[[2]] <- subcall(callobj[[2]],rfuns,vnames,vtarget);
  }
  # Same as above but for the second argument.
  if((as.character(callobj[[3]])==vtarget)[1]) callobj[[3]]<-sample(replvals,1)[[1]] else {
    callobj[[3]] <- subcall(callobj[[3]],rfuns,vnames,vtarget);
  }
  return(callobj);
}

usedvars<-function(callobj,used=c()){
  # Function that returns a list of unique values referenced by anything
  # inside an unevaluated call (or at any rate, referenced by the first
  # two arguments)
  
  # usage : usedvars(foo);
  # callobj : Unevaluated call, as returned by subcall(growcall())
  # used    : Optional variable automatically populated during recursion
  #           do not set manually
  if(length(callobj)<2) return(unique(used));
  if(typeof(callobj[[2]])=='symbol') used <- c(used,callobj[[2]]) else {
    used <- c(used,usedvars(callobj[[2]],used));
  }
  if(length(callobj)<3) return(unique(used));
  if(typeof(callobj[[3]])=='symbol') used <- c(used,callobj[[3]]) else {
    used <- c(used,usedvars(callobj[[3]],used));
  }
  return(unique(used));
}
# Here is how you can extract all unique variables referenced by a collection 
# of calls named `mycalls`:
myvars<-setNames(runif(100,-1,1),paste0('v',sample(10000:99999,100)))
mycalls <- replicate(30,subcall(growcall(),vnames=myvars));
as.character(unique(do.call(c,sapply(mycalls,usedvars))));

# Here is an end-to-end demo of iteratively creating a data.frame of random
# data using the above.

# Create some random starting variables. Note: seems to work better to use a 
# small range of values. The range of realized values becomes huge fast all on
# its own.
randvars<-setNames(as.list(runif(100,-1,1)),paste0('v',sample(1000:9999,100)));
# Turn it into a data.frame (in this case, 300 rows, 100 columsn)
rdf<-data.frame(randvars); rdf<-rdf[rep(1,300),];
# Select a random 30 variables that will be updated by the random expressions
# that will be created below.
rtargets <- sample(names(rdf),30);
# Now create a panel of 30 random expressions.
randxprs <- sapply(replicate(30,growcall()),subcall,vnames=randvars);
# Generate data and update the data.frame!
for(ii in 1:nrow(rdf)){
  # Carry forward the current row
  rdf[ii+1,]<-rdf[ii,];
  # evaluate each element in randxprs using the current row of rdf as the
  # evaluation scope.
  out<-sapply(randxprs,eval,env=rdf[ii,]);
  # write the non-NA results to the next row (instead of NA results the 
  # current row's values get carried over)
  rdf[ii+1,rtargets[!is.na(out)]]<-out[!is.na(out)];
}
# View the non-constant columns and save that data.frame.
View(dtemp<-rdf[-(1:3),sapply(rdf[-(1:5),],function(jj) length(unique(jj))>2)]);

# Various visualizations of this data frame.
plot(dtemp,pch='.');
heatmap(cor(dtemp),symm=T);
layout(matrix(1:6,nrow=2,byrow = T));sapply(dtemp,plot,type='l');layout(matrix(1));
layout(matrix(1:6,nrow=2,byrow = T));sapply(dtemp,hist);layout(matrix(1));

