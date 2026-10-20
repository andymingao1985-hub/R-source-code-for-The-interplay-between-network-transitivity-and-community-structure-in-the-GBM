# Simulation code based on professor-provided code
# Added calculations for theoretical functions g(lambda, tau) and h(lambda, tau)
# Added loop over tau values for table generation
    n<- 300
		lambda <- 2
		rd<- 0.02
		rd
		rs=lambda*rd
    tau_values <- c(0.1,0.2,0.3,0.4,0.5)
    for (tau in tau_values){
      
      if(lambda<2){
        #function h for lambda<2
        h<- (tau*((3*(tau^2)*(lambda^2))+(4*lambda-(lambda^2))*(1-tau^2)))/(4*(tau*lambda+1-tau)^2)+
          ((1-tau)*(3*((1-tau)^2)*(lambda^2)+(4*lambda-(lambda^2))*tau*(2-tau)))/(4*(((1-tau)*lambda)+tau)^2)
        
        #function g for lambda<2
        g <- (((3-(9*tau*(1-tau)))*(lambda^2)) + (3*tau)*(1-tau)*(4*lambda-(lambda^2)))/
          ((4-12*tau*(1-tau))*(lambda^2) + (8*tau)*(1-tau)*lambda+(4*tau)*(1-tau))
      }else{
        #function h for lambda>=2
        h<- (tau*(3*(tau^2)*(lambda^2)+4*(1-(tau^2))))/(4*(lambda*tau +1-tau)^2)+
          ((1-tau)*(3*((1-tau)^2)*(lambda^2)+(4*tau)*(2-tau)))/(4*(tau+(1-tau)*lambda)^2)
        #function g for lambda>=2
        g<-((3-(9*tau)*(1-tau))*(lambda^2)+(12*tau)*(1-tau))/
          ((4-(12*tau)*(1-tau))*(lambda^2)+(8*tau)*(1-tau)*lambda+(4*tau)*(1-tau))
      }
      REP=100
      
      Global=NULL
      Average=NULL
      for(rep in 1:REP){
        # generate positions
        x <- runif(n, 0, 1)
        
        # generate community labels using tau
        n1 <- tau*n
        n1
        n2 <- (1 - tau)*n
        n2
        z <- c(rep(1, n1), rep(0, n2))
        
        # build adjacency matrix
        A <- matrix(0, n, n)
        for(i in 1:(length(x)-1)){
          for(j in (i+1):length(x)) {
            dist_ij=min(abs(x[i]-x[j]),1-abs(x[i]-x[j]))
            threshold <- ifelse(z[i]==z[j], rs, rd)
            if (dist_ij<= threshold){
              A[i,j]=1
              A[j,i]=1
            }
          }
        }
        isSymmetric(A)
        
        sum_numer=sum(diag(A %*% A %*% A))
        sum_numer
        
        sum_denom=sum(A%*%A)-sum(diag(A%*%A))
        sum_denom
        
        Global_Coefficient = sum_numer/sum_denom
        Global_Coefficient
        
        d=rowSums(A) 
        
        numer_i <- diag(A%*%A%*%A)
        
        denom_i <- d*(d - 1)
        
        cc=ifelse(denom_i==0,0,numer_i/denom_i)
        
        Average_coefficient<-mean(cc)
        Average_coefficient
        
        Global[rep]=Global_Coefficient
        Average[rep]=Average_coefficient
      }
      cat("\n-----------------------\n")
      cat("n =", n, "\n")
      cat("lambda =", lambda, "\n")
      cat("tau =", tau, "\n")
      
      cat("g =", round(g,3), "\n")
      cat("Global mean =", round(mean(Global),3), "\n")
      cat("Global sd =", round(sd(Global),3), "\n")
      
      cat("h =", round(h,3), "\n")
      cat("Average mean =", round(mean(Average),3), "\n")
      cat("Average sd =", round(sd(Average),3), "\n")
      
    }
		
		


 


			