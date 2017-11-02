switch_R<-function(condiciones=TRUE,consecuencias="Ingrese Consecuencia")
{
  
  condiciones=as.vector(condiciones)
  consecuencias=as.vector(consecuencias)
  if(length(condiciones)+1==length(consecuencias))
  {
    for (i in 1:length(condiciones))
    {
      if(condiciones[i]==TRUE)
      {
        return(consecuencias[i])
      }
    }    
  }
  else
  {return(FALSE)
  }
}


switch_R(condiciones=c(FALSE,FALSE,TRUE),consecuencias = c(1,2,3,4))
