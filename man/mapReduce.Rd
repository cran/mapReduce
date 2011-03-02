\name{mapReduce}
\alias{mapReduce}
\title{mapReduce - mapReduce algorithm for parallel computation }

\description{                       

  mapReduce is an algorithm provides a simple framework for parallel
  computations.  See references for details.  This implementation 
  provides the following features:
    * pure R
    * simple R-style syntax
    * agnostic to parallelization backend
    
  mapReduce is also a convenient replacement for \code{\link{by}} 
  and \code{\link{aggregate}}.  

}

\usage{
  mapReduce( map, ..., data, apply = sapply)
}

\arguments{

  \item{map}{ 
    An expression to be evaluated on data which yielding a vector
    that is subsequently used to split the data into parts that can
    be operated on independently.   
  }

  \item{\dots}{ 
    The reduce step. One or more expressions that are evaluated for each
    of the partitions made 
  }

  \item{data}{ A R data structure such as a matrix, list or data.frame. }

  \item{apply}{ 
    The functions used for parallelization (default: \code{\link{sapply}})
    See Details for how to use another parallelization backend.
  }

}

\details{

  The mapReduce package provides a divide-and-conquer approach to
  parallel computations closely followng the framework and nomenclature
  proposed by Dean and Gemawatt.  The approach is not different 
  from the parallelization approach used internally by R's 
  \code{\link{apply}} function.  In fact, mapReduce is nothing more 
  than:

    apply( map(data), reduce )

  The novelty of both this package and the Dean and Gemawatt paper is
  the extension beyond a single-process to modern architectures: 
  multiple cores, processes, machines, clusters, data centers, or 
  clouds.  

  Because there is no standard "out-of-process" parallezation function
  in R, by default, mapReduce runs "in-process" using 
  \code{\link{sapply}}. Here, mapReduce can be though of as a 
  replacement of apply type function such as \code{\link{by}} and 
  \code{\link{aggregate}}.  
  
  This package was designed to make 
  "out-of-process" parallelization easy and seamless across all 
  parallelization infrastructure methods and technques. The user 
  need only supply his own parallelization function to the apply
  argument.
  
}

\value{
  The value returned depends on the reduce step.  Commonly, this is a 
  simple R data structure such as a data.frame.
}

\references{ 
Dean and Gemawatt, (2004) MapReduce: Simplified Data Processing on Large Clusters. OSDI'04: Sixth Symposium on Operating System Design and Implementation.  

also: http://labs.google.com/papers/mapreduce.html

The Open Data Group at http://www.opendatagroup.com

}

\author{ Christopher Brown <cbrown -at- opendatagroup.com> }

\note{ 
  Special Thanks to Collin Bennett and Robert Grossman of Open Data
  group for advice and feedback.
}


\seealso{ 
  \code{\link{apply}}, \code{\link{sapply}} - 
  \code{\link{by}}, \code{\link{aggregate}} ,

  Parallelization Backends: papply, multicore, snow.
}


\examples{  


mapReduce( 
  map=Species, 
  mean.sepal.length=mean(Sepal.Length),
  max.sepal.length=max(Sepal.Length) ,
  data = iris
) 

mapReduce( 
  substr(Species,1,3),  
  mean.sepal.length=mean(Sepal.Length),
  max.sepal.length=max(Sepal.Length),  
  data=iris
) 

mapReduce( cyl, mean(mpg), avg.hp=mean(hp), data=mtcars )   

}

\keyword{ iteration }

