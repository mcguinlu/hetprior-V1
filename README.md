# hetprior
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](http://www.repostatus.org/badges/latest/concept.svg)]

* * *

## Installing the `hetprior` R package
First ensure you have the `devtools` package installed:

    install.packages("devtools")

Then to install:

    library(devtools)
    install_github("mcguinlu/hetprior")

To update the package just run the `install_github("mcguinlu/hetprior")` command again.

* * *

## Examples
General Example (with output, without graph)
   
    hetprior("Tau2", "Binary","Log odds ratio","Log normal","General","General","Cancer","<50")
   
   
   
General Example with Graph (graph=TRUE)
    
    hetprior("Tau2", "Binary","Log odds ratio","Log normal","General","General","Cancer",">50",graph=TRUE)
    
    
    
General Example without output (quiet=TRUE)
    
    hetprior("Tau2", "Binary","Log odds ratio","Log normal","General","General","Cancer",">50",quiet=TRUE)
   
* * *   
