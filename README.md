#Code Sample
A sample of Wolfram Language code from the `master-functions` repo. 
These functions transform tensor integrals into equivalent sums of 
simpler scalar integrals. See `README.pdf` in the `CodeSample` 
directory for more detailed information.

###Optimization Techniques:
- Nearly all of our subroutines harness the parallel computing capabilities 
of the Wolfram Language. Namely, the `ParallelTable` function was used 
generously. 
- We devoted special care to the subroutine 
`ContractionCycles`, the workhorse of the tensor reduction. This subroutine 
handles metric tensor contractions and might be invoked millions of times per 
integral for millions of integrals. We reduced the problem of contracting 
metric tensors to that of finding the number of connected components in an 
undirected graph. By writing an efficient algorithm with good constants and 
using the `Compile` feature to generate compiled `C` code, we increased the
speed of the computation by at least a factor of ten compared to native 
functions in the Wolfram Language. 
- Once a reduction is processed, 
the final and intermediate results are stored for later retrieval.
