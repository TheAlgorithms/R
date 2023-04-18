# Issue #76
# myAlignment<- msaClustalW("mydata", type = "dna")
# use default substitution matrix
# Error in convertAlnRows(result$msa, type) : There is an invalid aln file!

# I'm facing issue using the above code. I want to perform multiple sequence alignment on csv or fasta file. but getting this error. please help me to solve it.


library(Biostrings)

#Set the file path and name
file_path <- "mydata.fasta"

# Perform the multiple sequence alignment using ClustalW
myAlignment <- msaClustalW(file_path, filetype = "fasta", type = "dna")