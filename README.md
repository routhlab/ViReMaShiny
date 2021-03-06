# ViReMaShiny
 A [Shiny app](https://routhlab.shinyapps.io/ViReMaShiny/) that generates figures for ViReMa analyses
 
> "Recombination is an essential driver of virus evolution and adaption, giving rise to new chimeric viruses, structural variants, sub-genomic RNAs, and Defective-RNAs. Next-Generation Sequencing of virus samples, either from experimental or clinical settings, has revealed a complex distribution of recombination events that contribute to the intrahost diversity within individual hosts. We and others have previously developed alignment tools to discover and map these diverse recombination events in NGS data. However, there is no standard for data visualization to contextualize events of interest and downstream analysis often requires bespoke coding. To address this, we present ViReMaShiny, a web-based application built using the R Shiny framework to allow interactive exploration and point-and-click visualization of viral recombination data provided in BED format generated by computational pipelines such as ViReMa (Viral-Recombination-Mapper)."

Check out the ViReMa papers below for example experiments!

## What is ViReMa?
ViReMa is an algorithm developed by the [Routh](https://www.utmb.edu/routhlab/home) and Johnson Labs providing “a versatile platform for rapid, sensitive and nucleotide-resolution detection of recombination junctions in viral genomes using next-generation sequencing data”. For quick setup, download ViReMa's Dockerfile [here](https://github.com/Routh-Lab/ViReMaDocker). Original code for ViReMa is written in Python and can be downloaded [here](https://sourceforge.net/projects/virema/). 

## Documentation for ViReMaShiny
[Tutorial and Vignettes](https://jayeung12.github.io/)


[Preprint](https://www.biorxiv.org/content/10.1101/2022.04.06.487215v1)

## Relevant Papers
The most recent ViReMa paper:
Sotcheff S, Zhou Y, Sun Y, Johnson JE, Torbett B, Routh AL. ViReMa: A Virus Recombination Mapper of Next-Generation Sequencing data characterizes diverse recombinant viral nucleic acids. bioRxiv. 2022:2022.03.12.484090. doi:10.1101/2022.03.12.484090

The original ViReMa paper:
Andrew Routh, John E. Johnson, Discovery of functional genomic motifs in viruses with ViReMa-a Virus Recombination Mapper-for analysis of next-generation sequencing data, Nucleic Acids Research, Volume 42, Issue 2, 1 January 2014, Page e11, https://doi.org/10.1093/nar/gkt916
