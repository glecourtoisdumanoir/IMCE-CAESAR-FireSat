###### Libraries
#install.packages("SPARQL")
#install.packages("igraph")
#install.packages("kableExtra")
#install.packages("stringr")
#install.packages("dplyr")
#install.packages("knitr")
#install.packages("stringdist")
#install.packages("data.tree")
#install.packages("treemap")
#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("tidyverse")
library(SPARQL)
library(igraph)
library(kableExtra)
library(stringr)
library(dplyr)
library(knitr)
library(stringdist)
library(data.tree)
library(treemap)
library(shiny)
library(shinydashboard)
library(tidyverse)

###### Queries
MDquery <-
'
PREFIX owl:   <http://www.w3.org/2002/07/owl#>
PREFIX rdfs:  <http://www.w3.org/2000/01/rdf-schema#>
PREFIX mission: <http://imce.jpl.nasa.gov/foundation/mission/mission#>
PREFIX base:  <http://imce.jpl.nasa.gov/foundation/base/base#>
PREFIX fse-backbone: <http://imce.jpl.nasa.gov/backbone/imce.jpl.nasa.gov/discipline/fse/fse#>

SELECT DISTINCT ?Child ?Parent
FROM <urn:x-arq:UnionGraph>
WHERE
{
    ?a a owl:Class ;
       rdfs:label ?b ;
       rdfs:subClassOf mission:Component .
    filter not exists {?a rdfs:subClassOf fse-backbone:Entity} .
    filter not exists {?a rdfs:subClassOf <http://imce.jpl.nasa.gov/backbone/firesat.jpl.nasa.gov/user-model/generated/md/nonAuthorities/Requirements_ID__18_5_3_8c20287_1560995931030_118996_18400#Entity>} .
    
    ?c a owl:Class ;
       rdfs:label ?Child ;
       rdfs:subClassOf mission:Component .
    filter not exists {?c rdfs:subClassOf fse-backbone:Entity} .
    filter not exists {?c rdfs:subClassOf <http://imce.jpl.nasa.gov/backbone/firesat.jpl.nasa.gov/user-model/generated/md/nonAuthorities/Requirements_ID__18_5_3_8c20287_1560995931030_118996_18400#Entity>} .
    
    optional {?a rdfs:subClassOf [a owl:Restriction ;
                                    owl:onProperty base:contains ;
                                    owl:someValuesFrom ?c] }.

    optional {?e rdfs:subClassOf [a owl:Restriction ;
                                    owl:onProperty base:contains ;
                                     owl:someValuesFrom ?c ].
    		  ?e rdfs:label ?Parent }.
  }
'
WBquery <-
  '
PREFIX oml: <http://def.seegrid.csiro.au/ontology/om/om-lite#>
PREFIX owl:   <http://www.w3.org/2002/07/owl#>
PREFIX rdfs:  <http://www.w3.org/2000/01/rdf-schema#>
PREFIX fse:   <http://imce.jpl.nasa.gov/discipline/fse/fse#>
PREFIX base:  <http://imce.jpl.nasa.gov/foundation/base/base#>
PREFIX analysis: <http://imce.jpl.nasa.gov/foundation/analysis/analysis#>

SELECT DISTINCT ?AssemblyAcronym ?AssemblyReferenceDesignator ?AlternateName ?CanonicalName ?Identifier ?ParentAlternateName ?ParentSubsystemNumber ?ParentAlternateName ?ParentCanonicalName ?ParentSubsystemAcronym ?ParentSubsystemReferenceDesignator ?AssemblyMass
WHERE
{
  GRAPH ?g {
    ?a a fse:Assembly ;
       fse:hasAssemblyAcronym ?AssemblyAcronym ;
       fse:hasAssemblyReferenceDesignator ?AssemblyReferenceDesignator ;
       analysis:isCharacterizedBy ?CharacterizedBy_Mass ;
       base:hasAlternateName ?AlternateName ;
       base:hasCanonicalName ?CanonicalName ;
       base:hasIdentifier ?Identifier ;
       base:isAggregatedIn ?AggregatedIn_Parent .
    ?AggregatedIn_Parent fse:hasSubsystemNumber ?ParentSubsystemNumber ;
                         base:hasAlternateName ?ParentAlternateName ;
                         base:hasCanonicalName ?ParentCanonicalName ; 
                         fse:hasSubsystemAcronym ?ParentSubsystemAcronym ; 
                         fse:hasSubsystemReferenceDesignator ?ParentSubsystemReferenceDesignator .
    ?CharacterizedBy_Mass fse:hasMassConstraintValue ?AssemblyMass .
    }
}
  '
MDendpoint <- "http://localhost:3030/MD/query"
MDqr <- SPARQL(MDendpoint,MDquery)
MDqr <- MDqr$results
MDqr <- unique(MDqr)
MDqr <- MDqr[order(MDqr$Child, MDqr$Parent),]
MDp <- MDqr
MDqr <- subset(MDqr, MDqr$Parent != "NA")
WBendpoint <- "http://localhost:3030/WBv6/query"
WBqr <- SPARQL(WBendpoint,WBquery)
WBqr <- WBqr$results
WBqr <- WBqr[ ,c('CanonicalName', 'ParentAlternateName')]
WBqr <- unique(WBqr)
colnames(WBqr)[colnames(WBqr)=="CanonicalName"] <- "Child"
colnames(WBqr)[colnames(WBqr)=="ParentAlternateName"] <- "Parent"
WBqr <- WBqr[order(WBqr$Child, WBqr$Parent),]
WBp <- WBqr
WBqr <- subset(WBqr, WBqr$Parent != "NA")


###### Comparing results
### MagicDraw PBS graph format
MDg <- graph_from_data_frame(d=MDqr, directed=TRUE, vertices=NULL)
#plot.igraph(MDg)
#title(main = "MagicDraw PBS", sub = sprintf("Total: %d elements and %d relationships", gorder(MDg), gsize(MDg)))
### Workbench PBS graph format
WBg <- graph_from_data_frame(d=WBqr, directed=TRUE, vertices=NULL)
#plot.igraph(WBg)
#title(main = "Workbench PBS", sub = sprintf("Total: %d elements and %d relationships", gorder(WBg), gsize(WBg)))
### MagicDraw but not in WorkBench graph format
MD_not_WB <- difference(MDg, WBg)
#plot.igraph(MD_not_WB)
#title(main = "MD-WB : Differences from MagicDraw PBS to WorkBench PBS", sub = sprintf("From the %d elements present in MD PBS, here are displayed the %d relationships missing in WB PBS", gorder(MD_not_WB), gsize(MD_not_WB)))
### WorkBench but not in MagicDraw graph format
WB_not_MD <- difference(WBg, MDg)
#plot.igraph(WB_not_MD)
#title(main = "WB-MD : Differences from WorkBench PBS to MagicDraw PBS", sub = sprintf("From the %d elements present in WB PBS, here are displayed the %d relationships missing in MD PBS", gorder(WB_not_MD), gsize(WB_not_MD)))
### WorkBench and in MagicDraw graph format
WB_and_MD <- intersection(WBg, MDg)
#plot.igraph(WB_and_MD)
#title(main = "Intersection of MagicDraw PBS and WorkBench PBS", sub = sprintf("Here are displayed the %d unique elements from the WB and MD PBS, in addition to the %d relationships they have in common", gorder(WB_and_MD), gsize(WB_and_MD)))
testA <- get.data.frame(MD_not_WB, what="edges")
colnames(testA)[colnames(testA)=="from"] <- "Child"
colnames(testA)[colnames(testA)=="to"] <- "Parent"
testA$WorkBench <- ""
testA$MagicDraw <- "x"
#print(testA)
#nrow(testA)
testB <- get.data.frame(WB_not_MD, what="edges")
colnames(testB)[colnames(testB)=="from"] <- "Child"
colnames(testB)[colnames(testB)=="to"] <- "Parent"
testB$WorkBench <- "x"
testB$MagicDraw <- ""
testC <- get.data.frame(WB_and_MD, what="edges")
colnames(testC)[colnames(testC)=="from"] <- "Child"
colnames(testC)[colnames(testC)=="to"] <- "Parent"
testC$WorkBench <- "x"
testC$MagicDraw <- "x"
testD <- rbind(testA, testB, testC)
testD <- testD[order(testD$Child, testD$Parent),]


authoritySources <- c("WorkBench", "MagicDraw")
authorityPrompt <- ""
while (authorityPrompt %in% authoritySources == FALSE)
{
  authorityPrompt <- readline(prompt = sprintf("Enter source of authority for the analysis (%s) : ", toString(authoritySources)))
}

slaves <- setdiff(authoritySources, authorityPrompt)
data.frame(slaves)
modifications <- data.frame(which(testD[slaves[1]] != testD[authorityPrompt[1]]))
#I$test <- stringdist(data.frame(which(testD[slaves[1]] != testD[authorityPrompt[1]])), data.frame(which(testD[slaves[1]] == testD[authorityPrompt[1]])))


testF <- testA[ ,c('Child', 'Parent')]
testF$Origin <- "MagicDraw"
testF$Origin.type <- 1

testG <- testB[ ,c('Child', 'Parent')]
testG$Origin <- "WorkBench"
testG$Origin.type <- 2

testH <- testC[ ,c('Child', 'Parent')]
testH$Origin <- "WorkBench and MagicDraw"
testH$Origin.type <- 3

testI <- union(testF, testG)
testJ <- union(testI, testH)


A <- testJ
B <- testJ
col_order <- c("Parent", "Child", "Origin", "Origin.type")
colnames(B)[colnames(B)=="Parent"] <- "Child2"
colnames(B)[colnames(B)=="Child"] <- "Parent"
colnames(B)[colnames(B)=="Child2"] <- "Child"
D <- union_all(A,B)
colnames(D)[colnames(D)=="Child"] <- "Node"
D <- D[ ,c('Node', 'Origin', 'Origin.type')]
D <- D[order(D$Node),]
D <- unique(D)

D$Duplicate_Node <- duplicated(D$Node)
E <- subset(D, Duplicate_Node == TRUE)
F <- subset(D, Duplicate_Node == FALSE)
E$Origin <- "WorkBench and MagicDraw"
E$Origin.type <- 3
G <- union(E, F)
H <- G[ !( duplicated(G[,1])),]
H <- H[ ,c('Node', 'Origin', 'Origin.type')]
I <- H[ ,c('Node', 'Origin')]
I$WorkBench <- ""
I$MagicDraw <- ""
colnames(H)[colnames(H)=="Node"] <- "Child"
J <- subset(I, I$Origin == "WorkBench")
J$WorkBench <- "x"
K <- subset(I, I$Origin == "MagicDraw")
K$MagicDraw <- "x"
L <- subset(I, I$Origin == "WorkBench and MagicDraw")
L$WorkBench <- "x"
L$MagicDraw <- "x"
M <- rbind(J, K, L)
M <- M[ ,c('Node', 'WorkBench', 'MagicDraw')]
M <- M[order(M$Node),]

if (authorityPrompt == "MagicDraw")
{
  OSA = matrix(ncol = nrow(K), nrow = nrow(J))
  SNX = matrix(ncol = nrow(K), nrow = nrow(J))
  N <- J
  N <- N[ ,c('Node', 'Origin')]
  i <- 1
  while (i <= nrow(J))
  {
    j <- 1
    while (j <= nrow(K))
    {
      OSA[i, j] <- stringdist(J$Node[i],  K$Node[j], method = c("osa"))
      SNX[i, j] <- stringdist(J$Node[i],  K$Node[j], method = c("soundex"))
      j <- j + 1
    }
    if (min(SNX[i,]) == 0)
    {
      N$ClosestStringFromTarget[i] <- K[match (0, SNX[i,]),1]
      N$Comment[i] <- sprintf("Suspected spelling mistake in %s", slaves[1])
    }
    else
    {
      if ((min(SNX[i,]) != 0) & (min(OSA[i,]) < 75/100*nchar(N$Node[i])))
      {
        N$ClosestStringFromTarget[i] <- K[match(min(OSA[i,]), OSA[i,]),1]
        N$Comment[i] <- sprintf("Suspected spelling mistake in %s", slaves[1])
      }
      else
      {
        N$ClosestStringFromTarget[i] <- "-"  
        N$Comment[i] <- sprintf("Suspected extra element in %s", slaves[1])
      }
    }
    i <- i + 1
  }
}

if (authorityPrompt == "WorkBench")
{
  OSA = matrix(ncol = nrow(J), nrow = nrow(K))
  SNX = matrix(ncol = nrow(J), nrow = nrow(K))
  N <- K
  N <- N[ ,c('Node', 'Origin')]
  i <- 1
  while (i <= nrow(K))
  {
    j <- 1
    while (j <= nrow(J))
    {
      OSA[i, j] <- stringdist(K$Node[i],  J$Node[j], method = c("osa"))
      SNX[i, j] <- stringdist(K$Node[i],  J$Node[j], method = c("soundex"))
      j <- j + 1
    }
    if (min(SNX[i,]) == 0)
    {
      N$ClosestStringFromTarget[i] <- J[match (0, SNX[i,]),1]
      N$Comment[i] <- sprintf("Suspected spelling mistake in %s", slaves[1])
    }
    else
    {
      if ((min(SNX[i,]) != 0) & (min(OSA[i,]) < 75/100*nchar(N$Node[i])))
      {
        N$ClosestStringFromTarget[i] <- J[match(min(OSA[i,]), OSA[i,]),1]
        N$Comment[i] <- sprintf("Suspected spelling mistake in %s", slaves[1])
      }
      else
      {
        N$ClosestStringFromTarget[i] <- "-"  
        N$Comment[i] <- sprintf("Suspected extra element in %s", slaves[1])
      }
    }
    i <- i + 1
  }
}

O <- N[0,]
if (authorityPrompt == "WorkBench") 
{
  O <- as.data.frame(setdiff(J$Node, N$ClosestStringFromTarget))
  colnames(O)[colnames(O)=="setdiff(J$Node, N$ClosestStringFromTarget)"] <- "Node"
}
if (authorityPrompt == "MagicDraw") 
{
  O <- as.data.frame(setdiff(K$Node, N$ClosestStringFromTarget))
  colnames(O)[colnames(O)=="setdiff(K$Node, N$ClosestStringFromTarget)"] <- "Node"
}
O$Origin <- authorityPrompt
O$ClosestStringFromTarget <- "-"
O$Comment <- sprintf("Suspected missing element in %s", slaves[1])
N <- rbind(N, O)

H <- H[order(H$Child),]
testJ <- testJ[order(testJ$Child),]
testK <- graph_from_data_frame(d=testJ, directed=TRUE, vertices=H)
colrs <- c("gray50", "tomato", "gold")
E(testK)$color <- colrs[E(testK)$Origin.type]
V(testK)$color <- colrs[V(testK)$Origin.type]
deg <-  degree(testK, mode="all")
V(testK)$size <- deg*1.5
V(testK)$label.cex <- 0.2
V(testK)$size <- 7
E(testK)$arrow.size <- .3

i <- 1
if (length(which(degree(MDg, mode="out") == 0)) == 1)
{
  while (i <= nrow(MDp))
  {
    if (is.na(MDp$Parent[i]))
    {
      MDroot <- (MDp$Child[i])
    }
    i <- i + 1
  }
  MDp <- subset(MDp, MDp$Parent != "NA")
  MDp <- subset(MDp, MDp$Parent != MDroot)
  MDp$pathString <- paste(MDroot, MDp$Parent, MDp$Child, sep = "/")
  MDtree <- as.Node(MDp)
  sink("MDtree.txt")
  print(ToDataFrameTree(MDtree, "level"))  
  sink()
}

i <- 1
if (length(which(degree(WBg, mode="out") == 0)) == 1)
{
  while (i <= nrow(WBp))
  {
    if (is.na(WBp$Parent[i]))
    {
      WBroot <- (WBp$Child[i])
    }
    i <- i + 1
  }
  WBp <- subset(WBp, WBp$Parent != "NA")
  WBp <- subset(WBp, WBp$Parent != WBroot)
  WBp$pathString <- paste(WBroot, wBp$Parent, WBp$Child, sep = "/")
  WBtree <- as.Node(wBp)
  sink("WBtree.txt")
  print(ToDataFrameTree(WBtree, "level"))  
  sink()
}

### Are WorkBench and MagicDraw models well-formed?
Q <- data.frame("Test" = c("Connected", "Acyclic", "Directed", "Rooted"), 
                "WorkBench" = c(is.connected(WBg), girth(WBg)$girth == 0, is_directed(WBg), length(which(degree(WBg, mode="out") == 0)) == 1), 
                "MagicDraw" = c(is.connected(MDg), girth(MDg)$girth == 0, is_directed(MDg), length(which(degree(MDg, mode="out") == 0)) == 1))
WBwf <- FALSE
MDwf <- FALSE
if ((Q$MagicDraw[1] == TRUE) & (Q$MagicDraw[2] == TRUE) & (Q$MagicDraw[3] == TRUE) & (Q$MagicDraw[4] == TRUE))
{
  MDwf <- TRUE
  #column_spec(match("MagicDraw", colnames(Q)), background = "green", color = "white")
}
if ((Q$WorkBench[1] == TRUE) & (Q$WorkBench[2] == TRUE) & (Q$WorkBench[3] == TRUE) & (Q$WorkBench[4] == TRUE))
{
  WBwf <- TRUE
  #column_spec(match("WorkBench", colnames(Q)), background = "green", color = "white")
}

### Are WorkBench and MagicDraw models isomorphic?
P <- data.frame("Test" = c("Isomorphic"), "WorkBench_MagicDraw" = isomorphic(MDg, WBg, method = c("auto")))

###Display tables and plots
green <- c("", "green")
white <- c("black", "white")
dir.create("/firesatReports")

kable(Q, align = c('l', 'c', 'c'), caption = print(sprintf("%d well-formedness checks performed", nrow(Q)))) %>%
  kable_styling("basic") %>%
  add_header_above(c("Are the models well-formed ?" = 3), font_size = 15) %>%
  column_spec(match("MagicDraw", colnames(Q)), background = green[1+MDwf], color = white[1+MDwf]) %>%
  column_spec(match("WorkBench", colnames(Q)), background = green[1+WBwf], color = white[1+WBwf]) %>%
  footnote(general = print(sprintf("The following models are not well-formed: %s %s", if (MDwf == FALSE){"MagicDraw"} else {""}, if (WBwf == FALSE){"WorkBench"} else {""}))) %>%
  save_kable("/firesatReports/wellformed.html")

kable(P, align = c('c', 'c')) %>%
  kable_styling("basic") %>%
  add_header_above(c("Are the models isomorphic ?" = 2), font_size = 15) %>%
  row_spec(which(P$WorkBench_MagicDraw == "TRUE"), background = "green", color = "white") %>%
  row_spec(which(P$WorkBench_MagicDraw == "FALSE"), background = "red", color = "white") %>%
  save_kable("/firesatReports/isomorphic.html")

kable(M, align = c('l', 'c', 'c'), caption = print(sprintf("%d unique vertices compared", nrow(M)))) %>%
  kable_styling("basic") %>%
  add_header_above(c("Are the vertices the same ?" = 4), font_size = 15) %>%
  column_spec(1+match(authorityPrompt, colnames(M)), background = "green", color = "white")  %>%
  #row_spec(which(N$authorityPrompt == "x" & N$), background = "green", color = "white") %>%
  footnote(general = print(sprintf("You chose to identify %s as the source of authority. \n %d vertices unconsistencies need to be corrected.", authorityPrompt, nrow(N)))) %>%
  save_kable("/firesatReports/vertices.html")

kable(testD, align = c('l', 'l', 'c', 'c'), caption = print(sprintf("%d unique edges compared", nrow(testD)))) %>%
  kable_styling("basic") %>%
  add_header_above(c("Are the edges the same ?" = 5), font_size = 15) %>%
  #column_spec(1+match(toString(slaves), colnames(testD)), background = "red", color = "white") %>%
  column_spec(1+match(authorityPrompt, colnames(testD)), background = "green", color = "white")  %>%
  #row_spec(which(testD[slaves[1]] == testD[authorityPrompt[1]]), background = "green", color = "white") %>%
  footnote(general = print(sprintf("You chose to identify %s as the source of authority. \n %d edges unconsistencies need to be corrected.", authorityPrompt, nrow(modifications)))) %>%
  save_kable("/firesatReports/edges.html")

kable(N, align = c('l', 'c', 'l', 'l'), caption = print(sprintf("%d vertices unconsistency analyzed", nrow(N)))) %>%
  kable_styling("basic") %>%
  add_header_above(c("Analysis of the vertices" = 5), font_size = 15) %>%
  #column_spec(1+match(authorityPrompt, colnames(M)), background = "green", color = "white")  %>%
  #row_spec(which(N$authorityPrompt == "x" & N$), background = "green", color = "white") %>%
  footnote(general = print(sprintf("You chose to identify %s as the source of authority.", authorityPrompt))) %>%
  save_kable("/firesatReports/verticesAnalysis.html")

pdf("/firesatReports/graph.pdf")
plot.igraph(testK, edge.curved=.1, layout=layout_nicely)
legend(x=-1,  y=-1.1,  c  ("MagicDraw","WorkBench","WorkBench and MagicDraw"),  pch=21,  col="#777777",  pt.bg=colrs,  pt.cex=0.9,  cex=.4,  bty="n",  ncol=1)
dev.off()

# UI
{
###Initializations
# Connected graph initialization
{
  isolatedNodes <- function(x){
    degree <- degree(x, mode="all")
    df <- data.frame(degree)
    df <- rownames_to_column(df, var = "Node")
    isolatedNodes <- subset(df, degree == 0, select=c(Node)) 
  }
  WBconnectedActions <- ""
  MDconnectedActions <- ""
  WBConnectedG <- WBg
  MDConnectedG <- MDg
  if (is.connected(WBg) == FALSE)
  {
    if (nrow(isolatedNodes(WBg)) != 0) 
    {
      WBconnectedActions <- tags$div(nrow(isolatedNodes(WBg)), "isolated edges found : ", isolatedNodes(WBg), " -> Look for them and connect them.")
      V(WBConnectedG)[which(V(WBConnectedG)$name == toString(isolatedNodes(WBg)$Node))]$color <- "red"
      V(WBConnectedG)[which(V(WBConnectedG)$name != toString(isolatedNodes(WBg)$Node))]$color <- "green"
    }
    if (nrow(isolatedNodes(WBg)) == 0)
    {WBconnectedActions <- tags$div(nrow(isolatedNodes(WBg)), "isolated edge found, however the graph remains not connected -> Look for isolated clusters of edges and connect them.")}
  }
  if (is.connected(WBg) == TRUE) {V(WBConnectedG)$color <- "green"}
  
  if (is.connected(MDg) == FALSE)
  {
    if (nrow(isolatedNodes(MDg)) != 0) 
    {
      MDconnectedActions <- tags$div(nrow(isolatedNodes(MDg)), "isolated edges found : ", isolatedNodes(MDg), " -> Look for them and connect them.")
      V(MDConnectedG)[which(V(MDConnectedG)$name == toString(isolatedNodes(MDg)$Node))]$color <- "red"
      V(MDConnectedG)[which(V(MDConnectedG)$name != toString(isolatedNodes(MDg)$Node))]$color <- "green"
    }
    if (nrow(isolatedNodes(MDg)) == 0)
    {MDconnectedActions <- tags$div(nrow(isolatedNodes(MDg)), "isolated edge found, however the graph remains not connected -> Look for isolated clusters of edges and connect them.")}
  }
  if (is.connected(MDg) == TRUE) {V(MDConnectedG)$color <- "green"}
}

# Directed graph initialization
{
  WBdirectedG <- as.directed(WBg, mode = c("mutual"))
  MDdirectedG <- as.directed(MDg, mode = c("mutual"))
  nbOfUndirectedEdgesInWB <- gsize(WBdirectedG) - gsize(WBg)
  nbOfUndirectedEdgesInMD <- gsize(MDdirectedG) - gsize(MDg)
  WBDirectedG <- WBg
  MDDirectedG <- MDg
  WBdirectedActions <- ""
  MDdirectedActions <- ""
  if (is_directed(WBg) == FALSE)
  {
    WBdirectedActions <- tags$div(nbOfUndirectedEdgesInWB, "undirected edges found -> Look for them and direct them.")
  }
  if (is_directed(WBg) == TRUE) 
  {
    E(WBDirectedG)$color <- "green"
  }
  
  if (is_directed(MDg) == FALSE)
  {
    MDdirectedActions <- tags$div(nbOfUndirectedEdgesInMD, "undirected edges found -> Look for them and direct them.")
  }
  if (is_directed(MDg) == TRUE) 
  {
    E(MDDirectedG)$color <- "green"
  }
}

# Acyclic graph initialization
{
  WBacyclicG <- WBg
  MDacyclicG <- MDg
  WBcircleSizeWB <- girth(MDg)
  MDcircleSizeMD <- girth(MDg)
  WBacyclicActions <- ""
  MDacyclicActions <- ""
  V(WBacyclicG)$color <- "green"
  V(MDacyclicG)$color <- "green"
  V(WBacyclicG)[girth(WBacyclicG)$circle]$color <- "red"
  V(MDacyclicG)[girth(MDacyclicG)$circle]$color <- "red"
  if (girth(WBg)$girth != 0)
  {
    WBacyclicActions <- tags$div("One circle of ", WBcircleSizeWB, "nodes makes the WorkBench graph cyclic.")
  }
  if (girth(MDg)$girth != 0)
  {
    MDacyclicActions <- tags$div("One circle of ", MDcircleSizeMD, "nodes makes the MagicDraw graph cyclic.")
  }
}

# Rooted graph initialization
{
  WBrootedG <- WBg
  MDrootedG <- MDg
  nbOfrootsInWB <- length(which(degree(WBg, mode="out") == 0))
  nbOfrootsInMD <- length(which(degree(MDg, mode="out") == 0))
  WBrootedActions <- ""
  MDrootedActions <- ""
  V(WBrootedG)[which(degree(WBg, mode="out") != 0)]$color <- "yellow"
  V(MDrootedG)[which(degree(MDg, mode="out") != 0)]$color <- "yellow"
  V(WBrootedG)[which(degree(WBg, mode="out") == 0)]$color <- "green"
  V(MDrootedG)[which(degree(MDg, mode="out") == 0)]$color <- "green"
  if (length(which(degree(WBg, mode="out") == 0)) > 1)
  {
    WBrootedActions <- tags$div(nbOfrootsInWB, "roots found instead of 1 -> Correct the extra roots.")
    V(WBrootedG)[which(degree(WBg, mode="out") == 0)]$color <- "red"
    V(WBrootedG)[which(degree(WBg, mode="out") != 0)]$color <- "yellow"
  }
  if (length(which(degree(WBg, mode="out") == 0)) == 0)
  {
    WBrootedActions <- tags$div(nbOfrootsInWB, "root found instead of 1 -> Build one root.")
  }
  if (length(which(degree(MDg, mode="out") == 0)) > 1)
  {
    MDrootedActions <- tags$div(nbOfrootsInMD, "roots found instead of 1 -> Correct the extra roots.")
    V(MDrootedG)[which(degree(MDg, mode="out") == 0)]$color <- "red"
    V(MDrootedG)[which(degree(MDg, mode="out") != 0)]$color <- "yellow"
  }
  if (length(which(degree(MDg, mode="out") == 0)) == 0)
  {
    MDrootedActions <- tags$div(nbOfrootsInMD, "root found instead of 1 -> Build one root.")
  }
}

### User interface
if (interactive()) 
{
  header <- dashboardHeader(disable = TRUE)  
  sidebar <- dashboardSidebar(disable = TRUE)
  body <- dashboardBody(
    fluidRow(
      tabBox(
        id = "tabset1", height = "auto", width = "auto", title = "Well-formedness analysis of structural/product decomposition",
        tabPanel(icon = if (WBwf == TRUE) {icon("check-circle")} 
                 else {icon("times-circle")}, 
                 title = "WorkBench",
                 
                 # Actions to perform UI                 
                 infoBox(
                   tags$div("Actions to perform: ", WBconnectedActions, WBacyclicActions, WBdirectedActions, WBrootedActions), 
                   icon = icon("list"), 
                   width = "0px"),
                 
                 # Connected UI
                 box(title = "Connected", status =
                       if (is.connected(WBg) == TRUE){"success"}
                     else {"danger"},
                     solidHeader = TRUE, 
                     collapsible = TRUE,  
                     collapsed = TRUE, 
                     width = "0px",
                     "Graph is connected when there is a path from any point to any other point in the graph.",
                     plotOutput("WBconnectedPlot", width = "750px", height = "750px"),
                     if (is.connected(WBg) == TRUE){"No action to perform."}
                     else {WBconnectedActions}),
                 
                 # Acyclic UI
                 box(title = "Acyclic", status =
                       if (girth(WBg)$girth == 0){"success"}
                     else {"danger"},
                     solidHeader = TRUE, 
                     collapsible = TRUE,  
                     collapsed = TRUE, 
                     width = "0px", 
                     "Graph is acyclic when no sequence of edges exists that can be followed to loop back to that starting node.",
                     plotOutput("WBacyclicPlot", width = "750px", height = "750px"),
                     if (girth(WBg)$girth == 0){tags$div("The graph is acyclic.", br(), "No action to perform.")}
                     else {WBacyclicActions}),
                 # Directed UI
                 box(title = "Directed", 
                     status =
                       if (is_directed(WBg) == TRUE){"success"}
                     else {"danger"},
                     solidHeader = TRUE, 
                     collapsible = TRUE,  
                     collapsed = TRUE, 
                     width = "0px", 
                     "Graph is directed when all the edges are directed from one vertex to another.",
                     plotOutput("WBdirectedPlot", width = "750px", height = "750px"),
                     tags$div(nbOfUndirectedEdgesInWB, "undirected edges found."),
                     if (is_directed(WBg) == TRUE){"No action to perform."}
                     else {WBdirectedActions}),
                 # Rooted UI
                 box(title = "Rooted", status =
                       if (length(which(degree(WBg, mode="out") == 0)) == 1){"success"}
                     else {"danger"},
                     solidHeader = TRUE, 
                     collapsible = TRUE, 
                     collapsed = TRUE, 
                     width = "0px", 
                     "Graph is rooted when one and one only vertex, shows no outcoming edge.",
                     plotOutput("WBrootedPlot", width = "750px", height = "750px"),
                     tags$div(nbOfrootsInWB, "root(s) found."),
                     if (length(which(degree(WBg, mode="out") == 0)) == 1){"No action to perform."}
                     else {WBrootedActions})),
        
        tabPanel(icon = if (MDwf == TRUE) {icon("check-circle")} 
                 else {icon("times-circle")}, 
                 title = "MagicDraw",
                 
                 # Actions to perform UI                 
                 infoBox(
                   tags$div("Actions to perform: ", MDconnectedActions, MDacyclicActions, MDdirectedActions, MDrootedActions), 
                   icon = icon("list"), 
                   width = "0px"),
                 
                 # Connected UI
                 box(title = "Connected", status =
                       if (is.connected(MDg) == TRUE){"success"}
                     else {"danger"},
                     solidHeader = TRUE, 
                     collapsible = TRUE,  
                     collapsed = TRUE, 
                     width = "0px",
                     "Graph is connected when there is a path from any point to any other point in the graph.",
                     plotOutput("MDconnectedPlot", width = "750px", height = "750px"),
                     if (is.connected(MDg) == TRUE){"No action to perform."}
                     else {MDconnectedActions}),
                 
                 # Acyclic UI
                 box(title = "Acyclic", status =
                       if (girth(MDg)$girth == 0){"success"}
                     else {"danger"},
                     solidHeader = TRUE, 
                     collapsible = TRUE,  
                     collapsed = TRUE, 
                     width = "0px", 
                     "Graph is acyclic when no sequence of edges exists that can be followed to loop back to that starting node.",
                     plotOutput("MDacyclicPlot", width = "750px", height = "750px"),
                     if (girth(MDg)$girth == 0){tags$div("The graph is acyclic.", br(), "No action to perform.")}
                     else {MDacyclicActions}),
                 # Directed UI
                 box(title = "Directed", 
                     status =
                       if (is_directed(MDg) == TRUE){"success"}
                     else {"danger"},
                     solidHeader = TRUE, 
                     collapsible = TRUE,  
                     collapsed = TRUE, 
                     width = "0px", 
                     "Graph is directed when all the edges are directed from one vertex to another.",
                     plotOutput("MDdirectedPlot", width = "750px", height = "750px"),
                     tags$div(nbOfUndirectedEdgesInMD, "undirected edges found."),
                     if (is_directed(MDg) == TRUE){"No action to perform."}
                     else {MDdirectedActions}),
                 # Rooted UI
                 box(title = "Rooted", status =
                       if (length(which(degree(MDg, mode="out") == 0)) == 1){"success"}
                     else {"danger"},
                     solidHeader = TRUE, 
                     collapsible = TRUE, 
                     collapsed = TRUE, 
                     width = "0px", 
                     "Graph is rooted when one and one only vertex, shows no outcoming edge.",
                     plotOutput("MDrootedPlot", width = "750px", height = "750px"),
                     tags$div(nbOfrootsInMD, "root(s) found."),
                     if (length(which(degree(MDg, mode="out") == 0)) == 1){"No action to perform."}
                     else {MDrootedActions}))
      )))
  server <- function(input, output) 
  {
    output$WBconnectedPlot <- renderPlot({plot.igraph(WBConnectedG, edge.curved=.1, layout=layout_nicely)})
    output$WBdirectedPlot <- renderPlot({plot.igraph(WBDirectedG, edge.curved=.1, layout=layout_nicely)})
    output$WBacyclicPlot <- renderPlot({plot.igraph(WBacyclicG, edge.curved=.1, layout=layout_nicely)})
    output$WBrootedPlot <- renderPlot({plot.igraph(WBrootedG, edge.curved=.1, layout=layout_nicely)})
    output$MDconnectedPlot <- renderPlot({plot.igraph(MDConnectedG, edge.curved=.1, layout=layout_nicely)})
    output$MDdirectedPlot <- renderPlot({plot.igraph(MDDirectedG, edge.curved=.1, layout=layout_nicely)})
    output$MDacyclicPlot <- renderPlot({plot.igraph(MDacyclicG, edge.curved=.1, layout=layout_nicely)})
    output$MDrootedPlot <- renderPlot({plot.igraph(MDrootedG, edge.curved=.1, layout=layout_nicely)})
  }
  shinyApp(
    ui = dashboardPage(
      header,
      sidebar,
      body
    ),
    server = server
  )
}
}