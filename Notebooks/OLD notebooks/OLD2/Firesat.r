
###### Libraries
install.packages("SPARQL", repos="http://cran.r-project.org")
install.packages("igraph", repos="http://cran.r-project.org")
install.packages("kableExtra", repos="http://cran.r-project.org")
install.packages("stringr", repos="http://cran.r-project.org")
install.packages("dplyr", repos="http://cran.r-project.org")
install.packages("knitr", repos="http://cran.r-project.org")
install.packages("stringdist", repos="http://cran.r-project.org")
install.packages("data.tree", repos="http://cran.r-project.org")
install.packages("treemap", repos="http://cran.r-project.org")
install.packages("networkD3", repos="http://cran.r-project.org")
library(SPARQL)
library(igraph)
library(kableExtra)
library(stringr)
library(dplyr)
library(knitr)
library(stringdist)
library(data.tree)
library(treemap)
library(networkD3)

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
WBendpoint <- "http://localhost:3030/WB/query"
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
                "WorkBench" = c(is_connected(WBg), girth(WBg)$girth == 0, is_directed(WBg), length(which(degree(WBg, mode="out") == 0)) == 1), 
                "MagicDraw" = c(is_connected(MDg), girth(MDg)$girth == 0, is_directed(MDg), length(which(degree(MDg, mode="out") == 0)) == 1))
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

kable(Q, align = c('l', 'c', 'c'), caption = print(sprintf("%d well-formedness checks performed", nrow(Q)))) %>%
  kable_styling("basic") %>%
  add_header_above(c("Are the models well-formed ?" = 3), font_size = 15) %>%
  column_spec(match("MagicDraw", colnames(Q)), background = green[1+MDwf], color = white[1+MDwf]) %>%
  column_spec(match("WorkBench", colnames(Q)), background = green[1+WBwf], color = white[1+WBwf]) %>%
  footnote(general = print(sprintf("The following models are not well-formed: %s %s", if (MDwf == FALSE){"MagicDraw"} else {""}, if (WBwf == FALSE){"WorkBench"} else {""})))

kable(P, align = c('c', 'c')) %>%
  kable_styling("basic") %>%
  add_header_above(c("Are the models isomorphic ?" = 2), font_size = 15) %>%
  row_spec(which(P$WorkBench_MagicDraw == "TRUE"), background = "green", color = "white") %>%
  row_spec(which(P$WorkBench_MagicDraw == "FALSE"), background = "red", color = "white")

kable(M, align = c('l', 'c', 'c'), caption = print(sprintf("%d unique vertices compared", nrow(M)))) %>%
  kable_styling("basic") %>%
  add_header_above(c("Are the vertices the same ?" = 4), font_size = 15) %>%
  column_spec(1+match(authorityPrompt, colnames(M)), background = "green", color = "white")  %>%
  #row_spec(which(N$authorityPrompt == "x" & N$), background = "green", color = "white") %>%
  footnote(general = print(sprintf("You chose to identify %s as the source of authority. \n %d vertices unconsistencies need to be corrected.", authorityPrompt, nrow(N))))

kable(testD, align = c('l', 'l', 'c', 'c'), caption = print(sprintf("%d unique edges compared", nrow(testD)))) %>%
  kable_styling("basic") %>%
  add_header_above(c("Are the edges the same ?" = 5), font_size = 15) %>%
  #column_spec(1+match(toString(slaves), colnames(testD)), background = "red", color = "white") %>%
  column_spec(1+match(authorityPrompt, colnames(testD)), background = "green", color = "white")  %>%
  #row_spec(which(testD[slaves[1]] == testD[authorityPrompt[1]]), background = "green", color = "white") %>%
  footnote(general = print(sprintf("You chose to identify %s as the source of authority. \n %d edges unconsistencies need to be corrected.", authorityPrompt, nrow(modifications))))

kable(N, align = c('l', 'c', 'l', 'l'), caption = print(sprintf("%d vertices unconsistency analyzed", nrow(N)))) %>%
  kable_styling("basic") %>%
  add_header_above(c("Analysis of the vertices" = 5), font_size = 15) %>%
  #column_spec(1+match(authorityPrompt, colnames(M)), background = "green", color = "white")  %>%
  #row_spec(which(N$authorityPrompt == "x" & N$), background = "green", color = "white") %>%
  footnote(general = print(sprintf("You chose to identify %s as the source of authority.", authorityPrompt)))

plot.igraph(testK, edge.curved=.1, layout=layout_nicely)
legend(x=-1,  y=-1.1,  c  ("MagicDraw","WorkBench","WorkBench and MagicDraw"),  pch=21,  col="#777777",  pt.bg=colrs,  pt.cex=0.9,  cex=.4,  bty="n",  ncol=1)


