### Code for all Chapter 1 figures in dissertation
library(JaB)
library(igraph)
library(tidyverse)
library(GGally)

#########################################
### Karate Network Plot ---------------------
#########################################
data("karate") #in JaB and igraphdata packages
pallette <- c( "#22a884",  "#fca636")
set.seed(23)
l <- layout_nicely(karate)
l[2, ] <- c(0.5, -1.5)
l[3, ] <- c(0, -3)
l[4, ] <- c(-0.8, -1.4)
l[5, ] <- c(-2.2, -3)
l[7, ] <- c(-2, -1.8)
l[8, ] <- c(-0.4, -0.75)
l[10, ] <- c(1, -3.5)
l[14, ] <- c(0.5, -2.2)
l[15, ] <- c(3.9, -1.4)
l[16, ] <- c(3.5, -0.6)
l[17, ] <- c(-2.5, -1.5)
l[19, ] <- c(3.8, -2)
l[21, ] <- c(3, -0.3)
l[23, ] <- c(2, -0.4)
l[25, ] <- c(1.8, -3.9)
l[26, ] <- c(2.8, -3.4)
l[28, ] <- c(2.6, -2.9)
l[29, ] <- c(1., -2.75)
l[30, ] <- c(3.7, -2.5)
l[33, ] <- c(3.1, -1.3)
l[34, 1] <- 2.15

# l[, 1] <- l[, 1]* 1.2
n <- c("H", rep(NA, 32), "A")
col <- pallette[V(karate)$color]

plot(karate,
     layout = l,
     vertex.label = n,
     vertex.color = col,
     vertex.size = 40,
     vertex.label.color = "black",
     edge.color = "black",
     rescale = FALSE,
     xlim = c(-3.5, 5),
     ylim = c(-4.5, -0.25))



#########################################
### Game of Thrones and Clash of Kings Network  -----------------
#########################################
### Download original data

# Character lists
got.char <- read.csv("https://raw.githubusercontent.com/harsh2201/GOTNetworkAnalysis/refs/heads/master/character-predictions.csv")
got.key1 <- read.csv("https://raw.githubusercontent.com/harsh2201/GOTNetworkAnalysis/refs/heads/master/book1-nodes.csv")
got.key2 <- read.csv("https://raw.githubusercontent.com/harsh2201/GOTNetworkAnalysis/refs/heads/master/book2-nodes.csv")

# Edges in books 1 and 2
got.edges1 <- read.csv("https://raw.githubusercontent.com/harsh2201/GOTNetworkAnalysis/refs/heads/master/book1-edges.csv")
got.edges2 <- read.csv("https://raw.githubusercontent.com/harsh2201/GOTNetworkAnalysis/refs/heads/master/book2-edges.csv")

# make book number indicator
got.edges1$book <- 1
got.key1$book <- 1
got.edges2$book <- 2
got.key2$book <- 1

# bind all edges together for plotting later
got.edges <- rbind(got.edges1, got.edges2)


### Unify naming conventions between the two books
got.key2$Label[got.key2$Id =="Aegon-I-Targaryen"] <- "Aegon I Targaryen"
got.key2$Label[got.key2$Id =="Aemon-Targaryen-(Maester-Aemon)"] <- "Aemon Targaryen (Maester Aemon)"
got.key2$Label[got.key2$Id =="Aerys-II-Targaryen"] <- "Aerys II Targaryen"
got.key2$Label[got.key2$Id =="Arya-Stark"] <- "Arya Stark"
got.key2$Label[got.key2$Id =="Balon-Swann"] <- "Balon Swann"
got.key2$Label[got.key2$Id =="Bran-Stark"] <- "Bran Stark"
got.key2$Label[got.key2$Id =="Catelyn-Stark"] <- "Catelyn Stark"
got.key2$Label[got.key2$Id =="Cersei-Lannister"] <- "Cersei Lannister"
got.key2$Label[got.key2$Id =="Daenerys-Targaryen"] <- "Daenerys Targaryen"
got.key2$Label[got.key2$Id =="Eddard-Stark"] <- "Eddard Stark"
got.key2$Label[got.key2$Id =="Jaime-Lannister"] <- "Jaime Lannister"
got.key2$Label[got.key2$Id =="Jeor-Mormont"] <- "Jeor Mormont"
got.key2$Label[got.key2$Id =="Joffrey-Baratheon"] <- "Joffrey Baratheon"
got.key2$Label[got.key2$Id =="Jon-Snow"] <- "Jon Snow"
got.key2$Label[got.key2$Id =="Jon-Umber-(Greatjon)"] <- "Jon Umber (Greatjon)"
got.key2$Label[got.key2$Id =="Jorah-Mormont"] <- "Jorah Mormont"
got.key2$Label[got.key2$Id =="Myrcella-Baratheon"] <- "Myrcella Baratheon"
got.key2$Label[got.key2$Id =="Petyr-Baelish"] <- "Petyr Baelish"
got.key2$Label[got.key2$Id =="Renly-Baratheon"] <- "Renly Baratheon"
got.key2$Label[got.key2$Id =="Rhaegar-Targaryen"] <- "Rhaegar Targaryen"
got.key2$Label[got.key2$Id =="Robb-Stark"] <- "Robb Stark"
got.key2$Label[got.key2$Id =="Robert-Baratheon"] <- "Robert Baratheon"
got.key2$Label[got.key2$Id =="Samwell-Tarly"] <- "Samwell Tarly"
got.key2$Label[got.key2$Id =="Sansa-Stark"] <- "Sansa Stark"
got.key2$Label[got.key2$Id =="Stannis-Baratheon"] <- "Stannis Baratheon"
got.key2$Label[got.key2$Id =="Theon-Greyjoy"] <- "Theon Greyjoy"
got.key2$Label[got.key2$Id =="Tommen-Baratheon"] <- "Tommen Baratheon"
got.key2$Label[got.key2$Id =="Tyrion-Lannister"] <- "Tyrion Lannister"
got.key2$Label[got.key2$Id =="Tywin-Lannister"] <- "Tywin Lannister"
got.key2$Label[got.key2$Id =="Viserys-Targaryen"] <- "Viserys Targaryen"


# Make dataset of all characters
got.key <- rbind(got.key1, got.key2) %>% distinct()

# make sure noone is listed twice
got.key %>%
  group_by(Label) %>%
  summarise(count = n()) %>%
  filter(count > 1)

got.key$Label[got.key$Id == "Davos-Seaworth"] <- "Davos Seaworth"



# Only keep characters in 1st and 2nd books
keep.id <- got.key$Id[got.key$Id %in% c(got.edges1$Source, got.edges1$Target, got.edges2$Source, got.edges2$Target)]


## Temporary data set for unifying house allegiance
## "house" is minor house alliance
temp <- got.key %>%
  left_join(got.char, by = join_by(Label == name))%>%
  mutate(name = Label) %>%
  mutate(ID2 = gsub(" ", "-", name))  %>%
  dplyr::select(name, Id, ID2, house, book)

# problems <- sort(keep.id[which(!(keep.id %in% temp$Id))])
#
# got.edges1 %>% filter(Source %in% problems | Target %in% problems) %>% tail()
# got.edges2 %>% filter(Source %in% problems | Target %in% problems)


temp$house[temp$name %in% c("Fogo", "Jommo", "Ogo", "Quaro", "Cohollo", "Qotho", "Haggo" , "Drogo")] <- "dothraki"

temp$house[temp$name %in% c("Beric Dondarrion" , "Brienne" )] <- "house baratheon"

temp$house[temp$name %in% c("Mance Rayder" , "Bael the Bard", "Rattleshirt") ] <- "free folk"

temp$house[temp$name %in% c("Will (prologue)", "Jarman-Buckwell") ] <- "night's watch"

temp$house[temp$Id %in% c("Harren-Hoare", "Jaqen-Hghar", "Lommy-Greenhands",
                          "Captain Myraham", "Myraham's Daughter",
                          "Florian the Fool", "Jonquil", "High Septon (Tyrions)",
                          "Lorent Caswell") ] <- "other"

temp$house[temp$Id %in% c("Aegon-V-Targaryen", "Daeron-II-Targaryen",
                          "Maekar-I-Targaryen", "Aerys-I-Targaryen") ] <- "targaryen"

temp$house[temp$Id %in% c("Tickler") ] <- "house lanister"


temp$house[temp$Id %in% c("Shadd") ] <- "house stark"


temp %>%
  filter(is.na(house)) %>%
  View()



## Data set for plotting
## cleaning up the major houses a bit more
plot.dat <- temp %>%
  mutate(Id = ifelse(is.na(Id), ID2, Id)) %>%
  dplyr::select(name, Id, house, book) %>%
  #filter(in.graph) %>%
  mutate(house = tolower(house)) %>%
  mutate(house = case_when(
    stringr::str_detect(house, "lannister") ~ "house lannister",
    stringr::str_detect(house, "baratheon")| stringr::str_detect(tolower(name), "baratheon") ~ "house baratheon",
    stringr::str_detect(house, "seven") ~ "faith of the seven",
    stringr::str_detect(tolower(name), "targaryen") ~ "house targaryen",
    stringr::str_detect(tolower(name), "stark") ~ "house stark",
    stringr::str_detect(tolower(name), "arryn") ~ "house arryn",
    stringr::str_detect(tolower(name), "frey") ~ "house frey",
    stringr::str_detect(tolower(name), "greyjoy") ~ "house greyjoy",
    stringr::str_detect(house, "crow") ~ "stone crows",
    stringr::str_detect(Id, "Thoros|fat_one|Gunthor") ~ "other",
    stringr::str_detect(Id, "Will-(prologue)") ~ "night's watch",
    stringr::str_detect(Id, "Davos|Shireen") ~ "baratheon",
    stringr::str_detect(Id, "Margaery") ~ "tyrell",
    TRUE ~ house
  )) %>%
  mutate(house = ifelse(is.na(house), "other", house))



## Color the node by the major house alligance
## "ally_house" is major house alligance

plot.dat <- plot.dat %>%
  mutate(main.house = stringr::str_detect(house, "lannister|baratheon|arryn|targaryen|stark|night")) %>%
  mutate(house.label = case_when(
    main.house == TRUE ~ house,
    main.house == FALSE ~ "other"
  )) %>%
  mutate(color = case_when(
    house.label == "house arryn" ~ "darkblue",
    house.label == "house lannister" ~ "#86090A",
    house.label == "house baratheon" ~ "#f6ef4c",
    house.label == "house targaryen" ~ "#6f4451",
    house.label == "house stark" ~ "#c9e9ee",
    house.label == "night's watch" ~ "#3c4f4e",
    TRUE ~ "#000000"
  )) %>%
  mutate(ally_house = case_when(
    house %in% c("house hunter", "black ears", "burned men", "house baelish", "house corbray, house egen", "house redfort", "house waynwood") ~ "house arryn",
    house %in% c("house blackwood", "house bracken", "house darry", "house frey", "house piper", "house vance of wayfarer's rest", "house whent", "house wode") ~ "house tully",
    house %in% c("house blount", "house stokeworth", "house swann", "house thorne", "house trant") ~ "house baratheon",
    house %in% c("house bolton", "house cassel", "house glover", "house hornwood", "house manderly", "house mollen", "house mormont", "house poole", "house reed", "house umber") ~ "house stark",
    house %in% c("house clegane", "house lefford", "house marbrand", "house payne", "house swyft") ~ "house lannister",
    house %in% c("house dayne") ~ "house martell",
    house %in% c("house hightower", "house oakheart", "house oakheart", "house redwyne", "house tarly") ~ "house tyrell",
    TRUE ~ house
  )) %>%
  mutate(main.house.ally = stringr::str_detect(ally_house, "lannister|baratheon|arryn|targaryen|stark|night|tully|tyrell|dothraki"))  %>%
  mutate(main.house.ally = ifelse(main.house.ally, ally_house, "other")) %>%
  mutate(color.ally = case_when(
    main.house.ally == "house arryn" ~ "darkblue",
    main.house.ally == "house lannister" ~ "#86090A",
    main.house.ally == "house baratheon" ~ "#f6ef4c",
    main.house.ally == "house targaryen" ~ "#6f4451",
    main.house.ally == "house stark" ~ "#808080",
    main.house.ally == "night's watch" ~ "#000000",
    main.house.ally == "house tully" ~ "#f77571",
    main.house.ally == "house tyrell" ~ "#9ECEA4",
    main.house.ally == "dothraki" ~ "#84550c",
    TRUE ~ "lightgray"
  ))



plot.dat %>%
  filter(is.na(house))

table(plot.dat$main.house.ally)


## Make igraph object
g.got <- graph_from_edgelist(as.matrix(got.edges[, 1:2]), directed = FALSE)
E(g.got)$weight <- got.edges$weight
E(g.got)$book <- got.edges$book


nrow(plot.dat)
gorder(g.got)

nrow(got.edges)
gsize(g.got)


#assign house allys to the nodes of the network
V(g.got)$house <- NA
for(i in 1:gorder(g.got)){
  n1 <- V(g.got)$name[i]
  id <- plot.dat$Id == n1
  if(sum(id) != 1){cat(n1)}
  V(g.got)$house[i] <- plot.dat$main.house.ally[id]
}

#make sure everyone has a house
plot.dat$Id[plot.dat$Id %in% V(g.got)$name[is.na(V(g.got)$house)]]



## Set up the layout for the plot
## do this with all edges in both books so characters in both books
## are in the same location in both plots
l <- layout_with_kk(g.got) # , weights =  got.edges$weight
V(g.got)$x <- l[,1]
V(g.got)$y <- l[,2]



## Set up each book as its own network

g.got1 <- delete_edges(g.got, E(g.got)[E(g.got)$book == 2])
g.got2 <- delete_edges(g.got, E(g.got)[E(g.got)$book == 1])

gorder(g.got1)
gorder(g.got2)
nrow(plot.dat)
plot.dat <-
  plot.dat %>%
  filter(Id %in% V(g.got)$name)
nrow(plot.dat)


set.seed(342)
layout1 <- igraph::layout_nicely(g.got)
layout1[184, ] <- c(5.1, -6.55)
layout1[123, ] <- c(5.1, -3.6)
layout1[134, ] <- c(-1.2, -6)
layout1[326, ] <- c(-7.1, -7)

layout1[161, ] <- c(-8, 3.87)
layout1[305, ] <- c(1.4, 6.5)
layout1[303, ] <- c(3.7, 6)


net1 <- intergraph::asNetwork(g.got1)
# net1 <- network::delete.vertices(net1, which(!( network::get.vertex.attribute(net1, "vertex.names" )%in% V(g.got1)$name[igraph::degree(g.got1)>0])))
# net1 <- network::get.inducedSubgraph(net.all, v = keep1)
# net1 <- network::delete.edges(net1, which(network::get.edge.attribute(net1, "book") == 2))
net2 <- intergraph::asNetwork(g.got2)
# net2 <- network::get.inducedSubgraph(net.all, v = keep2)
# net1 <- network::delete.edges(net2, which(network::get.edge.attribute(net1, "book") == 1))



### Game of thrones plot

layout_df1 <- as.data.frame(layout1)
colnames(layout_df1) <- c("x", "y")
edge_alpha1 <- scales::rescale(E(g.got1)$weight, to = c(0.5, 1))  # Normalize weights between 0.1 and 1
edge_alpha2 <- scales::rescale(E(g.got2)$weight, to = c(0.5, 1))  # Normalize weights between 0.1 and 1


all.edge.weight <- scales::rescale(E(g.got)$weight, to = c(0.1, 2))
# all.edge.weight[E(g.got)$book == 1]


tab <- got.edges %>%
  select(-weight, -Type) %>%
  pivot_longer(!book, names_to = "type", values_to ="Id") %>%
  select(-type) %>%
  distinct()


char1 <- tab$Id[tab$book == 1]
vnames1 <- network::get.vertex.attribute(net1, "vertex.names")
size1 <- ifelse(vnames1 %in% char1, 1, 0)

ggnet2(net1,
       mode = layout1,  # Use the nicely arranged layout
       label = TRUE,
       node.size = 3,
       size0 = TRUE,
       node.alpha = size1,
       node.label = NA,
       edge.color = "black",
       edge.size = all.edge.weight[E(g.got)$book == 1],
       edge.alpha = edge_alpha1,  # Set edge transparency based on weight
       node.color = plot.dat$color.ally,
       xlim = c(-9, 6),
       ylim = c(-8.3, 7)
)


char2 <- tab$Id[tab$book == 2]
vnames2 <- network::get.vertex.attribute(net2, "vertex.names")
size2 <- ifelse(vnames2 %in% char2, 1, 0)


ggnet2(net2,
       mode = layout1,
       label = TRUE,
       node.size = 3,
       size0 = TRUE,
       node.alpha = size2,
       node.label = NA,
       edge.color = "black",
       edge.size = all.edge.weight[E(g.got)$book == 2],
       edge.alpha = edge_alpha2,
       node.color = plot.dat$color.ally,
       xlim = c(-9, 6),
       ylim = c(-8.3, 7)
)



## Look at characters that are in one but not the other

# yes in book 2, no not in book 1
y2.n1 <- tab %>%
  filter(book == 2) %>%
  filter(! (Id %in% c(got.key1$Id))) %>%
  select(Id) %>%
  distinct() %>% pull()

# yes in book 1, no not in book 2
y1.n2 <- tab %>%
  filter(book == 1) %>%
  filter(! (Id %in% c(got.key2$Id))) %>%
  select(Id) %>%
  distinct() %>% pull()


V(g.got1)$name[igraph::degree(g.got1)==0] %in% y2.n1
V(g.got1)$name[igraph::degree(g.got1)==0]

V(g.got2)$name[igraph::degree(g.got2)==0] %in% y1.n2
V(g.got2)$name[igraph::degree(g.got2)==0]


# how many characters appear in either book
nrow(plot.dat)
# how many characters appear in both books
tab %>%
  group_by(Id) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  filter(count == 2) %>%
  nrow()

# Range of co-mentions in book 1
range(E(g.got1)$weight)

# Most frequent co-mention in Book 1
E(g.got1)[which.max(E(g.got1)$weight)]
E(g.got1)$weight[which.max(E(g.got1)$weight)]

# Average # of non-zero co-mentions in book 1
mean(E(g.got1)$weight)


# Range of co-mentions in book 2
range(E(g.got2)$weight)

# Most frequent co-mention in Book 2
E(g.got2)[which.max(E(g.got2)$weight)]
E(g.got2)$weight[which.max(E(g.got2)$weight)]

# Average # of non-zero co-mentions in book 1
mean(E(g.got2)$weight)



## Code to save game of thrones network as HTML data
## For presentations, but not for dissertation
#
#
# got.el <- as.data.frame(as_edgelist(g.got1))
# nodes <- plot.dat[, c("Id", "name", "main.house.ally")]
# nodes$id <- 0:(nrow(nodes) - 1)
#
#
# edges <- got.el %>%
#   left_join(nodes, by = c("V1" = "Id")) %>%
#   select(-V1) %>%
#   left_join(nodes, by = c("V2" = "Id")) %>%
#   select(-V2) %>%
#   select(id.x, id.y) %>%
#   rename(source = id.x, target = id.y)
#
#
# edges$width <-  E(g.got1)$weight/5
#
# nodes$group <- nodes$main.house.ally
#
#
# # simple with default colours
# # networkD3::forceNetwork(Links = edges, Nodes = nodes,
# #              Source = "source",
# #              Target = "target",
# #              NodeID ="name",
# #              Group = "group",
# #              Value = "width",
# #              opacity = 0.9,
# #              zoom = TRUE)
#
#
# ColourScale <- 'd3.scaleOrdinal()
#             .domain(["house arryn", "house lannister", "house baratheon", "house targaryen", "house stark", "night\'s watch", "house tully", "house tyrell", "dothraki", "other" ])
#            .range(["#00008B", "#86090A", "#f6ef4c", "#6f4451", "#808080", "#000000", "#f77571", "#9ECEA4", "#84550c", "lightgray" ]);'
#
#
#
# networkD3::forceNetwork(Links = edges, Nodes = nodes,
#              Source = "source",
#              Target = "target",
#              NodeID ="name",
#              Group = "group",
#              Value = "width",
#              opacity = 0.9,
#              fontSize = 24,
#              zoom = TRUE,
#              charge= -20,
#              colourScale = htmlwidgets::JS(ColourScale))
#              #linkColour = htmlwidgets::JS("function(d) { return 'rgba(0, 0, 0, ' +  1 + ')'; }"))
#
#
# # library(htmlwidgets)
# # saveWidget(p, file=paste0( getwd(), "/HtmlWidget/networkInteractive2.html"))
#
#


#########################################
### Paul Revere Network ---------------------------
#########################################
library(JaB)
data("paul.revere")

# Membership data
mems <- as.data.frame(vertex.attributes(paul.revere))[,2:8]

set.seed(4353)
# layout_with_kk
l <- layout_with_kk(paul.revere)
l[200, ] <- c(1.5, 0.09)
l[236, ] <- c(-2, 1.8)
l[75, ] <- c(-0.75, -1.75)
l[60, ] <- c(-2.3, -1.7)
l[233, ] <- c(1.75, -1)


# move northen caucus left and down
just.nc <- 1 == (mems$NorthCaucus * (rowSums(mems[, -3]) == 0))
l[just.nc, 2] <- l[just.nc, 2]-2
l[just.nc, 1] <- l[just.nc, 1]-1


# Move center cluster to the left
center.cluster <- 1 == (mems$NorthCaucus * mems$LondonEnemies * mems$TeaParty)
l[center.cluster, 1] <- l[center.cluster, 1] -2


# Moving individual people so there is no overalapping nodes

l[1, ] <- c(-6, 4.3)

l[71, ] <- c(-6, 7.5)
l[84, ]  <- c(-6.8, 7.2)
l[181, ] <- c(-8, 7)
l[136, ] <- c(-7.5, 6.5)
l[251, ] <- c(-7.8, 5.9)


l[51, ]  <- c(-2.8, 4)
l[2, ] <- c(-2.4, 4)

l[96, ] <- c(-1.3, 4.3)
l[69, ] <- c(-2, 5.2)


l[252, ] <- c(-1.4, 2.4)
l[203, ] <- c(-0.5, 1.8)
l[172, ] <- c(-0.2, 2.5)
l[195, ] <- c(1, 2.6)
l[48, ] <- c(0, 3.2)

l[184, ] <- c(-1.2, 8)
l[4, ] <- c(-2.4, 4.5)

l[157, ] <- c(-3.8, 2.4)
l[254, ] <- c(-4, 1.8)
l[11, ] <- c(-3, 1.9)
l[77, ] <- c(-4.8, 1.7)

l[167, ] <- c(-3.5, 7.44)
l[198, ] <- c(-3, 5.5)
l[143, ] <- c(-1.9, 7.1)
l[226, ] <- c(-1, 7.2)
l[107, ] <- c(-2.5, 6.5)

l[102, ] <- c(-4, 4.5)
l[232, ] <- c(-4.1, 6)

l[53, ] <- c(-4.5, 8.5)
l[231, ] <- c(-3.2, 8.5)

l[62, ] <- c(4, 0.5)


l[190, ] <- c(-1.5, 5.8)
l[64, ] <- c(-0.1, 4)
l[20, ] <- c(-0.5, 4.5)
l[97, ] <- c(-0.2, 5)

l[240, ] <- c(-2, 6)

l[93, ] <- c(-5, 0)
l[59,] <- c(-3.6, -0.5)

l[14,] <- c(-3.4, 1)
l[58,] <- c(-2.7, -0.5)
l[47,] <- c(-1, 1)

l[225,] <- c(-5, -1.5)


## Color palette by group membership
pallette <- data.frame(
  group = c("StAndrewsLodge",
            "NorthCaucus",
            "TeaParty",
            "LondonEnemies",
            "LoyalNine",
            "LongRoomClub",
            "BostonCommittee"),
  color = c("#E41A1C",  # Red
            "#377EB8",  # Blue
            "#4DAF4A",  # Green
            "#984EA3",  # Purple
            "#FF7F00",  # Orange
            "#FFFF33",  # Yellow
            "#A65628")  # Brown
)


## Number of organization each person is apart of
num.membersihp <- rowSums(mems)

## make each node a pie chart comprising of equal part of all of their memberships
l.pie <- vector(mode = "list", length = 254)
l.col <- vector(mode = "list", length = 254)
for(i in 1:254){
  l.pie[[i]] <- rep(1, num.membersihp[i])

  which.group <- names(mems)[unlist(mems[i, ])]
  l.col[[i]] <- rep(NA, num.membersihp[i])
  for(j in 1:num.membersihp[i]){
    l.col[[i]][j] <- pallette$color[pallette$group == which.group[j]]
  }

}

lab <- rep(NA, 254)
k <- which(V(paul.revere)$name == "Winthrop.John")
lab[k] <- k

plot(paul.revere,
     layout = l,
     vertex.label = NA,
     #vertex.label = lab, # label nodes when investiaging individal points
     vertex.label = 1:254,
     vertex.shape="pie",
     vertex.size = 4,
     vertex.pie= l.pie,
     vertex.pie.color=l.col,
     vertex.pie.lty = 0,
     vertex.frame.color = "black",
     edge.width= 0.2)



### Plot paul revere latent positions
data("paul.revere")
dist <- ASE(paul.revere, 2)

plot(dist,
     vertex.label = 1:254,
     vertex.shape="pie",
     vertex.size = 4,
     vertex.pie= l.pie,
     vertex.pie.color=l.col,
     vertex.pie.lty = 0)


V(paul.revere)$name[order(igraph::degree(paul.revere))]

data.frame(n = V(paul.revere)$name,
           deg = rowSums(mems)) %>%
  group_by(deg) %>%
  summarise(count = n())


## latent positions
positions <- data.frame(x = dist[,1] +rnorm(254, 0, 0.03), y =dist[,2]+rnorm(254, 0, 0.03))
pie_values <- list()
for(i in 1:nrow(dist)){
  pie_values[[i]] <-  table(factor(l.col[[i]],
                                   levels = c("#377EB8", "#FFFF33", "#A65628", "#984EA3", "#E41A1C", "#FF7F00", "#4DAF4A")))
}

colors <- c("#377EB8", "#FFFF33", "#A65628", "#984EA3", "#E41A1C", "#FF7F00", "#4DAF4A")

# Plot pies
library(plotrix)
plot(1, type = "n", xlim = c(-1.25, 0.15), ylim = c(-1.5,0.75), xlab = "", ylab = "", main = "Pies as Points")

for (i in 1:nrow(positions)) {
  floating.pie(
    xpos = positions$x[i],
    ypos = positions$y[i],
    x = pie_values[[i]],
    col = colors,
    radius = 0.02,
    lty = 0.1
  )
}


# Histograms of distances in latent space
dist.pr <- dist_ASE(as.matrix(as_adjacency_matrix(paul.revere)), 2)
hist(dist.pr)


####################################
### Chile Network plots ---------------
#################################
library(GGally)
library(dplyr)
invisible(lapply(c("ggplot2", "maps", "network", "sna"), base::library, character.only = TRUE))


### Data

Chile_edgelist_orig <- read.csv("~/Dropbox/Olivia/Conflict/school/EL/PlayingWithCode/JaBNetworkTest/ChileanPowerGrid/WithTap_edge.csv")
Chile_attributes <-  read.csv("~/Dropbox/Olivia/Conflict/school/EL/PlayingWithCode/JaBNetworkTest/ChileanPowerGrid/WithTap_node.csv")

unique(Chile_edgelist_orig$Type) # all undirected

#make igraph object
edgelist <- as.matrix(Chile_edgelist_orig[, 1:2])
chile_igraph <- graph_from_edgelist(edgelist, directed = FALSE)



## Type of nodes
nrow(Chile_attributes)
Chile_attributes %>%
  pull(Role) %>%
  table()

## Distances of edges
nrow(Chile_edgelist_orig)
Chile_edgelist_orig %>%
  pull(Distance) %>%
  mean()


#make a network object
chile.net <- network(Chile_edgelist_orig, directed = F)

# add coordinates to network
chile.net %v% "lat" <- Chile_attributes[network.vertex.names(chile.net), "Latitude"]
chile.net %v% "lon" <- Chile_attributes[network.vertex.names(chile.net), "Longitude"]


## Base plot with no zooming ------------------------------------------------------
chile <-
  ggplot(map_data("world", region = "Chile"), aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), color = "grey65",
               fill = "grey", linewidth = 0.2)+
  theme_minimal()
chile

## Add network to plot -------------------------------------------
node.c <- rep("red", 347)
node.c[47] <- "blue"

node.s <- rep(0.25, 347)
node.s[47] <- 4

p_ <- GGally::print_if_interactive
p_ <- GGally::ggnetworkmap(
  chile, chile.net, size = node.s, great.circles = FALSE,
  segment.color = "black", node.color = node.c
)+
  xlim(c(-76, -66)) +
  ylim(c(-45, -17.5))

p_

### full map ---------------------------------------------
pallette <- data.frame(Role = c("Plant","Substation","Switch", "Tap node", "Tap-ed"),
                       #col = c( "#9c179e", "#22a884", "#fca636",  "#e16462" , "#e16462"  ))
                       col = c("#e16462",  "#377EB8",  "#f0f921",  "#4DAF4A", "#f0f921"))


range(Chile_attributes$Latitude)
range(Chile_attributes$Longitude)



lat.min.0 <-  -45
lat.max.0 <- -24 # -16
lon.min.0 <- -75
lon.max.0 <- -68

chile.zoom0.data <- map_data("world", region = "Chile") %>%
  dplyr::filter(lat < lat.max.0 & lat >lat.min.0 ,
                long < lon.max.0 & long > lon.min.0)

chile.zoom1 <-
  ggplot(chile.zoom0.data, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), color = "black",
               fill = "#ecf0e6", linewidth = 0.2)+
  theme_minimal() +
  coord_map(projection = "azequalarea")
#coord_quickmap()
chile.zoom1

## Add network to plot
keep.nodes <- Chile_attributes %>%
  dplyr::filter(Latitude < lat.max.0 & Latitude > lat.min.0,
                Longitude < lon.max.0 & Longitude > lon.min.0)

keep.rows <- Chile_edgelist_orig$Source %in% keep.nodes$Id & Chile_edgelist_orig$Target %in% keep.nodes$Id

keep.edges <- Chile_edgelist_orig[keep.rows, ]

chile.zoom0.net <- network(keep.edges, directed = F)


color.vec0 <- Chile_attributes %>%
  left_join(pallette) %>%
  pull(col)

chile.zoom0.net %v% "lat" <- Chile_attributes[network.vertex.names(chile.zoom0.net), "Latitude"]
chile.zoom0.net %v% "lon" <- Chile_attributes[network.vertex.names(chile.zoom0.net), "Longitude"]



chile.zoom0.net %v% "color" <- color.vec0
chile.zoom0.net %v% "fill" <- color.vec0


table(Chile_attributes$Role)
# junction = switch + tap-ed = yellow
# plant =red
# tap node = t tap = green
# substation = blue

Chile_attributes <- Chile_attributes %>%
  left_join(pallette)


color.vec0 <- Chile_attributes %>%
  left_join(pallette) %>%
  pull(col)


p_0 <- GGally::print_if_interactive
p_0 <- GGally::ggnetworkmap(
  chile.zoom1, chile.zoom0.net, size = 2, great.circles = FALSE,
  segment.color = "#16161f", node.fill = color.vec0, node.color = color.vec0
)

p_0 +
  scale_x_continuous(limits = c(lon.min.0, lon.max.0), expand=c(0,0)) +
  scale_y_continuous(limits = c(lat.min.0, lat.max.0), expand=c(0,0)) +
  #coord_map(projection = "azequalarea") +
  coord_map(projection = "simpleconic", lat0=lat.min.0, lat1 = lat.max.0) +
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))



# Add points of interest
dat.extra <- data.frame(Id = c(105, 108, 29, 178, 130),
                        lat = NA,
                        long = NA)
for(i in 1:5){
  dat.extra$lat[i] <- Chile_attributes$Latitude[Chile_attributes$Id == dat.extra$Id[i]]
  dat.extra$long[i] <- Chile_attributes$Longitude[Chile_attributes$Id == dat.extra$Id[i]]
}

plot

p_0+
  xlim(c(lon.min.0, lon.max.0)) +
  ylim(c(lat.min.0, lat.max.0)) +
  coord_map(projection = "simpleconic", lat0=lat.min.0, lat1 = lat.max.0)+
  geom_point(data = dat.extra, color = c("#c20320"), shape = 15)


# ggsave(filename = "chile-map.tiff", width = 5, height = 10, device='tiff', dpi=2500)

# ### Zoomed in map on Cluster 1 (northern cluster) --------------------------------------------------------------

lat.min.1 <- -35.0
lat.max.1 <- -32.5
lon.min.1 <- -72.25
lon.max.1 <- -70

chile.zoom1.data <- map_data("world", region = "Chile") %>%
  dplyr::filter(lat < lat.max.1 & lat >lat.min.1 ,
                long < lon.max.1 & long > lon.min.1)

chile.zoom1 <-
  ggplot(chile.zoom1.data, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), color = "grey65",
               fill = "#ecf0e6", linewidth = 0.2)+
  coord_map(projection = "simpleconic", lat0=lat.min.1, lat1 = lat.max.1)+
  theme_minimal()
chile.zoom1

# ## Add network to plot
#
Chile_attributes %>% mutate()

keep.nodes <- Chile_attributes %>%
  dplyr::filter(Latitude < lat.max.1 & Latitude > lat.min.1,
                Longitude < lon.max.1 & Longitude > lon.min.1)

keep.rows <- Chile_edgelist_orig$Source %in% keep.nodes$Id & Chile_edgelist_orig$Target %in% keep.nodes$Id

keep.edges <- Chile_edgelist_orig[keep.rows, ]

chile.zoom1.net <- network(keep.edges, directed = F)


chile.zoom1.net %v% "lat" <- Chile_attributes[network.vertex.names(chile.zoom1.net), "Latitude"]
chile.zoom1.net %v% "lon" <- Chile_attributes[network.vertex.names(chile.zoom1.net), "Longitude"]

node.color1 <- Chile_attributes[network.vertex.names(chile.zoom1.net), "col"]


p_1 <- GGally::print_if_interactive
p_1 <- ggnetworkmap(
  chile.zoom1, chile.zoom1.net, size = 3, great.circles = FALSE,
  segment.color = "black", node.color = node.color1
)
p_1

#color data
dat.extra0 <- data.frame(Id = c(105, 130),
                         lat = NA,
                         long = NA)
for(i in 1:2){
  dat.extra0$lat[i] <- Chile_attributes$Latitude[Chile_attributes$Id == dat.extra0$Id[i]]
  dat.extra0$long[i] <- Chile_attributes$Longitude[Chile_attributes$Id == dat.extra0$Id[i]]
}

#plot
p_1+
  xlim(c(lon.min.1, lon.max.1)) +
  ylim(c(lat.min.1, lat.max.1)) +
  geom_point(data = dat.extra, shape = 15, color = "#c20320", size = 6)


### Zoomed in map on Cluster 2 (southern cluster) --------------------------------------------------------------

lat.min.2 <- -38.25
lat.max.2 <- -36.5
lon.min.2 <- -74
lon.max.2 <- -70.5

chile.zoom2.data <- map_data("world", region = "Chile") %>%
  dplyr::filter(lat < lat.max.2 & lat >lat.min.2 ,
                long < lon.max.2 & long > lon.min.2)

chile.zoom2 <-
  ggplot(chile.zoom2.data, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), color = "grey65",
               fill = "#ecf0e6", linewidth = 0.2)+
  theme_minimal()
chile.zoom2

## Add network to plot


keep.nodes <- Chile_attributes %>%
  dplyr::filter(Latitude < lat.max.2 & Latitude > lat.min.2,
                Longitude < lon.max.2 & Longitude > lon.min.2)

keep.rows <- Chile_edgelist_orig$Source %in% keep.nodes$Id & Chile_edgelist_orig$Target %in% keep.nodes$Id

keep.edges <- Chile_edgelist_orig[keep.rows, ]

chile.zoom2.net <- network(keep.edges, directed = F)

chile.zoom2.net %v% "lat" <- Chile_attributes[network.vertex.names(chile.zoom2.net), "Latitude"]
chile.zoom2.net %v% "lon" <- Chile_attributes[network.vertex.names(chile.zoom2.net), "Longitude"]

node.color2 <- Chile_attributes[network.vertex.names(chile.zoom2.net), "col"]


p_2 <- GGally::print_if_interactive
p_2 <- ggnetworkmap(
  chile.zoom2, chile.zoom2.net, size = 3, great.circles = FALSE,
  segment.color = "black",  node.color = node.color2
)
p_2


#plot
p_2+
  xlim(c(lon.min.2, lon.max.2)) +
  ylim(c(lat.min.2, lat.max.2)) +
  geom_point(data = dat.extra, shape = 15, color = "#c20320", size = 6)

# ### Zoomed in map on Cluster 3 (middle cluster) --------------------------------------------------------------

lat.min.3 <- -36.5
lat.max.3 <- -34.6
lon.min.3 <- -73
lon.max.3 <- -70

chile.zoom3.data <- map_data("world", region = "Chile") %>%
  dplyr::filter(lat < lat.max.3 & lat >lat.min.3 ,
                long < lon.max.3 & long > lon.min.3)

chile.zoom3 <-
  ggplot(chile.zoom3.data, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), color = "grey65",
               fill = "#ecf0e6", linewidth = 0.2)+
  theme_minimal()
chile.zoom3

## Add network to plot


keep.nodes <- Chile_attributes %>%
  dplyr::filter(Latitude < lat.max.3 & Latitude > lat.min.3,
                Longitude < lon.max.3 & Longitude > lon.min.3)

keep.rows <- Chile_edgelist_orig$Source %in% keep.nodes$Id & Chile_edgelist_orig$Target %in% keep.nodes$Id

keep.edges <- Chile_edgelist_orig[keep.rows, ]

chile.zoom3.net <- network(keep.edges, directed = F)

chile.zoom3.net %v% "lat" <- Chile_attributes[network.vertex.names(chile.zoom3.net), "Latitude"]
chile.zoom3.net %v% "lon" <- Chile_attributes[network.vertex.names(chile.zoom3.net), "Longitude"]

node.color3 <- Chile_attributes[network.vertex.names(chile.zoom3.net), "col"]


p_3 <- GGally::print_if_interactive
p_3 <- ggnetworkmap(
  chile.zoom3, chile.zoom3.net, size = 3, great.circles = FALSE,
  segment.color = "black", node.color = node.color3
)

p_3

#plot
p_3+
  xlim(c(lon.min.3, lon.max.3)) +
  ylim(c(lat.min.3, lat.max.3)) +
  geom_point(data = dat.extra, shape = 15, color = "#c20320", size = 6)





#############################################
### Trump Network
##########################################
data("trump.world")
g<- trump.world

# basic network graph

nodes.display <- igraph::closeness(g) >= 0.0001319784
nodes.display[c(744, 331, 217, 195, 116, 672, 1280)] <- TRUE


V(g)$color <- ifelse(nodes.display, "red", "#d4d7d9")
V(g)$alpha <- ifelse(nodes.display , 1, 0)
V(g)$size <- ifelse(nodes.display, 0.5, 0.1)
V(g)$class<- ifelse(nodes.display, orig_nodes, NA)

V(g)$label <- ifelse(nodes.display, V(g)$name, NA)


set.seed(123)

ggraph(g, layout = 'graphopt') +
  ggtitle('TrumpWorld') +
  geom_edge_link2(edge_alpha = 0.1) +
  geom_node_point(size = V(g)$size,
                  color = V(g)$color,
                  legend = F) +
  geom_node_text(aes(label = label),
                 colour = 'red',
                 size=2,
                 show.legend = FALSE,
                 family = "serif",
                 repel = T) +
  theme_graph()



