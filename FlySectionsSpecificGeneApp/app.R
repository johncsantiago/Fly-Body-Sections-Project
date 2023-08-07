
##to deploy the app to Rshiny server
#library(rsconnect)
#rsconnect::deployApp("/Users/johncsantiago/Documents/GitHub/Fly-Body-Sections-Project/FlySectionsSpecificGeneApp/")

##Load necessary libraries
library(shiny)
library(ggfortify)
library(plotly)
library(colourpicker)

##load all files from github

git.dir = "https://raw.githubusercontent.com/johncsantiago/Fly-Body-Sections-Project/master/Data/"
cpmdata = read.csv(paste0(git.dir,"cpmdata.csv"),row.names = 1)
groups  = read.csv(paste0(git.dir,"groups.csv"),row.names = 1)
##TKT.EdgeR = read.csv(paste0(git.dir,"TKT.EdgeR.Table.csv"),row.names = 1)
convert=read.csv("https://raw.githubusercontent.com/DavidRandLab/Santiago-et-al-2021-BMC-Genomics/main/Data%20Files/FBgnConversionTable.csv",row.names=1)

##Add additional groups for color factors
groups$GxS = paste0(groups$Species, groups$Sex)
groups$GxT = paste0(groups$Species, groups$Treatment)
groups$GxSxT = paste0(groups$GxS, groups$Treatment)

##FBgn2Symbol; Variable for converting FBgn to symbol
temp                   = convert[,c("updated.FBgn","Symbol")]
temp2                  = setdiff(row.names(cpmdata),row.names(convert))
temp2                  = matrix(rep(temp2,2), ncol=2)
row.names(temp2)       = temp2[,1]
colnames(temp2)        = colnames(temp)
FBgn2Symbol            = rbind(temp,temp2)
colnames(FBgn2Symbol)  = c("FBgn", "Symbol")

##groups. use; Variable used by the "Filter gene set" options 
groups.use = matrix(0,nrow = nrow(groups), ncol = 10)
row.names(groups.use) = row.names(groups)
colnames(groups.use) = c("ALL",
                         "OO", "SO", 
                         "M", "F", 
                         "C", "R",
                         "H", "T", "A")
groups.use[ , "ALL"] = 0
groups.use[groups$Species   == "OO", "OO"]  = 1
groups.use[groups$Species   == "SO", "SO"]  = 1
groups.use[groups$Sex       == "Male", "M"]   = 1
groups.use[groups$Sex       == "Female", "F"]   = 1
groups.use[groups$Treatment == "Control", "C"]   = 1
groups.use[groups$Treatment == "Rapa", "R"]  = 1
groups.use[groups$Tissue     == "Head", "H"]  = 1
groups.use[groups$Tissue     == "Thorax", "T"]  = 1
groups.use[groups$Tissue     == "Abdomen", "A"]  = 1

##all.genes; Variable used to copy gene set to clipboard so each gene is on a new line
temp=FBgn2Symbol[,"Symbol"]
i=2
temp2=paste0(temp[1],"\n")
while(i<=length(temp)){
  temp2=paste0(temp2,paste0(temp[i],"\n"))
  i=i+1
}
temp2
all.genes = temp2


##Starts the code for user interface. Sidebar controls and Main panel display
ui <- fluidPage(
  
##Sidebar control code 
  titlePanel("Fly Body Sections RNAseq Experiment: Specific Gene Expression"),
  sidebarLayout(position="left", sidebarPanel(
    
##General plot customization tools 
    ##gene.Controls; Gene selection tool. Uses a reactive variable
    uiOutput("geneControls"),
    p("Select Gene: Use gene symbol if available. autocompletes"),
    
    ##url; Link to selected gene flybase summary page. Uses a reactive variable
    uiOutput("url"),
    br(),
    
    fluidRow(
    ##cond.select; A a checkbox selection with multiple selections allowed. Used to select conditions to exclude when creating the boxplot
      column(6,
             checkboxGroupInput("cond.select", h5("Select Conditions to Exclude"), 
                                choices = list("OreR;OreR" = 2, 
                                               "sm21;OreR" = 3,
                                               "Male"      = 4,
                                               "Female"    = 5,
                                               "Control"   = 6,
                                               "Rapamycin" = 7,
                                               "Head"      = 8,
                                               "Thorax"   = 9,
                                               "Abdomen"    = 10)),
             ),
    ),
    
##This section is used to customize the colors of the box plots 
  fluidRow(
    ##colfactor; A drop down tab used to select the factor to differentiate by color
      column(6,
             selectInput("colfactor", h5("Select Factor for Color"), 
                         choices = list("Genotype x Sex"       = "GxS",
                                        "Genotype x Treatment" = "GxT",
                                        "Genotype x Sex x Treatment" = "GxSxT",
                                        "Group"                = "Group",
                                        "Genotype"             = "Species",
                                        "Treatment"            = "Treatment", 
                                        "Sex"                  = "Sex",
                                        "Tissue"               = "Tissue"),
                         selected = "GxSxT"),),
      
    ##colselect; A drop down tab that lets you select a color to change. Can only customize up to 4 factor colors. Once selected, a new tab will open that can be selected which will open up a color pallette to choose from.
      column(6,
             
             selectInput("colselect", h5("Select Color"), 
                         choices = list("Choose Condition" = 0,
                                        "Color 1"          = 1, 
                                        "Color 2"          = 2,
                                        "Color 3"          = 3,
                                        "Color 4"          = 4,
                                        "Color 3"          = 5,
                                        "Color 4"          = 6,
                                        "Color 3"          = 7,
                                        "Color 4"          = 8),
                         selected = 0),
      ),),
    
    conditionalPanel(
      condition = "input.colselect == 1",
      colourInput("color1", "Color 1",
                  "brown",showColour = 'background')),
    
    
    conditionalPanel(
      condition = "input.colselect == 2",
      colourInput("color2", "Color 2",
                  "cornflowerblue",showColour = 'background')),
    
    conditionalPanel(
      condition = "input.colselect == 3",
      colourInput("color3", "Color 3",
                  "darkgoldenrod1",showColour = 'background')),
    
    conditionalPanel(
      condition = "input.colselect == 4",
      colourInput("color4", "Color 4",
                  "chartreuse4",showColour = 'background')),

      
    conditionalPanel(
      condition = "input.colselect == 5",
      colourInput("color5", "Color 5",
                  "palevioletred",showColour = 'background')),
    
    conditionalPanel(
      condition = "input.colselect == 6",
      colourInput("color6", "Color 6",
                  "cadetblue",showColour = 'background')),

      
    conditionalPanel(
      condition = "input.colselect == 7",
      colourInput("color7", "Color 7",
                  "burlywood4",showColour = 'background')),
    
    conditionalPanel(
      condition = "input.colselect == 8",
      colourInput("color8", "Color 8",
                  "darkseagreen",showColour = 'background'))
,
    
br(),
  
    width = 3),

##Figure display settings    
  mainPanel(
    ##Display the boxplot
    plotlyOutput(outputId = "plot",
                 height=600, width=1250),
    ),),
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$url <- renderUI({
    page = a(paste0("Flybase Page for ", input$gene), href = paste0("http://flybase.org/reports/",FBgn2Symbol[FBgn2Symbol[,"Symbol"]==input$gene,"FBgn"]), target="_blank")
    tagList(page)
  })
    
  output$geneControls =  renderUI({
    gene.choices<<-FBgn2Symbol[row.names(cpmdata),2]
    selectizeInput("gene", label = "Select Gene", 
                   choices = gene.choices,
                   options = list(create = TRUE,
                                  ##maxOptions = 5,
                                  placeholder = 'select a gene name'),
                   selected="Tor")
  })
  
  output$plot <- renderPlotly({
    
    gene.name = FBgn2Symbol[FBgn2Symbol[,"Symbol"]==input$gene, "FBgn"]
    
    minlim    = min(as.numeric(na.omit(cpmdata[gene.name,])))
    maxlim    = max(as.numeric(na.omit(cpmdata[gene.name,])))
      
    if(length(input$cond.select) == 0){
      use.samples = row.names(groups.use)
    }    

    
    if(length(input$cond.select)>0){
      use.samples=row.names(groups.use)[apply(groups.use[,c(1,as.numeric(input$cond.select))],1,sum)==0]
    }
      
    boxdata=data.frame(cpm   = as.numeric(cpmdata[gene.name,use.samples]),
                       color = groups[use.samples,input$colfactor],
                       group = groups[use.samples,"Group"],
                       ID    = use.samples)
    
    row.names(boxdata) = boxdata$ID
    
    
    Table.names = groups[boxdata$ID,]
    Table.names$Tissue = factor(Table.names$Tissue, levels=c("Head","Thorax", "Abdomen"))
    Table.names = Table.names[order(Table.names$Tissue),]
    Table.names = Table.names[order(Table.names$Treatment),]
    
    Table.names = Table.names[order(Table.names$Species),]
    Table.names = Table.names[order(Table.names$Sex),]

    
    boxcolors = c(input$color1,input$color2,
                  input$color3,input$color4,
                  input$color5,input$color6,
                  input$color7,input$color8)
    
    if(length(unique(boxdata$color))<9){
      boxcolors = c(input$color1, input$color2,
                    input$color3, input$color4,
                    input$color5, input$color6,
                    input$color7, input$color8)[1:length(unique(boxdata$color))]      
    }

    ##color.order = boxdata
    ##color.order$color = factor(color.order$color, levels = unique(color.order$color))
    
    boxdata = boxdata[row.names(Table.names),]
    boxdata$group = factor(boxdata$group, levels=(unique(boxdata$group)))
    boxdata$color = factor(boxdata$color, levels=unique(boxdata$color))

    
    ##color.order = unique(as.numeric(color.order[row.names(boxdata), "color"]))
    
    
    fig = plot_ly(data = boxdata,
                  x = ~group,
                  y = ~cpm,
                  name = ~group,
                  type = "box",
                  boxpoints = "all",
                  pointpos = 0,
                  hoveron = 'points',
                  jitter = 0,
                  hovertext = paste0("Sample = ", boxdata$ID, "\nCPM = ", round(boxdata$cpm, 1)),
                  hoverinfo = 'text',
                  color = ~color,
                  colors = boxcolors,
                  marker = list(size = 15,
                                line = list(color = "black", width = 1.5)))
    
  })  

 

}


shinyApp(ui = ui, server = server, options)