library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(bslib)
library(htmltools)
library(fontawesome)

# ---- Load & Merge Data ----
# Make sure your shapefile and CSV paths are correct
wards <- st_read("./data/boundaries/geo_export_f582634c-2a9b-46ab-87c0-3efdb0090ab7.shp")

alders <- read.csv("./data/ChicagoAlderpersons.csv", stringsAsFactors = FALSE)
alders$Ward <- as.character(alders$Ward)
wards$ward <- as.character(wards$ward)

# Join the shapefile and alder data
wards_merged <- wards %>%
  left_join(alders, by = c("ward" = "Ward"))

wards_merged <- st_transform(wards_merged, crs = 4326)

wards_clean <- wards_merged %>%
  mutate(geometry = st_make_valid(geometry))


wards_50 <- wards_clean %>%
  group_by(ward, Name, Email, Office.Phone, Website, City.Hall.Address,
           RCV.Position..1.best..4.worst..0.unknown.,
           Charter.Comission.Resolution..16JUL25.,
           Leadership.Roles, Military.Affiliated, Notes) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

wards_50 <- st_make_valid(wards_50) %>%
  st_simplify(dTolerance = 50)   # reduces tiny lines


library(tidygeocoder)
#ward_offices <- alders %>%
#  select(ward = Ward, Name, Office.Address) %>%
 # mutate(full_address = paste0(Office.Address, ", Chicago, IL")) %>%
#  geocode(address = full_address, method = "osm")

# Save so you don’t geocode every run
#write.csv(ward_offices, "./data/ward_offices_geocoded.csv", row.names = FALSE)

ward_offices <- read.csv("./data/ward_offices_geocoded.csv")

# ---- UI ----
ui <- tagList(
  # Banner always on top
  fluidRow(
    column(12,
           div(style="text-align:center; background:#E6F2FF; padding:5px;",
               tags$img(
                 src = "bannerbigvoters.png",
                 style = "width:50%; height:50%; border-radius:0; margin-bottom:10px;"
               )
           )
    )
  ),
  
  # Navbar comes underneath banner
  navbarPage(
  title = NULL,
  theme = bs_theme(
    bg = "#E6F2FF",
    fg = "#000000",
    primary = "#EA80FC", secondary = "#48DAC6",
    base_font = font_google("Open Sans"),
    heading_font = font_google("Fjalla One")
  ),
  header = tags$style(HTML("
    /* Style navbar tabs */
    .navbar-nav > li > a {
      font-weight: 600;
      padding: 12px 18px;
    }
    .navbar-nav > .active > a {
      border-bottom: 4px solid #EA80FC !important;
      color: #EA80FC !important;
      background-color: transparent !important;
    }
    .navbar-nav > li > a:hover {
      color: #48DAC6 !important;
    }
  ")),
    
  tabPanel("Home + Charter Tracker",
           fluidPage(
             fluidRow(
               # Left column: intro + context
               column(
                 width = 4,
                 h2("Welcome to the City Charter Tracker!"),
                 p("This dashboard helps track which alderpeople support the Chicago City Charter, 
    who has hosted Charter Chats, and how you can get involved."),
                 tags$hr(),
                 h4("How it Works"),
                 tags$ul(
                   tags$li("✅ Wards highlighted in turquoise = sponsor/support Charter Resolution"),
                   tags$li("❌ Gray wards = no resolution yet"),
                 ),
                 tags$hr(),
                 # CTA button as link
                 tags$a(
                   href = "https://chidot.substack.com/p/call-to-action?r=3yvpa&utm_campaign=post&utm_medium=web&triedRedirect=true",
                   target = "_blank",
                   class = "btn btn-primary",
                   "Click here to get involved with Chicago Department of Transformation's many City Charter initiatives and ambitions!"
                 )
               ),
               
               # Right column: the map
               column(
                 width = 8,
                 leafletOutput("charter_map", height = 600),
                 # inside the left column (under How it Works)
                 tags$hr(),
                 h4("Legislative Champions and Milestones"),
                 tags$ul(
                   tags$li("Ald. Villegas — hosted a Charter Chat at Ukrainian Cultural Center on August 18th ✅"),
                   tags$li("Ald. Vasquez — answered questions at a Charter Chat on at Budlong Library April 2nd 👀"),
                   tags$li("IL State Rep. Kam Buckner — working on State legislation that will allow Chicago to adopt a City Charter and sponsored a Charter Chat at King Branch Library 🤝")
                 ),
               )
             )
           )
  ),
  
    
  tabPanel("Opportunities for the Brave",
           fluidPage(
             fluidRow(
               # Left column: poster
               column(
                 width = 6,
                 tags$img(
                   src = "bigvoterswords.png",
                   style = "max-width:100%; border-radius:12px; margin-bottom:20px; 
                   box-shadow:0px 0px 8px rgba(0,0,0,0.2);"
                 )
               ),
               
               # Right column: mission + resources + extra flyer
               column(
                 width = 6,
                 h3("Our Mission"),
                 p("The City Charter is a people-powered movement to make Chicago government 
           more accountable, transparent, and fair. By engaging residents in Charter Chats 
           and building momentum across neighborhoods, we ensure decisions are rooted 
           in the voices of Chicagoans."),
                 
                 h4("Resources"),
                 tags$ul(
                   tags$li(tags$a(href = "https://chidot.substack.com", target = "_blank", "ChiDOT Substack")),
                   tags$li(tags$a(href = "https://chidot.substack.com/p/call-to-action", target = "_blank", "Evergreen Call to Action")),
                   tags$li(tags$a(href = "https://www.chicago.gov/city/en/depts/mayor/iframe/lookup_ward_and_alderman.html", target = "_blank", "City of Chicago Government")),
                   tags$li(tags$a(href = "https://www.eventbrite.com/e/run-the-movement-chicago-veterans-empowerment-530pm-at-city-hall-tickets-1689504957809", target = "_blank", "Run the Movement"))
                 ),
                 
                 tags$hr(),
                 h4("Join Us"),
                 tags$img(
                   src = "runthemovementveteransfinalfinal.png",
                   style = "max-width:70%; border-radius:8px; margin-top:12px; 
                   box-shadow:0px 0px 6px rgba(0,0,0,0.15);"
                 )
               )
             )
           )
  ),
    
  tabPanel("Deliberative Democracy",
           fluidPage(
             fluidRow(
               # Left column: video
               column(
                 width = 5,
                 tags$video(
                   src = "deliberative_democracy.mp4", 
                   type = "video/mp4", 
                   controls = "controls",  
                   style = "width:100%; max-height:500px; border-radius:12px; 
                   box-shadow:0px 0px 10px rgba(0,0,0,0.2);"
                 )
               ),
               
               # Right column: text
               column(
                 width = 7,
                 h3("What is Deliberative Democracy?"),
                 p("Deliberative democracy is about more than meetings — it’s about creating 
           spaces where people listen, learn, and imagine together. It is rooted in the 
           belief that citizens can do more than react to politics: we can shape it."),
                 
                 p("The City Charter movement introduces Chicagoans to this idea in a tangible way. 
           Through Charter Chats, neighbors gather in libraries, parks, and community spaces 
           to deliberate on how we can fix our institutions and strengthen our democracy."),
                 
                 p("Chicago has always been a city of builders, fighters, and dreamers. 
           The Charter invites us to be brave enough to imagine: what if we built a city 
           where trust in government was earned, where citizens had the tools to shape policy, 
           and where democracy was not just protected, but renewed?"),
                 
                 strong("Together, we can create a Chicago where civic power is joyful, accountable, and unstoppable.")
               )
             )
           )
  )
  ),
  tags$footer(
    style = "
      position:relative;
      bottom:0;
      width:100%;
      background:#E6F2FF;
      text-align:center;
      padding:12px;
      margin-top:20px;
      font-size:14px;
      font-weight:500;
      color:#333;
      border-top:2px solid #EA80FC;
    ",
    "🌆 Chicago Department of Transformation — Building trust, joy, and civic power together."
  )
)


# ---- Server ----
server <- function(input, output, session) {
  output$charter_map <- renderLeaflet({
    leaflet(wards_50) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~ifelse(`Charter.Comission.Resolution..16JUL25.` != "", "#48DAC6", "#CCCCCC"),
        color = "#333", weight = 1, opacity = 1,
        fillOpacity = 0.6,
        label = ~paste0("Ward ", ward, " — ", Name),
        popup = ~paste0(
          "<strong>Ward ", ward, ": ", Name, "</strong><br>",
          ifelse(Charter.Comission.Resolution..16JUL25. != "", 
                 "✅ Sponsor/Resolution on Charter<br>", 
                 "❌ No public resolution yet<br>")
        ),
        highlightOptions = highlightOptions(weight = 3, color = "#EA80FC", bringToFront = TRUE)
      ) %>%
      addLegend("bottomright", 
                colors = c("#48DAC6", "#CCCCCC"),
                labels = c("Resolution on Charter", "No Resolution"),
                title = "City Charter Status",
                opacity = 0.9
      )
  })
}


# ---- Run App ----
shinyApp(ui, server)
