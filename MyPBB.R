library(shiny)
library(shinydashboard)
library(ggplot2)

# Custom CSS for modern look
custom_css <- "
  .skin-blue .main-header .logo {
    background-color: #2c3e50;
    color: white;
    font-weight: bold;
  }
  .skin-blue .main-header .navbar {
    background-color: #34495e;
  }
  .skin-blue .main-sidebar {
    background-color: #2c3e50;
  }
  .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
    background-color: #2980b9;
  }
  .skin-blue .main-sidebar .sidebar .sidebar-menu a {
    color: white;
  }
  .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
    background-color: #2980b9;
  }
  .box.box-primary {
    border-top-color: #2980b9;
  }
  .btn-primary {
    background-color: #2980b9;
    border-color: #2980b9;
  }
  .btn-primary:hover {
    background-color: #1c5a8a;
    border-color: #1c5a8a;
  }
"

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "My PBB"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Input Data", tabName = "input", icon = icon("edit")),
      menuItem("Hasil Perhitungan PBB", tabName = "hasil", icon = icon("calculator")),
      menuItem("Visualisasi NJOP", tabName = "njop", icon = icon("chart-bar")),
      menuItem("Visualisasi PBB", tabName = "pbb", icon = icon("chart-pie"))
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML(custom_css))), # Apply custom CSS
    tabItems(
      tabItem(tabName = "input",
              fluidRow(
                box(title = "Input Data", status = "primary", solidHeader = TRUE, width = 12,
                    numericInput("luas_tanah", "Luas Tanah (m²):", value = 0, min = 0),
                    numericInput("harga_tanah", "Harga Tanah per m²:", value = 0, min = 0),
                    numericInput("luas_bangunan", "Luas Bangunan (m²):", value = 0, min = 0),
                    numericInput("harga_bangunan", "Harga Bangunan per m²:", value = 0, min = 0),
                    checkboxInput("kolam_renang", "Memiliki Kolam Renang?", value = FALSE),
                    conditionalPanel(
                      condition = "input.kolam_renang == true",
                      numericInput("luas_kolam", "Luas Kolam Renang (m²):", value = 0, min = 0),
                      numericInput("njop_kolam", "NJOP Kolam Renang per m²:", value = 0, min = 0)
                    ),
                    checkboxInput("pagar_mewah", "Memiliki Pagar Mewah?", value = FALSE),
                    conditionalPanel(
                      condition = "input.pagar_mewah == true",
                      numericInput("panjang_pagar", "Panjang Pagar (m):", value = 0, min = 0),
                      numericInput("tinggi_pagar", "Tinggi Pagar (m):", value = 0, min = 0),
                      numericInput("njop_pagar", "NJOP Pagar per m²:", value = 0, min = 0)
                    ),
                    selectInput("njoptkp", "Pilih NJOPTKP:", 
                                choices = list(
                                  "Bantul = Rp10.000.000" = 10000000,
                                  "Sleman = Rp15.000.000" = 15000000,
                                  "Kulon Progo = Rp25.000.000" = 25000000,
                                  "Gunung Kidul = Rp10.000.000" = 10000001,
                                  "Yogyakarta = Rp20.000.000" = 20000000
                                )),
                    actionButton("hitung", "Hitung PBB", class = "btn-primary")
                )
              )
      ),
      
      tabItem(tabName = "hasil",
              fluidRow(
                box(title = "Hasil Perhitungan PBB", status = "primary", solidHeader = TRUE, width = 12,
                    verbatimTextOutput("hasil")
                )
              )
      ),
      
      tabItem(tabName = "njop",
              fluidRow(
                box(title = "Visualisasi NJOP", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("njop_plot")
                )
              )
      ),
      
      tabItem(tabName = "pbb",
              fluidRow(
                box(title = "Visualisasi PBB", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("pbb_plot")
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$hitung, {
    # Check for missing values
    if (anyNA(c(input$luas_tanah, input$harga_tanah, input$luas_bangunan, input$harga_bangunan))) {
      cat("Input values cannot be missing.\n")
      return(NULL)
    }
    
    # Perform calculations using double values
    luas_tanah <- as.double(input$luas_tanah)
    harga_tanah <- as.double(input$harga_tanah)
    luas_bangunan <- as.double(input$luas_bangunan)
    harga_bangunan <- as.double(input$harga_bangunan)
    
    nilai_tanah <- luas_tanah * harga_tanah
    nilai_bangunan <- luas_bangunan * harga_bangunan
    
    if (input$kolam_renang) {
      if (anyNA(c(input$luas_kolam, input$njop_kolam))) {
        cat("Input values for Kolam Renang cannot be missing.\n")
        return(NULL)
      }
      luas_kolam <- as.double(input$luas_kolam)
      njop_kolam <- as.double(input$njop_kolam)
      nilai_kolam <- luas_kolam * njop_kolam
    } else {
      nilai_kolam <- 0.0
    }
    
    if (input$pagar_mewah) {
      if (anyNA(c(input$panjang_pagar, input$tinggi_pagar, input$njop_pagar))) {
        cat("Input values for Pagar Mewah cannot be missing.\n")
        return(NULL)
      }
      panjang_pagar <- as.double(input$panjang_pagar)
      tinggi_pagar <- as.double(input$tinggi_pagar)
      njop_pagar <- as.double(input$njop_pagar)
      nilai_pagar <- panjang_pagar * tinggi_pagar * njop_pagar
    } else {
      nilai_pagar <- 0.0
    }
    
    total_njop <- nilai_tanah + nilai_bangunan + nilai_kolam + nilai_pagar
    
    # Pengurangan NJOPTKP
    njoptkp <- as.numeric(input$njoptkp)
    nilai_setelah_njoptkp <- total_njop - njoptkp
    
    # Perhitungan PBB berdasarkan kondisi
    if (nilai_setelah_njoptkp < 1000000000) {
      pbb_awal <- nilai_setelah_njoptkp * 0.2
    } else {
      pbb_awal <- nilai_setelah_njoptkp * 0.4
    }
    
    # PBB akhir
    pbb <- pbb_awal * 0.005
    
    # Tampilkan hasil menggunakan cat
    hasil <- paste(
      "NJOP Tanah: Rp", format(nilai_tanah, big.mark = ","), "\n",
      "NJOP Bangunan: Rp", format(nilai_bangunan, big.mark = ","), "\n",
      if (input$kolam_renang) paste("NJOP Kolam Renang: Rp", format(nilai_kolam, big.mark = ","), "\n") else "",
      if (input$pagar_mewah) paste("NJOP Pagar: Rp", format(nilai_pagar, big.mark = ","), "\n") else "",
      "Total NJOP: Rp", format(total_njop, big.mark = ","), "\n",
      "NJOP setelah NJOPTKP: Rp", format(nilai_setelah_njoptkp, big.mark = ","), "\n",
      "PBB Terutang: Rp", format(pbb, big.mark = ","), "\n"
    )
    
    output$hasil <- renderPrint({ cat(hasil) })
    
    # Data untuk visualisasi
    njop_data <- data.frame(
      Komponen = c("Tanah", "Bangunan", if(input$kolam_renang) "Kolam Renang" else NULL, if(input$pagar_mewah) "Pagar" else NULL),
      Nilai = c(nilai_tanah, nilai_bangunan, if(input$kolam_renang) nilai_kolam else NULL, if(input$pagar_mewah) nilai_pagar else NULL)
    )
    njop_data <- njop_data[!is.na(njop_data$Komponen), ]  # Remove components with NA values
    
    # Plot NJOP Komponen
    output$njop_plot <- renderPlot({
      ggplot(njop_data, aes(x = Komponen, y = Nilai, fill = Komponen)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        theme_minimal() +
        scale_fill_brewer(palette = "Set3") +
        labs(title = "Komponen NJOP", y = "Nilai (Rp)", x = "") +
        theme(plot.title = element_text(hjust = 0.5))
    })
    
    # Data untuk PBB
    pbb_data <- data.frame(
      Kategori = c("Total NJOP", "NJOP setelah NJOPTKP", "PBB Terutang"),
      Nilai = c(total_njop, nilai_setelah_njoptkp, pbb)
    )
    
    # Plot PBB
    output$pbb_plot <- renderPlot({
      ggplot(pbb_data, aes(x = Kategori, y = Nilai, fill = Kategori)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        theme_minimal() +
        scale_fill_brewer(palette = "Set2") +
        labs(title = "Perhitungan PBB", y = "Nilai (Rp)", x = "") +
        theme(plot.title = element_text(hjust = 0.5))
    })
  })
}

num = 12383828281831342
print ("original number :")
print (num)

# after global setting for
# options 
options(scipen = 100, digits = 4)

# declaring the number
num = 12383828281831342
print ("Modified number :")
print (num)


# Run Shiny app
shinyApp(ui = ui, server = server)

library(rsconnect)
rsconnect::deployApp(appDir = "C:/MyPBB", appPrimaryDoc = "MyPBB.R")

install.packages("purrr")
library(purrr)
.libPaths() %>%
  set_names() %>%
  map(function(lib) {
    .packages(all.available = TRUE, lib.loc = lib) %>%
      keep(function(pkg) {
        f <- system.file('Meta', 'package.rds', package = pkg, lib.loc = lib)
        tryCatch({readRDS(f); FALSE}, error = function(e) TRUE)
      })
  })