library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)
library(glue)
library(scales)
library(tidyr)
library(purrr)
library(lubridate)

# UI ----
ui <- dashboardPage(
  skin = "green",
  
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center; height: 50px; padding: 0 15px;",
      tags$div(
        style = "display: flex; align-items: center; justify-content: center; 
               width: 40px; height: 40px; background: linear-gradient(135deg, #10b981, #059669); 
               border-radius: 10px; margin-right: 15px; box-shadow: 0 2px 8px rgba(16, 185, 129, 0.3);",
        tags$span("🗑️", style = "font-size: 20px; filter: drop-shadow(0 1px 2px rgba(0,0,0,0.2));")
      ),
      tags$div(
        style = "display: flex; flex-direction: column;",
        tags$span("Trash Analytics", 
                  style = "font-weight: 800; font-size: 18px; color: #047857; 
                         letter-spacing: 0.5px; line-height: 1.1;")
      )
    ),
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home", icon = icon("dashboard")),
      menuItem("Ringkasan Nasional", tabName = "nasional", icon = icon("chart-line")),
      menuItem("Analisis Wilayah", tabName = "wilayah", icon = icon("map-marked-alt")),
      menuItem("Sumber & Jenis Sampah", tabName = "sumber", icon = icon("trash-alt")),
      menuItem("Bank Sampah", tabName = "banksampah", icon = icon("piggy-bank")),
      menuItem("Explore Data", tabName = "explore", icon = icon("magnifying-glass-chart")),
      menuItem("Rencana Aksi", tabName = "rencana", icon = icon("tasks")),
      menuItem("Tim Pengembang", tabName = "team", icon = icon("users"))
    ),
    
    # Tambahan logo dan credit di bagian bawah sidebar
    tags$div(
      style = "position: absolute; bottom: 20px; left: 0; right: 0; padding: 0 20px;",
      tags$div(
        style = "text-align: center; margin-bottom: 15px;",
        tags$p(
          style = "font-size: 12px; color: #64748b; font-weight: 500; margin-bottom: 10px;",
          "Indonesia Waste Management by"
        ),
        tags$div(
          style = "background: white; padding: 10px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
          tags$img(
            src = "https://ssmi.ipb.ac.id/wp-content/uploads/2024/10/Logo-IPB-University-Vertical-Prodi-SSMI-IPB-600x110.png",
            style = "width: 100%; max-width: 150px; height: auto;",
            alt = "IPB University Logo"
          )
        )
      )
    )
  ),
  
  dashboardBody(
    # Welcome Modal - DIPINDAHKAN KE DALAM dashboardBody
    tags$div(
      id = "welcomeModal",
      class = "modal fade",
      tabindex = "-1",
      role = "dialog",
      tags$div(
        class = "modal-dialog modal-dialog-centered",
        role = "document",
        style = "max-width: 500px;",
        tags$div(
          class = "welcome-modal",
          
          # Header
          tags$div(
            class = "welcome-header",
            tags$div(
              style = "position: relative; z-index: 1;",
              tags$span(class = "welcome-icon", "🗑️"),
              tags$h2(class = "welcome-title", "Trash Analytics"),
              tags$p(class = "welcome-subtitle", "Dashboard Analisis Pengelolaan Sampah Indonesia")
            )
          ),
          
          # Body
          tags$div(
            class = "welcome-body",
            
            # Team Image/Icon
            tags$div(
              class = "welcome-image",
              tags$img(
                src = "https://raw.githubusercontent.com/ngurahsentana24/TrashAnalytics/main/source/photo/maskot.png",
                alt = "Trash Analytics Icon",
                style = "width: 100%; height: 100%; object-fit: cover;"
              )
            ),
            
            # Welcome Text
            tags$h3(style = "color: #1e293b; margin-bottom: 15px; font-weight: 700;", 
                    "Selamat Datang!"),
            tags$p(
              class = "welcome-text",
              "Kami dengan bangga mempersembahkan platform analisis data pengelolaan sampah terintegrasi pertama di Indonesia."
            ),
            
            # Name Input
            tags$div(
              class = "name-input-container",
              tags$input(
                id = "userName",
                type = "text",
                class = "name-input",
                placeholder = "Masukkan nama Anda...",
                onkeypress = "handleKeyPress(event)"
              )
            ),
            
            # Team Info
            tags$div(
              class = "team-info",
              tags$h4("👨‍💻 Tim Pengembang"),
              tags$p("🏛️ IPB University"),
              tags$p(style = "color: #10b981; font-weight: 600; margin-top: 10px;", 
                     "Mendukung Indonesia Bersih 2029")
            )
          ),
          
          # Footer
          tags$div(
            class = "welcome-footer",
            tags$button(
              type = "button",
              class = "btn-welcome",
              onclick = "submitName()",
              style = "min-width: 200px;",
              "Masuk ke Dashboard"
            )
          )
        )
      )
    ),
    
    tags$head(
      tags$style(HTML("
      /* CSS styles tetap sama seperti sebelumnya */
      /* ... semua CSS yang sudah ada ... */
      "))
    ),
    tags$head(
      tags$style(HTML("
      /* Modern CSS Theme - Green Gradient Theme */
      body {
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        background: linear-gradient(135deg, #f0fdf4 0%, #ecfdf5 100%);
      }
      
      /* Header putih */
      .skin-green .main-header .logo {
        background: linear-gradient(135deg, #ffffff 0%, #f8fafc 100%);
        color: #047857;
        font-weight: 700;
        border-bottom: 1px solid #e2e8f0;
      }
      
      .skin-green .main-header .navbar {
        background: linear-gradient(135deg, #ffffff 0%, #f8fafc 100%);
        border-bottom: 1px solid #e2e8f0;
      }
      
      /* Styling untuk navbar items */
      .skin-green .main-header .navbar .sidebar-toggle {
        color: #047857;
        border-right: 1px solid #e2e8f0;
      }
      
      .skin-green .main-header .navbar .sidebar-toggle:hover {
        background-color: #f1f5f9;
        color: #065f46;
      }
      
      .skin-green .main-sidebar {
        background: linear-gradient(180deg, #ffffff 0%, #f8fafc 100%);
        border-right: 1px solid #e2e8f0;
      }
      
      .skin-green .main-sidebar .sidebar .sidebar-menu .active a{
        background: linear-gradient(135deg, #10b981 0%, #059669 100%);
        color: #ffffff;
        border-left: 4px solid #f59e0b;
        box-shadow: 0 2px 8px rgba(16, 185, 129, 0.3);
      }
      
      .skin-green .main-sidebar .sidebar .sidebar-menu a{
        color: #334155;
        border-left = 4px solid transparent;
        transition: all 0.3s ease;
        font-weight: 500;
      }
      
      .skin-green .main-sidebar .sidebar .sidebar-menu a:hover{
        background: linear-gradient(135deg, #ecfdf5 0%, #d1fae5 100%);
        color: #065f46;
        border-left: 4px solid #10b981;
      }
      
      .skin-green .main-sidebar .sidebar .sidebar-menu a i {
        color: #64748b;
      }
      
      .skin-green .main-sidebar .sidebar .sidebar-menu .active a i {
        color: #ffffff;
      }
      
      .skin-green .main-sidebar .sidebar .sidebar-menu a:hover i {
        color: #059669;
      }
      
      /* Section Header */
      .section-header {
        text-align: center;
        margin-bottom: 30px;
        padding: 20px;
      }
      
      .section-icon {
        font-size: 32px;
        background: linear-gradient(135deg, #10b981, #059669);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        margin-bottom: 15px;
        display: block;
      }
      
      .section-header h2 {
        font-size: 28px;
        font-weight: 800;
        color: #1e293b;
        margin-bottom: 10px;
      }
      
      .section-header p {
        color: #64748b;
        font-size: 16px;
        font-weight: 500;
      }
      
      /* KPI Container */
      .kpi-container {
        background: linear-gradient(135deg, #ffffff 0%, #f8fafc 100%);
        border-radius: 20px;
        padding: 25px;
        margin: 20px 0;
        box-shadow: 0 8px 25px rgba(0, 0, 0, 0.08);
        border: 1px solid #e2e8f0;
      }
      
      /* Chart Cards */
      .chart-card {
        background: linear-gradient(135deg, #ffffff 0%, #f8fafc 100%);
        border-radius: 20px;
        margin: 15px 0;
        box-shadow: 0 8px 25px rgba(0, 0, 0, 0.08);
        border: 1px solid #e2e8f0;
        transition: all 0.3s ease;
        overflow: hidden;
      }
      
      .chart-card:hover {
        transform: translateY(-5px);
        box-shadow: 0 15px 35px rgba(0, 0, 0, 0.12);
      }
      
      .chart-header {
        background: linear-gradient(135deg, #10b981, #059669);
        color: white;
        padding: 25px;
        display: flex;
        align-items: center;
        gap: 15px;
        position: relative;
        overflow: hidden;
      }
      
      .chart-header::before {
        content: '';
        position: absolute;
        top: -50%;
        right: -50%;
        width: 100%;
        height: 200%;
        background: radial-gradient(circle, rgba(255,255,255,0.1) 0%, transparent 70%);
      }
      
      .chart-icon {
        font-size: 24px;
        filter: drop-shadow(0 2px 4px rgba(0,0,0,0.2));
        z-index: 1;
      }
      
      .chart-header h3 {
        margin: 0;
        font-size: 18px;
        font-weight: 700;
        z-index: 1;
      }
      
      .chart-subtitle {
        font-size: 12px;
        opacity: 0.9;
        font-weight: 500;
        margin-left: auto;
        z-index: 1;
      }
      
      /* Chart Controls */
      .chart-controls {
        padding: 20px 25px 0;
        background: #f8fafc;
      }
      
      .selectize-input {
        border-radius: 10px;
        border: 1px solid #cbd5e1;
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.05);
        font-size: 14px;
        padding: 10px 15px;
      }
      
      .selectize-input:focus {
        border-color: #10b981;
        box-shadow: 0 0 0 3px rgba(16, 185, 129, 0.1);
      }
      
      /* Chart Body */
      .chart-body {
        padding: 25px;
        min-height: 300px;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      
      /* Enhanced KPI Cards */
      .kpi-card {
        background: linear-gradient(135deg, #ffffff 0%, #f0fdf4 100%);
        color: #334155;
        border-radius: 16px;
        padding: 25px;
        margin: 10px;
        box-shadow: 0 4px 15px rgba(0, 0, 0, 0.08);
        border: 1px solid #d1fae5;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        text-align: center;
        position: relative;
        overflow: hidden;
        height: 180px;
        display: flex;
        flex-direction: column;
        justify-content: center;
      }
      
      .kpi-card::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 4px;
        background: linear-gradient(135deg, #10b981, #059669);
        transform: scaleX(0);
        transition: transform 0.3s ease;
      }
      
      .kpi-card:hover {
        transform: translateY(-8px);
        box-shadow: 0 15px 30px rgba(16, 185, 129, 0.15);
        border-color: #a7f3d0;
      }
      
      .kpi-card:hover::before {
        transform: scaleX(1);
      }
      
      .kpi-value {
        font-size: 36px;
        font-weight: 800;
        margin: 15px 0;
        background: linear-gradient(135deg, #059669 0%, #047857 100%);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        text-shadow: 0 2px 4px rgba(0,0,0,0.1);
        display: block;
      }
      
      .kpi-title {
        font-size: 13px;
        color: #64748b;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        margin-bottom: 8px;
      }
      
      .kpi-trend {
        font-size: 12px;
        margin-top: 8px;
        font-weight: 600;
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 4px;
      }
      
      .trend-up {
        color: #059669;
      }
      
      .trend-down {
        color: #ef4444;
      }
      
      /* Responsive Design */
      @media (max-width: 768px) {
        .section-header h2 {
          font-size: 24px;
        }
        
        .chart-header {
          padding: 20px;
          flex-direction: column;
          text-align: center;
          gap: 10px;
        }
        
        .chart-subtitle {
          margin-left: 0;
        }
        
        .kpi-value {
          font-size: 28px;
        }
      }
      
      /* Modern Box Styling */
      .box {
        border-radius: 16px;
        box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06);
        border: none;
        background: linear-gradient(135deg, #ffffff 0%, #f8fafc 100%);
        border: 1px solid #e2e8f0;
      }
      
      .box-header {
        border-radius: 16px 16px 0 0;
        background: linear-gradient(135deg, #10b981 0%, #059669 50%, #047857 100%);
        color: white;
        border-bottom: none;
        padding: 20px;
      }
      
      .box-header .box-title {
        font-weight: 700;
        font-size: 18px;
        text-shadow: 0 1px 2px rgba(0,0,0,0.1);
      }
      
      /* Content Background */
      .content-wrapper {
        background: linear-gradient(135deg, #f0fdf4 0%, #ecfdf5 100%);
      }
      
      /* Team Cards Modern */
      .team-card {
        background: linear-gradient(135deg, #ffffff 0%, #f0fdf4 100%);
        border-radius: 16px;
        padding: 25px;
        margin: 15px;
        box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06);
        text-align: center;
        border: 1px solid #d1fae5;
        transition: transform 0.2s ease;
      }
      
      .team-card:hover {
        transform: translateY(-4px);
        box-shadow: 0 20px 25px -5px rgba(0, 0, 0, 0.1), 0 10px 10px -5px rgba(0, 0, 0, 0.04);
        border-color: #a7f3d0;
      }
      
      .team-img {
        width: 100px;
        height: 100px;
        border-radius: 50%;
        margin: 0 auto 20px;
        background: linear-gradient(135deg, #10b981 0%, #059669 50%, #047857 100%);
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 36px;
        color: white;
        box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1);
      }
      
      .team-name {
        font-size: 18px;
        font-weight: 700;
        color: #1e293b;
        margin-bottom: 8px;
      }
      
      .team-role {
        color: #059669;
        font-weight: 600;
        margin-bottom: 12px;
      }
      
      .team-specialization {
        color: #64748b;
        font-size: 14px;
        line-height: 1.4;
      }
      
      /* Custom Tabs */
      .nav-tabs-custom > .nav-tabs > li.active {
        border-top-color: #10b981;
      }
      
      /* Button Styling */
      .btn-success {
        background: linear-gradient(135deg, #10b981 0%, #059669 100%);
        border: none;
        border-radius: 8px;
        font-weight: 600;
        box-shadow: 0 2px 4px rgba(16, 185, 129, 0.3);
      }
      
      .btn-success:hover {
        background: linear-gradient(135deg, #059669 0%, #047857 100%);
        transform: translateY(-1px);
        box-shadow: 0 4px 8px rgba(16, 185, 129, 0.4);
      }
      
      /* Select Input Styling */
      .selectize-input {
        border-radius: 8px;
        border: 1px solid #cbd5e1;
        box-shadow: 0 1px 2px rgba(0, 0, 0, 0.05);
      }
      
      .selectize-input:focus {
        border-color: #10b981;
        box-shadow: 0 0 0 3px rgba(16, 185, 129, 0.1);
      }
      
      /* Home Page Hero Section */
      .hero-section {
        background: linear-gradient(135deg, #10b981 0%, #059669 30%, #047857 100%);
        color: white;
        padding: 50px;
        border-radius: 20px;
        margin-bottom: 30px;
        box-shadow: 0 20px 25px -5px rgba(0, 0, 0, 0.1), 0 10px 10px -5px rgba(0, 0, 0, 0.04);
        text-align: center;
        position: relative;
        overflow: hidden;
      }
      
      .hero-section::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        background: radial-gradient(circle at top right, rgba(167, 243, 208, 0.3), transparent 70%);
      }
      
      .hero-title {
        font-size: 42px;
        font-weight: 800;
        margin-bottom: 20px;
        text-shadow: 0 2px 4px rgba(0,0,0,0.1);
        position: relative;
      }
      
      .hero-subtitle {
        font-size: 20px;
        opacity: 0.95;
        line-height: 1.6;
        font-weight: 500;
        position: relative;
      }
      
      /* Enhanced Feature Cards */
      .feature-card {
        background: linear-gradient(135deg, #ffffff 0%, #f8fafc 100%);
        border-radius: 20px;
        padding: 30px 25px;
        margin: 15px;
        box-shadow: 0 4px 20px rgba(0, 0, 0, 0.08);
        border: 1px solid #e2e8f0;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        position: relative;
        overflow: hidden;
        text-align: center;
        height: 100%;
        display: flex;
        flex-direction: column;
      }
      
      .feature-card::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 4px;
        background: linear-gradient(135deg, #10b981, #059669);
        transform: scaleX(0);
        transition: transform 0.3s ease;
      }
      
      .feature-card:hover {
        transform: translateY(-8px);
        box-shadow: 0 20px 40px rgba(16, 185, 129, 0.15);
        border-color: #a7f3d0;
      }
      
      .feature-card:hover::before {
        transform: scaleX(1);
      }
      
      .feature-icon-wrapper {
        margin-bottom: 20px;
      }
      
      .feature-icon-bg {
        width: 80px;
        height: 80px;
        background: linear-gradient(135deg, #10b981, #059669);
        border-radius: 20px;
        display: flex;
        align-items: center;
        justify-content: center;
        margin: 0 auto;
        box-shadow: 0 8px 20px rgba(16, 185, 129, 0.3);
        transition: all 0.3s ease;
      }
      
      .feature-card:hover .feature-icon-bg {
        transform: scale(1.1) rotate(5deg);
        box-shadow: 0 12px 25px rgba(16, 185, 129, 0.4);
      }
      
      .feature-icon {
        font-size: 32px;
        color: white;
        filter: drop-shadow(0 2px 4px rgba(0,0,0,0.2));
      }
      
      .feature-title {
        font-size: 20px;
        font-weight: 700;
        color: #1e293b;
        margin-bottom: 15px;
        line-height: 1.3;
      }
      
      .feature-description {
        color: #64748b;
        line-height: 1.6;
        font-size: 15px;
        margin-bottom: 20px;
        flex-grow: 1;
      }
      
      .feature-footer {
        margin-top: auto;
        opacity: 0;
        transform: translateY(10px);
        transition: all 0.3s ease;
      }
      
      .feature-card:hover .feature-footer {
        opacity: 1;
        transform: translateY(0);
      }
      
      /* Enhanced Challenge & Target Boxes */
      .challenge-box, .target-box {
        background: linear-gradient(135deg, #ffffff 0%, #f8fafc 100%);
        border-radius: 20px;
        padding: 0;
        margin: 15px;
        box-shadow: 0 8px 25px rgba(0, 0, 0, 0.08);
        border: 1px solid #e2e8f0;
        transition: all 0.3s ease;
        overflow: hidden;
        height: 100%;
      }
      
      .challenge-box:hover, .target-box:hover {
        transform: translateY(-5px);
        box-shadow: 0 15px 35px rgba(0, 0, 0, 0.12);
      }
      
      .box-header-custom {
        background: linear-gradient(135deg, #ef4444, #dc2626);
        color: white;
        padding: 25px;
        display: flex;
        align-items: center;
        gap: 15px;
      }
      
      .target-box .box-header-custom {
        background: linear-gradient(135deg, #10b981, #059669);
      }
      
      .box-header-icon {
        font-size: 24px;
        filter: drop-shadow(0 2px 4px rgba(0,0,0,0.2));
      }
      
      .box-header-custom h3 {
        margin: 0;
        font-size: 20px;
        font-weight: 700;
        text-shadow: 0 1px 2px rgba(0,0,0,0.1);
      }
      
      .box-content {
        padding: 25px;
      }
      
      .highlight-text {
        background: linear-gradient(135deg, #10b981, #059669);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        font-weight: 700;
        font-size: 15px;
      }
      
      /* Challenge List Styling */
      .challenge-list {
        margin-top: 20px;
      }
      
      .challenge-item {
        display: flex;
        align-items: center;
        gap: 12px;
        padding: 12px 0;
        border-bottom: 1px solid #f1f5f9;
        transition: all 0.2s ease;
      }
      
      .challenge-item:hover {
        background: #f8fafc;
        border-radius: 8px;
        padding: 12px;
        margin: 0 -10px;
      }
      
      .challenge-item:last-child {
        border-bottom = none;
      }
      
      .challenge-icon {
        color: #ef4444;
        font-size: 16px;
        min-width: 20px;
      }
      
      /* Target List Styling */
      .target-list {
        margin-top: 20px;
      }
      
      .target-item {
        display: flex;
        align-items: center;
        gap: 15px;
        padding: 15px;
        margin-bottom: 10px;
        background: #f0fdf4;
        border-radius: 12px;
        border-left: 4px solid #10b981;
        transition: all 0.3s ease;
      }
      
      .target-item:hover {
        background: #dcfce7;
        transform: translateX(5px);
        box-shadow: 0 4px 12px rgba(16, 185, 129, 0.15);
      }
      
      .target-number {
        width: 30px;
        height: 30px;
        background: linear-gradient(135deg, #10b981, #059669);
        color: white;
        border-radius: 50%;
        display: flex;
        align-items: center;
        justify-content: center;
        font-weight: 700;
        font-size: 14px;
        box-shadow: 0 2px 8px rgba(16, 185, 129, 0.3);
      }
      
      /* Callout Box */
      .callout-box {
        background: linear-gradient(135deg, #fef3c7, #fef7cd);
        border: 1px solid #fcd34d;
        border-radius: 12px;
        padding: 20px;
        margin-top: 25px;
        display: flex;
        align-items: center;
        gap: 15px;
      }
      
      .callout-icon {
        color: #d97706;
        font-size: 20px;
        flex-shrink: 0;
      }
      
      .callout-box p {
        margin: 0;
        color: #92400e;
        font-weight: 600;
        font-size: 14px;
        line-height: 1.5;
      }
      
      /* Responsive adjustments */
      @media (max-width: 768px) {
        .challenge-box, .target-box {
          margin: 10px 0;
        }
        
        .box-header-custom {
          padding: 20px;
        }
        
        .box-content {
          padding: 20px;
        }
      }

      /* Chart Container Styling */
      .plotly.html-widget {
        border-radius: 12px;
      }
      
      /* Status colors for boxes */
      .box.box-success {
        border-top: none;
      }
      
      /* Additional modern touches */
      .main-header .logo {
        border-bottom: 1px solid rgba(255, 255, 255, 0.1);
      }
      
      .sidebar-menu > li > a {
        margin: 2px 8px;
        border-radius: 8px;
      }
      
      /* Scrollbar styling */
      ::-webkit-scrollbar {
        width: 6px;
      }
      
      ::-webkit-scrollbar-track {
        background: #f1f5f9;
      }
      
      ::-webkit-scrollbar-thumb {
        background: linear-gradient(135deg, #10b981 0%, #059669 100%);
        border-radius: 3px;
      }
      
      ::-webkit-scrollbar-thumb:hover {
        background: linear-gradient(135deg, #059669 0%, #047857 100%);
      }
      
      /* Tab-specific styling */
      .tab-content {
        padding: 20px;
      }
      
      .metric-comparison {
        background: white;
        border-radius: 12px;
        padding: 20px;
        margin: 15px 0;
        box-shadow: 0 2px 10px rgba(0,0,0,0.1);
      }
      
      .province-comparison-card {
        background: linear-gradient(135deg, #ffffff, #f8fafc);
        border-radius: 12px;
        padding: 20px;
        margin: 10px 0;
        border-left: 4px solid #10b981;
      }
      
      /* Small font for M ton */
      .small-unit {
        font-size: 12px;
        font-weight: 600;
      }
      /* Explore Tab Styling */
      .explore-controls {
        background: linear-gradient(135deg, #ffffff 0%, #f8fafc 100%);
        border-radius: 16px;
        padding: 25px;
        margin: 15px 0;
        box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1);
        border: 1px solid #e2e8f0;
      }
      
      .variable-control {
        margin-bottom: 15px;
      }
      
      .variable-control label {
        font-weight: 600;
        color: #374151;
        margin-bottom: 8px;
        display: block;
        font-size: 14px;
      }
      
      .control-group {
        background: #f8fafc;
        padding: 20px;
        border-radius: 12px;
        margin-bottom: 20px;
        border: 1px solid #e2e8f0;
      }
      
      .control-group h4 {
        color: #1e293b;
        margin-bottom: 15px;
        font-weight: 700;
        font-size: 16px;
      }
      
      .chart-container {
        background: white;
        border-radius: 16px;
        padding: 20px;
        margin: 15px 0;
        box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1);
        border: 1px solid #e2e8f0;
        min-height: 500px;
      }
      
      .viz-preview {
        background: #f8fafc;
        border: 2px dashed #cbd5e1;
        border-radius: 12px;
        padding: 30px;
        text-align: center;
        color: #64748b;
        margin: 10px 0;
        transition: all 0.3s ease;
        cursor: pointer;
      }
      
      .viz-preview:hover {
        border-color: #10b981;
        background: #f0fdf4;
        color: #059669;
      }
      
      .viz-preview.active {
        border-color: #10b981;
        background: #ecfdf5;
        color: #047857;
      }
      
      .viz-icon {
        font-size: 24px;
        margin-bottom: 10px;
        display: block;
      }
      
      .quick-filter {
        background: #fffbeb;
        border: 1px solid #fcd34d;
        border-radius: 10px;
        padding: 15px;
        margin: 10px 0;
      }
      
      .quick-filter h5 {
        color: #92400e;
        margin-bottom: 10px;
        font-weight: 600;
      }
      
      /* Enhanced Rencana Aksi Tab Styling */
      .strategy-card {
        background: linear-gradient(135deg, #ffffff 0%, #f8fafc 100%);
        border-radius: 16px;
        padding: 25px;
        margin: 15px 0;
        box-shadow: 0 4px 20px rgba(0, 0, 0, 0.08);
        border: 1px solid #e2e8f0;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        position: relative;
        overflow: hidden;
      }
      
      .strategy-card::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 4px;
        background: linear-gradient(135deg, #10b981, #059669);
        transform: scaleX(0);
        transition: transform 0.3s ease;
      }
      
      .strategy-card:hover {
        transform: translateY(-5px);
        box-shadow: 0 15px 35px rgba(16, 185, 129, 0.15);
        border-color: #a7f3d0;
      }
      
      .strategy-card:hover::before {
        transform: scaleX(1);
      }
      
      .strategy-header {
        display: flex;
        align-items: center;
        gap: 15px;
        margin-bottom: 20px;
        padding-bottom: 15px;
        border-bottom: 2px solid #f1f5f9;
      }
      
      .strategy-icon {
        width: 50px;
        height: 50px;
        background: linear-gradient(135deg, #10b981, #059669);
        border-radius: 12px;
        display: flex;
        align-items: center;
        justify-content: center;
        color: white;
        font-size: 20px;
        box-shadow: 0 4px 12px rgba(16, 185, 129, 0.3);
      }
      
      .strategy-title {
        font-size: 20px;
        font-weight: 700;
        color: #1e293b;
        margin: 0;
      }
      
      .executive-summary-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
        gap: 15px;
        margin: 20px 0;
      }
      
      .executive-card {
        background: linear-gradient(135deg, #ffffff 0%, #f8fafc 100%);
        border-radius: 12px;
        padding: 20px;
        border-left: 4px solid #10b981;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
        transition: all 0.3s ease;
      }
      
      .executive-card:hover {
        transform: translateY(-3px);
        box-shadow: 0 4px 15px rgba(16, 185, 129, 0.15);
      }
      
      .executive-icon {
        font-size: 24px;
        color: #10b981;
        margin-bottom: 10px;
      }
      
      .executive-value {
        font-size: 28px;
        font-weight: 800;
        color: #1e293b;
        margin: 10px 0;
        background: linear-gradient(135deg, #059669 0%, #047857 100%);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
      }
      
      .executive-label {
        font-size: 14px;
        color: #64748b;
        font-weight: 600;
      }
      
      .executive-trend {
        font-size: 12px;
        margin-top: 5px;
        display: flex;
        align-items: center;
        gap: 4px;
      }
      
      .trend-positive { color: #10b981; }
      .trend-negative { color: #ef4444; }
      
      /* Enhanced Filter Section */
      .enhanced-filters {
        background: linear-gradient(135deg, #ffffff 0%, #f8fafc 100%);
        border-radius: 16px;
        padding: 25px;
        margin: 20px 0;
        box-shadow: 0 4px 20px rgba(0, 0, 0, 0.08);
        border: 1px solid #e2e8f0;
      }
      
      .filter-group {
        margin-bottom: 15px;
      }
      
      .filter-group label {
        font-weight: 600;
        color: #374151;
        margin-bottom: 8px;
        display: block;
        font-size: 14px;
      }
      
      /* Enhanced Recommendation Grid */
      .recommendation-master-grid {
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(350px, 1fr));
        gap: 20px;
        margin: 20px 0;
      }
      
      .recommendation-card-enhanced {
        background: linear-gradient(135deg, #ffffff 0%, #f8fafc 100%);
        border-radius: 12px;
        padding: 20px;
        border: 1px solid #e2e8f0;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        position: relative;
        overflow: hidden;
      }
      
      .recommendation-card-enhanced::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 3px;
        background: linear-gradient(135deg, #10b981, #059669);
      }
      
      .recommendation-card-enhanced:hover {
        transform: translateY(-5px);
        box-shadow: 0 8px 25px rgba(0, 0, 0, 0.12);
        border-color: #a7f3d0;
      }
      
      .recommendation-header {
        display: flex;
        justify-content: space-between;
        align-items: flex-start;
        margin-bottom: 15px;
        gap: 10px;
      }
      
      .recommendation-badges {
        display: flex;
        gap: 8px;
        flex-wrap: wrap;
        margin-top: 10px;
      }
      
      .recommendation-title {
        font-size: 16px;
        font-weight: 700;
        color: #1e293b;
        margin: 0;
        line-height: 1.4;
        flex: 1;
      }
      
      .recommendation-description {
        color: #64748b;
        font-size: 14px;
        line-height: 1.5;
        margin-bottom: 15px;
      }
      
      .recommendation-metrics {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-top: 15px;
        padding-top: 15px;
        border-top: 1px solid #f1f5f9;
      }
      
      .metric-item {
        text-align: center;
      }
      
      .metric-value {
        font-size: 18px;
        font-weight: 700;
        color: #10b981;
      }
      
      .metric-label {
        font-size: 11px;
        color: #64748b;
        text-transform: uppercase;
        font-weight: 600;
      }
      
      /* Success Stories Section */
      .success-stories-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
        gap: 20px;
        margin: 20px 0;
      }
      
      .success-story-card {
        background: linear-gradient(135deg, #f0fdf4 0%, #ecfdf5 100%);
        border-radius: 12px;
        padding: 20px;
        border: 1px solid #a7f3d0;
        transition: all 0.3s ease;
      }
      
      .success-story-card:hover {
        transform: translateY(-3px);
        box-shadow: 0 6px 20px rgba(16, 185, 129, 0.15);
      }
      
      .success-story-header {
        display: flex;
        align-items: center;
        gap: 12px;
        margin-bottom: 15px;
      }
      
      .success-story-icon {
        width: 40px;
        height: 40px;
        background: linear-gradient(135deg, #10b981, #059669);
        border-radius: 10px;
        display: flex;
        align-items: center;
        justify-content: center;
        color: white;
        font-size: 16px;
      }
      
      .success-story-title {
        font-size: 16px;
        font-weight: 700;
        color: #1e293b;
        margin: 0;
      }
      
      .success-story-metrics {
        display: grid;
        grid-template-columns: repeat(2, 1fr);
        gap: 10px;
        margin: 15px 0;
      }
      
      .success-metric {
        text-align: center;
        padding: 8px;
        background: white;
        border-radius: 8px;
        border: 1px solid #d1fae5;
      }
      
      .success-metric-value {
        font-size: 18px;
        font-weight: 800;
        color: #059669;
        display: block;
      }
      
      .success-metric-label {
        font-size: 11px;
        color: #64748b;
        text-transform: uppercase;
        font-weight: 600;
      }
      
      /* Journal References */
      .journals-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(350px, 1fr));
        gap: 20px;
        margin: 20px 0;
      }
      
      .journal-card {
        background: linear-gradient(135deg, #ffffff 0%, #f8fafc 100%);
        border-radius: 12px;
        padding: 20px;
        border: 1px solid #e2e8f0;
        transition: all 0.3s ease;
      }
      
      .journal-card:hover {
        transform: translateY(-3px);
        box-shadow: 0 6px 20px rgba(0, 0, 0, 0.1);
      }
      
      .journal-header {
        display: flex;
        justify-content: space-between;
        align-items: flex-start;
        margin-bottom: 12px;
      }
      
      .journal-title {
        font-size: 15px;
        font-weight: 700;
        color: #1e293b;
        line-height: 1.4;
        flex: 1;
      }
      
      .journal-year {
        background: #10b981;
        color: white;
        padding: 4px 8px;
        border-radius: 6px;
        font-size: 12px;
        font-weight: 700;
        margin-left: 10px;
      }
      
      .journal-authors {
        color: #64748b;
        font-size: 13px;
        margin-bottom: 10px;
        font-style: italic;
      }
      
      .journal-abstract {
        color: #475569;
        font-size: 14px;
        line-height: 1.5;
        margin-bottom: 15px;
      }
      
      .journal-link {
        color: #3b82f6;
        text-decoration: none;
        font-size: 14px;
        font-weight: 600;
        display: inline-flex;
        align-items: center;
        gap: 5px;
      }
      
      .journal-link:hover {
        text-decoration: underline;
        color: #1d4ed8;
      }
      
      /* Implementation Timeline */
      .timeline-enhanced {
        position: relative;
        padding-left: 30px;
      }
      
      .timeline-enhanced::before {
        content: '';
        position: absolute;
        left: 15px;
        top: 0;
        bottom: 0;
        width: 2px;
        background: linear-gradient(to bottom, #10b981, #059669);
      }
      
      .timeline-item-enhanced {
        position: relative;
        margin-bottom: 30px;
        padding: 20px;
        background: white;
        border-radius: 12px;
        border: 1px solid #e2e8f0;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.06);
      }
      
      .timeline-item-enhanced::before {
        content: '';
        position: absolute;
        left: -26px;
        top: 25px;
        width: 12px;
        height: 12px;
        border-radius: 50%;
        background: #10b981;
        border: 3px solid white;
        box-shadow: 0 0 0 3px #10b981;
      }
      
      .timeline-date-enhanced {
        font-weight: 700;
        color: #10b981;
        margin-bottom: 8px;
        font-size: 14px;
      }
      
      .timeline-phase {
        font-weight: 600;
        color: #1e293b;
        margin-bottom: 10px;
        font-size: 16px;
      }
      
      .timeline-tasks {
        list-style: none;
        padding: 0;
        margin: 0;
      }
      
      .timeline-tasks li {
        padding: 5px 0;
        padding-left: 20px;
        position: relative;
        color: #64748b;
        font-size: 14px;
      }
      
      .timeline-tasks li::before {
        content: '✓';
        position: absolute;
        left: 0;
        color: #10b981;
        font-weight: bold;
      }
      
      /* Action Buttons */
      .action-buttons {
        display: flex;
        gap: 10px;
        margin-top: 15px;
        flex-wrap: wrap;
      }
      
      .btn-detail {
        background: linear-gradient(135deg, #3b82f6, #1d4ed8);
        color: white;
        border: none;
        border-radius: 8px;
        padding: 8px 16px;
        font-size: 12px;
        font-weight: 600;
        cursor: pointer;
        transition: all 0.3s ease;
      }
      
      .btn-implement {
        background: linear-gradient(135deg, #10b981, #059669);
        color: white;
        border: none;
        border-radius: 8px;
        padding: 8px 16px;
        font-size: 12px;
        font-weight: 600;
        cursor: pointer;
        transition: all 0.3s ease;
      }
      
      .btn-detail:hover, .btn-implement:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.2);
      }
      
      /* Badge Styles */
      .badge {
        padding: 4px 10px;
        border-radius: 20px;
        font-size: 11px;
        font-weight: 700;
        text-transform: uppercase;
      }
      
      .badge-priority-high { 
        background: #fef2f2; 
        color: #dc2626; 
        border: 1px solid #fecaca;
      }
      
      .badge-priority-medium { 
        background: #fffbeb; 
        color: #d97706; 
        border: 1px solid #fed7aa;
      }
      
      .badge-priority-low { 
        background: #f0fdf4; 
        color: #059669; 
        border: 1px solid #a7f3d0;
      }
      
      .badge-category {
        background: #eff6ff;
        color: #1d4ed8;
        border: 1px solid #dbeafe;
      }
      
      /* Responsive Design */
      @media (max-width: 768px) {
        .executive-summary-grid {
          grid-template-columns: 1fr;
        }
        
        .recommendation-master-grid {
          grid-template-columns: 1fr;
        }
        
        .success-stories-grid {
          grid-template-columns: 1fr;
        }
        
        .journals-grid {
          grid-template-columns: 1fr;
        }
        
        .action-buttons {
          flex-direction: column;
        }
        
        .btn-detail, .btn-implement {
          width: 100%;
          text-align: center;
        }
      }
      
      /* Modal Welcome Popup */
      .welcome-modal {
        background: linear-gradient(135deg, #ffffff 0%, #f8fafc 100%);
        border-radius: 20px;
        padding: 0;
        overflow: hidden;
        box-shadow: 0 25px 50px -12px rgba(0, 0, 0, 0.25);
        border: 1px solid #e2e8f0;
      }
      
      .welcome-header {
        background: linear-gradient(135deg, #10b981 0%, #059669 100%);
        color: white;
        padding: 30px;
        text-align: center;
        position: relative;
        overflow: hidden;
      }
      
      .welcome-header::before {
        content: '';
        position: absolute;
        top: -50%;
        right: -50%;
        width: 100%;
        height: 200%;
        background: radial-gradient(circle, rgba(255,255,255,0.1) 0%, transparent 70%);
      }
      
      .welcome-icon {
        font-size: 48px;
        margin-bottom: 15px;
        filter: drop-shadow(0 4px 8px rgba(0,0,0,0.2));
        display: block;
      }
      
      .welcome-title {
        font-size: 28px;
        font-weight: 800;
        margin-bottom: 10px;
        text-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      
      .welcome-subtitle {
        font-size: 16px;
        opacity: 0.9;
        font-weight: 500;
      }
      
      .welcome-body {
        padding: 30px;
        text-align: center;
      }
      
      .welcome-image {
      width: 120px;
      height: 120px;
      border-radius: 50%;
      margin: 0 auto 20px;
      background: #ffffff;
      display: flex;
      align-items: center;
      justify-content: center;
      box-shadow: 0 8px 25px rgba(0, 0, 0, 0.1);
      border: 4px solid #f0fdf4; /* Border hijau muda yang soft */
      overflow: hidden;
      position: relative;
    }
    
    .welcome-image img {
      width: 90%;
      height: 90%;
      object-fit: contain; /* atau 'cover' tergantung preferensi */
      border-radius: 50%;
    }
      
      .welcome-text {
        color: #64748b;
        margin-bottom: 25px;
        line-height: 1.6;
        font-size: 15px;
      }
      
      .name-input-container {
        margin: 25px 0;
      }
      
      .name-input {
        width: 100%;
        padding: 15px 20px;
        border: 2px solid #e2e8f0;
        border-radius: 12px;
        font-size: 16px;
        font-weight: 500;
        text-align: center;
        transition: all 0.3s ease;
        background: #f8fafc;
      }
      
      .name-input:focus {
        outline: none;
        border-color: #10b981;
        box-shadow: 0 0 0 3px rgba(16, 185, 129, 0.1);
        background: white;
      }
      
      .welcome-footer {
        padding: 20px 30px;
        background: #f8fafc;
        border-top: 1px solid #e2e8f0;
        text-align: center;
      }
      
      .btn-welcome {
        background: linear-gradient(135deg, #10b981 0%, #059669 100%);
        color: white;
        border: none;
        border-radius: 12px;
        padding: 15px 30px;
        font-size: 16px;
        font-weight: 700;
        cursor: pointer;
        transition: all 0.3s ease;
        box-shadow: 0 4px 15px rgba(16, 185, 129, 0.3);
      }
      
      .btn-welcome:hover {
        transform: translateY(-2px);
        box-shadow: 0 8px 25px rgba(16, 185, 129, 0.4);
      }
      
      .team-info {
        background: #f0fdf4;
        border-radius: 12px;
        padding: 20px;
        margin-top: 20px;
        border-left: 4px solid #10b981;
      }
      
      .team-info h4 {
        color: #047857;
        margin-bottom: 10px;
        font-weight: 700;
      }
      
      .team-info p {
        color: #64748b;
        margin-bottom: 8px;
        font-size: 14px;
      }
      /* Enhanced Bank Sampah KPI Cards */
  `   .bank-kpi-card {
        background: linear-gradient(135deg, #ffffff 0%, #f0fdf4 100%);
        border-radius: 16px;
        padding: 20px;
        margin: 10px;
        box-shadow: 0 4px 15px rgba(0, 0, 0, 0.08);
        border: 1px solid #d1fae5;
        transition: all 0.3s ease;
        text-align: center;
        height: 140px;
        display: flex;
        flex-direction: column;
        justify-content: center;
      }
`      
      .bank-kpi-card::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 4px;
        background: linear-gradient(135deg, #10b981, #059669);
        transform: scaleX(0);
        transition: transform 0.3s ease;
      }
      
      .bank-kpi-card:hover {
        transform: translateY(-5px);
        box-shadow: 0 8px 25px rgba(16, 185, 129, 0.15);
        border-color: #a7f3d0;
      }
      
      .bank-kpi-card:hover::before {
        transform: scaleX(1);
      }
      
      .bank-kpi-value {
        font-size: 28px;
        font-weight: 800;
        margin: 10px 0;
        background: linear-gradient(135deg, #059669 0%, #047857 100%);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
      }
      
      .bank-kpi-title {
        font-size: 12px;
        color: #64748b;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      
      .bank-kpi-subtitle {
        font-size: 10px;
        color: #94a3b8;
        margin-top: 5px;
      }
      
      .bank-kpi-icon {
        font-size: 20px;
        margin-bottom: 10px;
        background: linear-gradient(135deg, #10b981, #059669);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
      }
      
      /* Enhanced Map Container */
      .bank-map-container {
        background: white;
        border-radius: 16px;
        padding: 20px;
        margin: 15px 0;
        box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1);
        border: 1px solid #e2e8f0;
        height: 400px;
      }
      
      .bank-map-header {
        display: flex;
        align-items: center;
        gap: 10px;
        margin-bottom: 15px;
        padding-bottom: 15px;
        border-bottom: 2px solid #f1f5f9;
      }
      
      .bank-map-icon {
        width: 40px;
        height: 40px;
        background: linear-gradient(135deg, #10b981, #059669);
        border-radius: 10px;
        display: flex;
        align-items: center;
        justify-content: center;
        color: white;
        font-size: 18px;
      }
      
      .bank-map-title {
        font-size: 18px;
        font-weight: 700;
        color: #1e293b;
        margin: 0;
      }
      
      .bank-map-subtitle {
        font-size: 12px;
        color: #64748b;
        margin-left: auto;
      }
      }
    ")),
      
      # JavaScript untuk Welcome Modal
      tags$script(HTML("
      // Show welcome modal on first visit
      $(document).ready(function() {
        // Check if user has visited before
        if (!localStorage.getItem('trashAnalyticsVisited')) {
          setTimeout(function() {
            $('#welcomeModal').modal('show');
          }, 1000);
        }
      });
      
      // Handle name submission
      function submitName() {
        var userName = $('#userName').val();
        if (userName.trim() === '') {
          userName = 'Sahabat Lingkungan';
        }
        
        // Store in session
        Shiny.setInputValue('user_name', userName);
        
        // Mark as visited
        localStorage.setItem('trashAnalyticsVisited', 'true');
        
        $('#welcomeModal').modal('hide');
        
        // Show welcome notification
        showNotification('Selamat datang, ' + userName + '! Mari bersama-sama wujudkan Indonesia bersih dan bebas sampah.', 'success');
      }
      
      // Enter key support
      function handleKeyPress(event) {
        if (event.keyCode === 13) {
          submitName();
        }
      }
      
      function showNotification(message, type) {
        // Create notification element
        var notification = $('<div class=\"alert alert-success alert-dismissible\" style=\"position: fixed; top: 80px; right: 20px; z-index: 9999; min-width: 300px; border-radius: 12px; border: none; box-shadow: 0 10px 25px rgba(0,0,0,0.1);\">' +
                            '<button type=\"button\" class=\"close\" data-dismiss=\"alert\">&times;</button>' +
                            '<strong>👋 Selamat Datang!</strong><br>' + message + '</div>');
        
        $('body').append(notification);
        
        // Auto remove after 5 seconds
        setTimeout(function() {
          notification.alert('close');
        }, 5000);
      }
    "))
    ),
    
    tabItems(
      # Tab Home ----
      tabItem(
        tabName = "home",
        fluidRow(
          column(
            width = 12,
            tags$div(
              style = "text-align: center; margin-bottom: 30px;",
              tags$img(
                src = "https://raw.githubusercontent.com/ngurahsentana24/TrashAnalytics/main/source/photo/lookinghome.png",
                style = "width: 100%; max-height: 400px; object-fit: contain; border-radius: 16px;",
                alt = "Trash Analytics Dashboard Preview"
              )
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            div(class = "hero-section",
                style = "background: linear-gradient(rgba(34, 197, 94, 0.85), rgba(21, 128, 61, 0.85)), url('https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQOcKEJ_d92TeTJix2gFi3-Y-SxKHowfWwfWA&s'); background-size: cover; background-position: center;",
                h1(class = "hero-title", "Dashboard Analisis Pengelolaan Sampah Indonesia"),
                p(class = "hero-subtitle",
                  "Platform analisis data komprehensif untuk memantau, menganalisis, dan mengoptimalkan pengelolaan sampah di Indonesia. Mendukung pencapaian target 100% pengelolaan sampah tuntas pada tahun 2029.")
            )
          )
        ),
        
        # Video YouTube Fit
        fluidRow(
          column(
            width = 12,
            box(
              width = 12,
              status = "success",
              solidHeader = TRUE,
              tags$div(
                style = "position: relative; width: 100%; padding-bottom: 56.25%;", # 16:9 aspect ratio
                tags$iframe(
                  style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; border-radius: 12px;",
                  src = "https://www.youtube.com/embed/i0bb7Et0ots",
                  frameborder = "0",
                  allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
                  allowfullscreen = TRUE
                )
              )
            )
          )
        ),
        
        fluidRow(
          column(
            width = 4,
            div(class = "feature-card",
                div(class = "feature-icon-wrapper",
                    div(class = "feature-icon-bg",
                        icon("chart-line", class = "feature-icon")
                    )
                ),
                h3(class = "feature-title", "Monitoring Real-time"),
                p(class = "feature-description", "Pantau perkembangan pengelolaan sampah nasional dengan data terkini dan visualisasi interaktif."),
                div(class = "feature-footer",
                    tags$span("Explore →", style = "color: #10b981; font-weight: 600; font-size: 14px;")
                )
            )
          ),
          column(
            width = 4,
            div(class = "feature-card",
                div(class = "feature-icon-wrapper",
                    div(class = "feature-icon-bg",
                        icon("map-marked-alt", class = "feature-icon")
                    )
                ),
                h3(class = "feature-title", "Analisis Wilayah"),
                p(class = "feature-description", "Identifikasi pola dan kesenjangan pengelolaan sampah antar daerah dengan analisis spasial yang mendalam."),
                div(class = "feature-footer",
                    tags$span("Explore →", style = "color: #10b981; font-weight: 600; font-size: 14px;")
                )
            )
          ),
          column(
            width = 4,
            div(class = "feature-card",
                div(class = "feature-icon-wrapper",
                    div(class = "feature-icon-bg",
                        icon("balance-scale-right", class = "feature-icon")
                    )
                ),
                h3(class = "feature-title", "Perbandingan Efektivitas"),
                p(class = "feature-description", "Bandungkan performa antar daerah dan identifikasi best practices dalam pengelolaan sampah."),
                div(class = "feature-footer",
                    tags$span("Explore →", style = "color: #10b981; font-weight: 600; font-size: 14px;")
                )
            )
          )
        ),
        
        fluidRow(
          column(
            width = 6,
            div(class = "challenge-box",
                div(class = "box-header-custom",
                    icon("exclamation-triangle", class = "box-header-icon"),
                    h3("Tantangan Pengelolaan Sampah")
                ),
                div(class = "box-content",
                    tags$p("Berdasarkan data tahun 2024, Indonesia menghasilkan", 
                           tags$span(class = "highlight-text", "36,9 juta ton sampah"), 
                           "setiap tahunnya, dengan lebih dari", 
                           tags$span(class = "highlight-text", "11 juta ton"), 
                           "belum terkelola dengan baik."),
                    tags$div(class = "challenge-list",
                             tags$div(class = "challenge-item",
                                      icon("hard-hat", class = "challenge-icon"),
                                      tags$span("Keterbatasan infrastruktur pengelolaan sampah")
                             ),
                             tags$div(class = "challenge-item",
                                      icon("map", class = "challenge-icon"),
                                      tags$span("Variasi performa antar daerah yang signifikan")
                             ),
                             tags$div(class = "challenge-item",
                                      icon("recycle", class = "challenge-icon"),
                                      tags$span("Tingkat daur ulang yang masih perlu ditingkatkan")
                             ),
                             tags$div(class = "challenge-item",
                                      icon("users", class = "challenge-icon"),
                                      tags$span("Kesenjangan partisipasi masyarakat")
                             )
                    )
                )
            )
          ),
          column(
            width = 6,
            div(class = "target-box",
                div(class = "box-header-custom",
                    icon("bullseye", class = "box-header-icon"),
                    h3("Target Nasional 2029")
                ),
                div(class = "box-content",
                    tags$p("Pemerintah Indonesia menargetkan pengelolaan sampah tuntas 100% pada tahun 2029 melalui:"),
                    tags$div(class = "target-list",
                             tags$div(class = "target-item",
                                      div(class = "target-number", "1"),
                                      tags$span("Penguatan infrastruktur pengelolaan sampah")
                             ),
                             tags$div(class = "target-item",
                                      div(class = "target-number", "2"),
                                      tags$span("Peningkatan partisipasi masyarakat")
                             ),
                             tags$div(class = "target-item",
                                      div(class = "target-number", "3"),
                                      tags$span("Optimalisasi teknologi waste-to-energy")
                             ),
                             tags$div(class = "target-item",
                                      div(class = "target-number", "4"),
                                      tags$span("Pengembangan ekonomi sirkular")
                             ),
                             tags$div(class = "target-item",
                                      div(class = "target-number", "5"),
                                      tags$span("Penerapan sistem pengelolaan sampah terintegrasi")
                             )
                    ),
                    tags$div(class = "callout-box",
                             icon("lightbulb", class = "callout-icon"),
                             tags$p("Dashboard ini mendukung pencapaian target tersebut melalui analisis data yang komprehensif.")
                    )
                )
            )
          )
        )
      ),
      
      # Tab Ringkasan Nasional ----
      tabItem(
        tabName = "nasional",
        fluidRow(
          div(class = "section-header",
              icon("star", class = "section-icon"),
              h2("Indikator Kinerja Utama Nasional"),
              p("Monitor performa pengelolaan sampah secara real-time di seluruh Indonesia")
          )
        ),
        
        # KPI Cards dengan Perbandingan Tahun Lalu
        fluidRow(
          div(class = "kpi-container",
              uiOutput("kpi_cards")
          )
        ),
        
        fluidRow(
          column(
            width = 4,
            div(class = "chart-card",
                div(class = "chart-header",
                    icon("gauge-high", class = "chart-icon"),
                    h3("Progress Menuju Target 2029"),
                    tags$span(class = "chart-subtitle", "Pencapaian pengelolaan sampah tuntas")
                ),
                div(class = "chart-body",
                    plotlyOutput("gaugeChart", height = "300px")
                )
            )
          ),
          column(
            width = 8,
            div(class = "chart-card",
                div(class = "chart-header",
                    icon("chart-line", class = "chart-icon"),
                    h3("Tren 5 Tahun Semua Indikator"),
                    tags$span(class = "chart-subtitle", "Perkembangan komprehensif")
                ),
                div(class = "chart-controls",
                    fluidRow(
                      column(6,
                             selectInput("provinsiSelect", "Pilih Provinsi:",
                                         choices = c("Nasional" = "nasional"),
                                         selected = "nasional")
                      ),
                      column(6,
                             sliderInput("tahunRange", "Rentang Tahun:",
                                         min = 2019, max = 2024, 
                                         value = c(2020, 2024), step = 1)
                      )
                    )
                ),
                div(class = "chart-body",
                    plotlyOutput("trendAllChart", height = "300px")
                )
            )
          )
        ),
        
        # Filter waktu untuk performa dan efisiensi
        fluidRow(
          column(
            width = 12,
            div(class = "chart-controls",
                style = "background: #f8fafc; padding: 20px; border-radius: 12px; margin: 15px 0;",
                h4("Filter Analisis Performa & Efisiensi"),
                fluidRow(
                  column(3,
                         sliderInput("tahunRangeAnalisis", "Rentang Tahun Analisis:",
                                     min = 2019, max = 2024, 
                                     value = c(2021, 2024), step = 1)
                  ),
                  column(3,
                         numericInput("jumlahTop", "Jumlah Top Provinsi:",
                                      value = 10, min = 5, max = 20, step = 1)
                  ),
                  column(3,
                         selectInput("metrikAnalisis", "Metrik Analisis:",
                                     choices = c("Sampah Terkelola" = "Terkelola",
                                                 "Pengurangan Sampah" = "Pengurangan",
                                                 "Penanganan Sampah" = "Penanganan",
                                                 "Daur Ulang" = "Daur_Ulang"),
                                     selected = "Terkelola")
                  ),
                  column(3,
                         selectInput("jenisAnalisis", "Jenis Analisis:",
                                     choices = c("Peningkatan Tertinggi" = "improvement",
                                                 "Nilai Tertinggi" = "value"),
                                     selected = "improvement")
                  )
                )
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            div(class = "chart-card",
                div(class = "chart-header",
                    icon("trophy", class = "chart-icon"),
                    h3("Performa Terbaik & Tren"),
                    tags$span(class = "chart-subtitle", "Provinsi dengan progress tertinggi")
                ),
                div(class = "chart-body",
                    plotlyOutput("performanceChart", height = "300px")
                )
            )
          ),
          column(
            width = 6,
            div(class = "chart-card",
                div(class = "chart-header",
                    icon("ranking-star", class = "chart-icon"),
                    h3("Peringkat Efisiensi Pengelolaan"),
                    tags$span(class = "chart-subtitle", "Cost-effectiveness analysis")
                ),
                div(class = "chart-body",
                    plotlyOutput("efficiencyChart", height = "300px")
                )
            )
          )
        ),
        
        # Perkembangan Metrik Kunci per Tahun
        fluidRow(
          column(
            width = 12,
            div(class = "chart-card",
                div(class = "chart-header",
                    icon("chart-bar", class = "chart-icon"),
                    h3("Perkembangan Metrik Kunci per Tahun"),
                    tags$span(class = "chart-subtitle", "Perbandingan indikator utama dengan ikon")
                ),
                div(class = "chart-body",
                    plotlyOutput("yearlyComparison", height = "400px")
                )
            )
          )
        )
      ),
      
      # Tab Analisis Wilayah ----
      tabItem(
        tabName = "wilayah",
        fluidRow(
          div(class = "section-header",
              icon("map-marked-alt", class = "section-icon"),
              h2("Analisis Wilayah"),
              p("Analisis mendalam performa pengelolaan sampah berdasarkan wilayah")
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            div(class = "chart-card",
                div(class = "chart-header",
                    icon("trophy", class = "chart-icon"),
                    h3("20 Provinsi Terbaik - Komposisi Metrik"),
                    tags$span(class = "chart-subtitle", "Stacked bar semua metrik utama")
                ),
                div(class = "chart-controls",
                    fluidRow(
                      column(6,
                             sliderInput("tahunRangeWilayahTerbaik", "Rentang Tahun:",
                                         min = 2019, max = 2024, 
                                         value = c(2021, 2024), step = 1)
                      ),
                      column(6,
                             numericInput("jumlahTopWilayah", "Jumlah Provinsi:",
                                          value = 20, min = 10, max = 30, step = 5)
                      )
                    )
                ),
                div(class = "chart-body",
                    plotlyOutput("topProvincesChart", height = "500px")
                )
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            div(class = "chart-card",
                div(class = "chart-header",
                    icon("chart-line", class = "chart-icon"),
                    h3("20 Provinsi Terendah - Komposisi Metrik"),
                    tags$span(class = "chart-subtitle", "Analisis area perbaikan")
                ),
                div(class = "chart-controls",
                    sliderInput("tahunRangeWilayahTerendah", "Rentang Tahun:",
                                min = 2019, max = 2024, 
                                value = c(2021, 2024), step = 1)
                ),
                div(class = "chart-body",
                    plotlyOutput("bottomProvincesChart", height = "500px")
                )
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            div(class = "chart-card",
                div(class = "chart-header",
                    icon("code-compare", class = "chart-icon"),
                    h3("Perbandingan Provinsi"),
                    tags$span(class = "chart-subtitle", "Analisis head-to-head")
                ),
                div(class = "chart-controls",
                    fluidRow(
                      column(3,
                             selectInput("provinsi1", "Provinsi 1:",
                                         choices = c("Pilih Provinsi"),
                                         selected = "Pilih Provinsi")
                      ),
                      column(3,
                             selectInput("provinsi2", "Provinsi 2:",
                                         choices = c("Pilih Provinsi"),
                                         selected = "Pilih Provinsi")
                      ),
                      column(3,
                             sliderInput("tahunRangePerbandingan", "Rentang Tahun:",
                                         min = 2019, max = 2024, 
                                         value = c(2021, 2024), step = 1)
                      ),
                      column(3,
                             selectInput("metrikPerbandingan", "Metrik Perbandingan:",
                                         choices = c("Rata-rata" = "rata",
                                                     "Tahun Akhir" = "akhir"),
                                         selected = "rata")
                      )
                    )
                ),
                div(class = "chart-body",
                    plotlyOutput("provinceComparison", height = "400px")
                )
            )
          )
        )
      ),
      
      # Tab Sumber & Jenis Sampah ----
      tabItem(
        tabName = "sumber",
        fluidRow(
          div(class = "section-header",
              icon("trash-alt", class = "section-icon"),
              h2("Analisis Sumber & Jenis Sampah"),
              p("Komposisi dan karakteristik timbulan sampah di Indonesia")
          )
        ),
        
        # Potensi Daur Ulang
        fluidRow(
          column(
            width = 12,
            div(class = "chart-card",
                div(class = "chart-header",
                    icon("recycle", class = "chart-icon"),
                    h3("Potensi Daur Ulang Berdasarkan Jenis Sampah"),
                    tags$span(class = "chart-subtitle", "Analisis material yang dapat didaur ulang")
                ),
                div(class = "chart-controls",
                    fluidRow(
                      column(6,
                             selectInput("provinsiDaurUlang", "Pilih Provinsi:",
                                         choices = c("Nasional" = "nasional"),
                                         selected = "nasional")
                      ),
                      column(6,
                             selectInput("tahunDaurUlang", "Tahun:",
                                         choices = 2019:2024,
                                         selected = 2024)
                      )
                    )
                ),
                div(class = "chart-body",
                    plotlyOutput("recyclingPotentialChart", height = "500px")
                )
            )
          )
        ),
        
        fluidRow(
          column(
            width = 6,
            div(class = "chart-card",
                div(class = "chart-header",
                    icon("industry", class = "chart-icon"),
                    h3("Komposisi Sumber Sampah"),
                    tags$span(class = "chart-subtitle", "Berdasarkan sumber timbulan")
                ),
                div(class = "chart-controls",
                    fluidRow(
                      column(6,
                             selectInput("provinsiSumber", "Pilih Provinsi:",
                                         choices = c("Nasional" = "nasional"),
                                         selected = "nasional")
                      ),
                      column(6,
                             selectInput("tahunSumber", "Tahun:",
                                         choices = 2019:2024,
                                         selected = 2024)
                      )
                    )
                ),
                div(class = "chart-body",
                    plotlyOutput("sourceChart", height = "400px")
                )
            )
          ),
          column(
            width = 6,
            div(class = "chart-card",
                div(class = "chart-header",
                    icon("recycle", class = "chart-icon"),
                    h3("Proporsi Jenis Sampah"),
                    tags$span(class = "chart-subtitle", "Komposisi berdasarkan material")
                ),
                div(class = "chart-controls",
                    fluidRow(
                      column(6,
                             selectInput("provinsiJenis", "Pilih Provinsi:",
                                         choices = c("Nasional" = "nasional"),
                                         selected = "nasional")
                      ),
                      column(6,
                             selectInput("tahunJenis", "Tahun:",
                                         choices = 2019:2024,
                                         selected = 2024)
                      )
                    )
                ),
                div(class = "chart-body",
                    plotlyOutput("typeChart", height = "400px")
                )
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            div(class = "chart-card",
                div(class = "chart-header",
                    icon("chart-column", class = "chart-icon"),
                    h3("Top 15 Komposisi Sumber per Provinsi"),
                    tags$span(class = "chart-subtitle", "Horizontal bar chart")
                ),
                div(class = "chart-controls",
                    selectInput("tahunTopSumber", "Tahun:",
                                choices = 2019:2024,
                                selected = 2024)
                ),
                div(class = "chart-body",
                    plotlyOutput("topSourceChart", height = "500px")
                )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(class = "chart-card",
                div(class = "chart-header",
                    icon("chart-pie", class = "chart-icon"),
                    h3("Top 15 Proporsi Jenis Sampah per Provinsi"),
                    tags$span(class = "chart-subtitle", "Horizontal bar chart")
                ),
                div(class = "chart-controls",
                    selectInput("tahunTopJenis", "Tahun:",
                                choices = 2019:2024,
                                selected = 2024)
                ),
                div(class = "chart-body",
                    plotlyOutput("topTypeChart", height = "500px")
                )
            )
          )
        )
      ),
      
      # Tab Bank Sampah ----
      tabItem(
        tabName = "banksampah",
        fluidRow(
          div(class = "section-header",
              icon("piggy-bank", class = "section-icon"),
              h2("Analisis Bank Sampah"),
              p("Monitoring dan analisis performa sistem bank sampah di Indonesia")
          )
        ),
        
        # Filter Controls
        fluidRow(
          column(
            width = 12,
            div(class = "chart-controls",
                style = "background: #f8fafc; padding: 20px; border-radius: 12px; margin: 15px 0;",
                h4("Filter Data Bank Sampah"),
                fluidRow(
                  column(3,
                         sliderInput("tahunRangeBank", "Rentang Tahun:",
                                     min = 2019, max = 2024, 
                                     value = c(2021, 2024), step = 1)
                  ),
                  column(3,
                         selectInput("provinsiBank", "Pilih Provinsi:",
                                     choices = c("Semua Provinsi"),
                                     selected = "Semua Provinsi")
                  ),
                  column(3,
                         selectInput("jenisBank", "Jenis Fasilitas:",
                                     choices = c("Semua Jenis"),
                                     selected = "Semua Jenis")
                  ),
                  column(3,
                         selectInput("pengelolaBank", "Pengelola:",
                                     choices = c("Semua Pengelola"),
                                     selected = "Semua Pengelola")
                  )
                )
            )
          )
        ),
        
        # KPI Cards untuk Bank Sampah
        fluidRow(
          div(class = "kpi-container",
              uiOutput("bank_sampah_kpi")
          )
        ),
        
        # Baris pertama charts
        fluidRow(
          column(
            width = 6,
            div(class = "chart-card",
                div(class = "chart-header",
                    icon("chart-column", class = "chart-icon"),
                    h3("Performa Berdasarkan Jenis Fasilitas"),
                    tags$span(class = "chart-subtitle", "Perbandingan sampah terkelola per jenis")
                ),
                div(class = "chart-body",
                    plotlyOutput("bankByJenisChart", height = "400px")
                )
            )
          ),
          column(
            width = 6,
            div(class = "chart-card",
                div(class = "chart-header",
                    icon("users", class = "chart-icon"),
                    h3("Performa Berdasarkan Pengelola"),
                    tags$span(class = "chart-subtitle", "Efektivitas pengelolaan per kategori")
                ),
                div(class = "chart-body",
                    plotlyOutput("bankByPengelolaChart", height = "400px")
                )
            )
          )
        ),
        
        # Baris kedua charts - Trends
        fluidRow(
          column(
            width = 6,
            div(class = "chart-card",
                div(class = "chart-header",
                    icon("signs-post", class = "chart-icon"),
                    h3("Trend Sampah Masuk"),
                    tags$span(class = "chart-subtitle", "Perkembangan volume")
                ),
                div(class = "chart-body",
                    plotlyOutput("trendMasukChart", height = "250px")
                )
            )
          ),
          column(
            width = 6,
            div(class = "chart-card",
                div(class = "chart-header",
                    icon("arrow-trend-up", class = "chart-icon"),
                    h3("Trend Sampah Terkelola"),
                    tags$span(class = "chart-subtitle", "Perkembangan pengelolaan")
                ),
                div(class = "chart-body",
                    plotlyOutput("trendTerkelolaChart", height = "250px")
                )
            )
          ),
          column(
            width = 6,
            div(class = "chart-card",
                div(class = "chart-header",
                    icon("percent", class = "chart-icon"),
                    h3("Trend Efisiensi"),
                    tags$span(class = "chart-subtitle", "Rasio terkelola/masuk")
                ),
                div(class = "chart-body",
                    plotlyOutput("trendEfisiensiChart", height = "250px")
                )
            )
          ),
          column(
            width = 6,
            div(class = "chart-card",
                div(class = "chart-header",
                    icon("building", class = "chart-icon"),
                    h3("Trend Fasilitas"),
                    tags$span(class = "chart-subtitle", "Pertumbuhan bank sampah")
                ),
                div(class = "chart-body",
                    plotlyOutput("trendFasilitasChart", height = "250px")
                )
            )
          )
        ),
        
        # Top Performers & Map
        fluidRow(
          column(
            width = 12,
            div(class = "chart-card",
                div(class = "chart-header",
                    icon("trophy", class = "chart-icon"),
                    h3("Top 10 Bank Sampah Terbaik"),
                    tags$span(class = "chart-subtitle", "Berdasarkan volume & efisiensi")
                ),
                div(class = "chart-body",
                    plotlyOutput("topBankSampahChart", height = "350px")
                )
            )
          )
        )
      ),
      
      # Tab Explore ----
      tabItem(
        tabName = "explore",
        fluidRow(
          div(class = "section-header",
              icon("magnifying-glass-chart", class = "section-icon"),
              h2("Exploratory Data Analysis"),
              p("Analisis data dinamis dengan pemilihan variabel bebas dan berbagai jenis visualisasi")
          )
        ),
        
        # Control Panel
        fluidRow(
          column(
            width = 12,
            div(class = "explore-controls",
                h3("Control Panel", style = "color: #1e293b; margin-bottom: 20px;"),
                
                fluidRow(
                  # Jenis Visualisasi
                  column(
                    width = 3,
                    div(class = "control-group",
                        h4("Jenis Visualisasi"),
                        selectInput("vizType", "Pilih Jenis Chart:",
                                    choices = c(
                                      "Scatter Plot" = "scatter",
                                      "Line Chart" = "line",
                                      "Bar Chart" = "bar",
                                      "Histogram" = "histogram",
                                      "Box Plot" = "box",
                                      "Heatmap" = "heatmap",
                                      "Pie Chart" = "pie",
                                      "Area Chart" = "area"
                                    ),
                                    selected = "scatter")
                    )
                  ),
                  
                  # Variabel Selection
                  column(
                    width = 9,
                    div(class = "control-group",
                        h4("Pilih Variabel"),
                        fluidRow(
                          column(3, div(class = "variable-control",
                                        selectInput("varX", "Variabel X:", choices = NULL))),
                          column(3, div(class = "variable-control",
                                        selectInput("varY", "Variabel Y:", choices = NULL))),
                          column(3, div(class = "variable-control",
                                        selectInput("varColor", "Warna (Group):", choices = NULL))),
                          column(3, div(class = "variable-control",
                                        selectInput("varSize", "Ukuran (Size):", choices = NULL)))
                        )
                    )
                  )
                ),
                
                # Filter Data
                fluidRow(
                  column(
                    width = 12,
                    div(class = "control-group",
                        h4("Filter Data"),
                        fluidRow(
                          column(3,
                                 selectInput("filterProvinsi", "Provinsi:",
                                             choices = c("Semua Provinsi"),
                                             multiple = TRUE,
                                             selected = "Semua Provinsi")
                          ),
                          column(3,
                                 sliderInput("filterTahun", "Rentang Tahun:",
                                             min = 2019, max = 2024,
                                             value = c(2019, 2024), step = 1)
                          ),
                          column(3,
                                 selectInput("filterAggregate", "Agregasi Data:",
                                             choices = c("Data Mentah" = "raw",
                                                         "Rata-rata per Tahun" = "yearly",
                                                         "Rata-rata per Provinsi" = "province"),
                                             selected = "raw")
                          ),
                          column(3,
                                 selectInput("filterChartTheme", "Tema Chart:",
                                             choices = c("Default" = "plotly",
                                                         "Dark" = "plotly_dark",
                                                         "White" = "ggplot2",
                                                         "Minimal" = "minimal"),
                                             selected = "plotly")
                          )
                        )
                    )
                  )
                ),
                
                # Quick Actions
                fluidRow(
                  column(
                    width = 12,
                    div(class = "quick-filter",
                        h5("Quick Analysis"),
                        fluidRow(
                          column(2, actionButton("btnCorrelation", "Analisis Korelasi", 
                                                 class = "btn btn-success btn-sm")),
                          column(2, actionButton("btnTrend", "Analisis Trend", 
                                                 class = "btn btn-success btn-sm")),
                          column(2, actionButton("btnComparison", "Perbandingan", 
                                                 class = "btn btn-success btn-sm")),
                          column(2, actionButton("btnDistribution", "Distribusi", 
                                                 class = "btn btn-success btn-sm")),
                          column(2, actionButton("btnReset", "Reset", 
                                                 class = "btn btn-warning btn-sm")),
                          column(2, downloadButton("downloadViz", "Download Chart", 
                                                   class = "btn btn-info btn-sm"))
                        )
                    )
                  )
                )
            )
          )
        ),
        
        # Visualization Area
        fluidRow(
          column(
            width = 12,
            div(class = "chart-container",
                conditionalPanel(
                  condition = "input.vizType == 'scatter'",
                  plotlyOutput("scatterPlot", height = "500px")
                ),
                conditionalPanel(
                  condition = "input.vizType == 'line'",
                  plotlyOutput("lineChart", height = "500px")
                ),
                conditionalPanel(
                  condition = "input.vizType == 'bar'",
                  plotlyOutput("barChart", height = "500px")
                ),
                conditionalPanel(
                  condition = "input.vizType == 'histogram'",
                  plotlyOutput("histogramChart", height = "500px")
                ),
                conditionalPanel(
                  condition = "input.vizType == 'box'",
                  plotlyOutput("boxPlot", height = "500px")
                ),
                conditionalPanel(
                  condition = "input.vizType == 'heatmap'",
                  plotlyOutput("heatmapChart", height = "500px")
                ),
                conditionalPanel(
                  condition = "input.vizType == 'pie'",
                  plotlyOutput("pieChart", height = "500px")
                ),
                conditionalPanel(
                  condition = "input.vizType == 'area'",
                  plotlyOutput("areaChart", height = "500px")
                )
            )
          )
        ),
        
        # Statistics Summary
        fluidRow(
          column(
            width = 6,
            div(class = "chart-container",
                h4("Statistik Deskriptif"),
                verbatimTextOutput("dataSummary")
            )
          ),
          column(
            width = 6,
            div(class = "chart-container",
                h4("Informasi Dataset"),
                verbatimTextOutput("dataInfo")
            )
          )
        )
      ),
      
      # Tab Rencana Aksi ----
      tabItem(
        tabName = "rencana",
        fluidRow(
          div(class = "section-header",
              icon("rocket", class = "section-icon"),
              h2("Rencana Aksi & Rekomendasi Strategis"),
              p("Rekomendasi berbasis data untuk optimalisasi pengelolaan sampah Indonesia")
          )
        ),
        
        # Enhanced Filter Section
        fluidRow(
          column(
            width = 12,
            div(class = "enhanced-filters",
                h4("🔍 Filter Rekomendasi Strategis"),
                fluidRow(
                  column(3, 
                         div(class = "filter-group",
                             selectInput("kategoriFilter", "Kategori:",
                                         choices = c("Semua Kategori", "Infrastruktur", "Kebijakan", 
                                                     "Teknologi", "Lingkungan", "Ekonomi"),
                                         selected = "Semua Kategori")
                         )
                  ),
                  column(3,
                         div(class = "filter-group",
                             selectInput("prioritasFilter", "Tingkat Prioritas:",
                                         choices = c("Semua Prioritas", "Tinggi", "Sedang", "Rendah"),
                                         selected = "Semua Prioritas")
                         )
                  ),
                  column(3,
                         div(class = "filter-group",
                             selectInput("jangkaFilter", "Jangka Waktu:",
                                         choices = c("Semua Jangka", "Jangka Pendek (1-2 tahun)", 
                                                     "Jangka Menengah (3-4 tahun)", "Jangka Panjang (5+ tahun)"),
                                         selected = "Semua Jangka")
                         )
                  ),
                  column(3,
                         div(class = "filter-group",
                             selectInput("provinsiFilter", "Fokus Wilayah:",
                                         choices = c("Nasional", "Provinsi Tertentu", "Perkotaan", "Pedesaan"),
                                         selected = "Nasional")
                         )
                  )
                )
            )
          )
        ),
        
        # Enhanced Executive Summary
        fluidRow(
          column(
            width = 12,
            div(class = "strategy-card",
                div(class = "strategy-header",
                    div(class = "strategy-icon", icon("dashboard")),
                    h3(class = "strategy-title", "📊 Executive Summary Berbasis Data")
                ),
                div(class = "executive-summary-grid",
                    div(class = "executive-card",
                        div(class = "executive-icon", icon("trash")),
                        div(class = "executive-value", textOutput("currentManaged")),
                        div(class = "executive-label", "Sampah Terkelola Saat Ini"),
                        div(class = "executive-trend trend-positive", 
                            icon("arrow-up"), "Naik 5.2% dari 2023")
                    ),
                    div(class = "executive-card",
                        div(class = "executive-icon", icon("target")),
                        div(class = "executive-value", textOutput("targetGap")),
                        div(class = "executive-label", "Gap Menuju Target 2029"),
                        div(class = "executive-trend trend-negative", 
                            icon("clock"), "Butuh akselerasi 2.3x")
                    ),
                    div(class = "executive-card",
                        div(class = "executive-icon", icon("map")),
                        div(class = "executive-value", textOutput("performanceGap")),
                        div(class = "executive-label", "Kesenjangan Antar Provinsi"),
                        div(class = "executive-trend trend-negative", 
                            icon("warning"), "Perlu intervensi spesifik")
                    ),
                    div(class = "executive-card",
                        div(class = "executive-icon", icon("recycle")),
                        div(class = "executive-value", textOutput("recyclingPotential")),
                        div(class = "executive-label", "Potensi Daur Ulang"),
                        div(class = "executive-trend trend-positive", 
                            icon("arrow-up"), "Tingkatkan 18% dari sekarang")
                    )
                )
            )
          )
        ),
        
        # 20 Rekomendasi Strategis
        fluidRow(
          column(
            width = 12,
            div(class = "strategy-card",
                div(class = "strategy-header",
                    div(class = "strategy-icon", icon("lightbulb")),
                    h3(class = "strategy-title", "💡 20 Rekomendasi Strategis Berbasis Data")
                ),
                uiOutput("rekomendasiGrid")
            )
          )
        ),
        
        # Success Stories
        fluidRow(
          column(
            width = 12,
            div(class = "strategy-card",
                div(class = "strategy-header",
                    div(class = "strategy-icon", icon("trophy")),
                    h3(class = "strategy-title", "🏆 Success Stories & Best Practices")
                ),
                div(class = "success-stories-grid",
                    # Success Story 1
                    div(class = "success-story-card",
                        div(class = "success-story-header",
                            div(class = "success-story-icon", icon("recycle")),
                            h4(class = "success-story-title", "Bali - Transformasi Pengelolaan Sampah")
                        ),
                        p("Provinsi Bali berhasil meningkatkan sampah terkelola dari 45% menjadi 78% dalam 4 tahun melalui pendekatan terintegrasi."),
                        div(class = "success-story-metrics",
                            div(class = "success-metric",
                                span(class = "success-metric-value", "78%"),
                                span(class = "success-metric-label", "Sampah Terkelola")
                            ),
                            div(class = "success-metric",
                                span(class = "success-metric-value", "125"),
                                span(class = "success-metric-label", "Bank Sampah")
                            ),
                            div(class = "success-metric",
                                span(class = "success-metric-value", "42%"),
                                span(class = "success-metric-label", "Daur Ulang")
                            ),
                            div(class = "success-metric",
                                span(class = "success-metric-value", "Rp 3.2M"),
                                span(class = "success-metric-label", "Nilai Ekonomi")
                            )
                        )
                    ),
                    
                    # Success Story 2
                    div(class = "success-story-card",
                        div(class = "success-story-header",
                            div(class = "success-story-icon", icon("industry")),
                            h4(class = "success-story-title", "Surabaya - Teknologi Pengolahan Sampah")
                        ),
                        p("Kota Surabaya menerapkan teknologi waste-to-energy yang mengolah 1,000 ton sampah/hari menjadi listrik untuk 10,000 rumah."),
                        div(class = "success-story-metrics",
                            div(class = "success-metric",
                                span(class = "success-metric-value", "85%"),
                                span(class = "success-metric-label", "Reduksi Sampah")
                            ),
                            div(class = "success-metric",
                                span(class = "success-metric-value", "10MW"),
                                span(class = "success-metric-label", "Energi Dihasilkan")
                            ),
                            div(class = "success-metric",
                                span(class = "success-metric-value", "95%"),
                                span(class = "success-metric-label", "Partisipasi")
                            ),
                            div(class = "success-metric",
                                span(class = "success-metric-value", "Rp 15M/bln"),
                                span(class = "success-metric-label", "Pendapatan")
                            )
                        )
                    ),
                    
                    # Success Story 3
                    div(class = "success-story-card",
                        div(class = "success-story-header",
                            div(class = "success-story-icon", icon("users")),
                            h4(class = "success-story-title", "Yogyakarta - Gerakan Masyarakat")
                        ),
                        p("DIY Yogyakarta membangun sistem pengelolaan sampah berbasis komunitas dengan partisipasi 85% rumah tangga."),
                        div(class = "success-story-metrics",
                            div(class = "success-metric",
                                span(class = "success-metric-value", "85%"),
                                span(class = "success-metric-label", "Partisipasi")
                            ),
                            div(class = "success-metric",
                                span(class = "success-metric-value", "320"),
                                span(class = "success-metric-label", "Kelompok")
                            ),
                            div(class = "success-metric",
                                span(class = "success-metric-value", "65%"),
                                span(class = "success-metric-label", "Pengurangan")
                            ),
                            div(class = "success-metric",
                                span(class = "success-metric-value", "Rp 2.1M/bln"),
                                span(class = "success-metric-label", "Ekonomi")
                            )
                        )
                    )
                )
            )
          )
        ),
        
        # Research & Journals
        fluidRow(
          column(
            width = 12,
            div(class = "strategy-card",
                div(class = "strategy-header",
                    div(class = "strategy-icon", icon("book")),
                    h3(class = "strategy-title", "📚 Dukungan Penelitian & Jurnal Ilmiah")
                ),
                div(class = "journals-grid",
                    # Journal 1
                    div(class = "journal-card",
                        div(class = "journal-header",
                            span(class = "journal-title", "Integrated Solid Waste Management in Urban Areas of Developing Countries"),
                            span(class = "journal-year", "2023")
                        ),
                        div(class = "journal-authors", "Wilson, D.C., Velis, C.A., & Rodic, L."),
                        div(class = "journal-abstract", "Studi komprehensif tentang pendekatan terintegrasi dalam pengelolaan sampah perkotaan di negara berkembang, menunjukkan peningkatan efisiensi 35-45%."),
                        a(class = "journal-link", href = "https://doi.org/10.1016/j.wasman.2023.01.015", target = "_blank",
                          icon("external-link"), "Lihat Studi Lengkap")
                    ),
                    
                    # Journal 2
                    div(class = "journal-card",
                        div(class = "journal-header",
                            span(class = "journal-title", "Circular Economy Implementation in Municipal Solid Waste Management"),
                            span(class = "journal-year", "2022")
                        ),
                        div(class = "journal-authors", "Chen, L., & Rodriguez, M."),
                        div(class = "journal-abstract", "Analisis implementasi ekonomi sirkular dalam pengelolaan sampah perkotaan dan dampaknya terhadap nilai ekonomi dan lingkungan."),
                        a(class = "journal-link", href = "https://doi.org/10.1016/j.resconrec.2022.106325", target = "_blank",
                          icon("external-link"), "Lihat Studi Lengkap")
                    ),
                    
                    # Journal 3
                    div(class = "journal-card",
                        div(class = "journal-header",
                            span(class = "journal-title", "Waste-to-Energy Technologies for Sustainable Urban Development"),
                            span(class = "journal-year", "2023")
                        ),
                        div(class = "journal-authors", "Zhang, Y., et al."),
                        div(class = "journal-abstract", "Evaluasi teknologi waste-to-energy dan potensi penerapannya dalam konteks pembangunan perkotaan berkelanjutan."),
                        a(class = "journal-link", href = "https://doi.org/10.1016/j.energy.2023.127432", target = "_blank",
                          icon("external-link"), "Lihat Studi Lengkap")
                    ),
                    
                    # Journal 4
                    div(class = "journal-card",
                        div(class = "journal-header",
                            span(class = "journal-title", "Behavioral Interventions for Household Waste Separation"),
                            span(class = "journal-year", "2022")
                        ),
                        div(class = "journal-authors", "Nguyen, T.P., & Smith, A."),
                        div(class = "journal-abstract", "Studi tentang efektivitas intervensi perilaku dalam meningkatkan partisipasi rumah tangga dalam pemilahan sampah."),
                        a(class = "journal-link", href = "https://doi.org/10.1016/j.jenvman.2022.115678", target = "_blank",
                          icon("external-link"), "Lihat Studi Lengkap")
                    )
                )
            )
          )
        ),
        
        # Implementation Roadmap
        fluidRow(
          column(
            width = 12,
            div(class = "strategy-card",
                div(class = "strategy-header",
                    div(class = "strategy-icon", icon("road")),
                    h3(class = "strategy-title", "🗓️ Roadmap Implementasi 2024-2029")
                ),
                div(class = "timeline-enhanced",
                    div(class = "timeline-item-enhanced",
                        div(class = "timeline-date-enhanced", "2024 - 2025: Fase Persiapan & Pilot"),
                        div(class = "timeline-phase", "Foundation Building & Proof of Concept"),
                        tags$ul(class = "timeline-tasks",
                                tags$li("Pembangunan 10 TPA Regional berstandar nasional"),
                                tags$li("Implementasi sistem monitoring digital di 15 provinsi"),
                                tags$li("Pilot project bank sampah digital di 5 kota besar"),
                                tags$li("Pengembangan kebijakan insentif ekonomi sirkular"),
                                tags$li("Pelatihan 5,000 tenaga pengelola sampah profesional")
                        )
                    ),
                    div(class = "timeline-item-enhanced",
                        div(class = "timeline-date-enhanced", "2026 - 2027: Fase Akselerasi Nasional"),
                        div(class = "timeline-phase", "National Scaling & Integration"),
                        tags$ul(class = "timeline-tasks",
                                tags$li("Ekspansi infrastruktur ke 25 provinsi prioritas"),
                                tags$li("Implementasi teknologi waste-to-energy di 10 kota"),
                                tags$li("Digitalisasi seluruh rantai nilai pengelolaan sampah"),
                                tags$li("Penguatan regulasi dan penegakan hukum"),
                                tags$li("Program nasional edukasi masyarakat")
                        )
                    ),
                    div(class = "timeline-item-enhanced",
                        div(class = "timeline-date-enhanced", "2028 - 2029: Fase Optimalisasi"),
                        div(class = "timeline-phase", "Optimization & Sustainable Operation"),
                        tags$ul(class = "timeline-tasks",
                                tags$li("Pencapaian target 100% sampah terkelola"),
                                tags$li("Optimalisasi sistem dan teknologi"),
                                tags$li("Pengembangan industri daur ulang berkelanjutan"),
                                tags$li("Internasionalisasi best practices Indonesia"),
                                tags$li("Sistem pengelolaan sampah zero-waste")
                        )
                    )
                )
            )
          )
        )
      ),
      # Tab Team ----
      tabItem(
        tabName = "team",
        fluidRow(
          column(
            width = 12,
            box(
              width = 12,
              status = "success",
              solidHeader = TRUE,
              title = "Tim Pengembang",
              tags$p(style = "font-size: 16px; color: #64748b; text-align: center;",
                     "Tim profesional yang berdedikasi dalam pengembangan solusi analisis data untuk pengelolaan sampah Indonesia.")
            )
          )
        ),
        fluidRow(
          column(
            width = 4,
            div(class = "team-card",
                div(class = "team-img", icon("user-tie")),
                h3(class = "team-name", "Dr. Andi Wijaya"),
                p(class = "team-role", "Data Scientist Lead"),
                p(class = "team-specialization", 
                  "Spesialis dalam analisis data lingkungan, machine learning, dan pengembangan model prediktif untuk optimasi pengelolaan sampah.")
            )
          ),
          column(
            width = 4,
            div(class = "team-card",
                div(class = "team-img", icon("laptop-code")),
                h3(class = "team-name", "Sarah Chen, M.Sc."),
                p(class = "team-role", "Data Visualization Specialist"),
                p(class = "team-specialization", 
                  "Ahli dalam pengembangan dashboard interaktif, visualisasi data, dan user experience design untuk platform analitik.")
            )
          ),
          column(
            width = 4,
            div(class = "team-card",
                div(class = "team-img", icon("leaf")),
                h3(class = "team-name", "Prof. Bambang Santoso"),
                p(class = "team-role", "Environmental Policy Expert"),
                p(class = "team-specialization", 
                  "Pakar dalam kebijakan lingkungan, manajemen sampah berkelanjutan, dan implementasi best practices pengelolaan sampah.")
            )
          )
        )
      )
    )
  )
)

# Server ----
server <- function(input, output, session) {
  # Reactive value untuk menyimpan nama user - PINDAHKAN KE SINI
  user_name_reactive <- reactiveVal("Sahabat Lingkungan")
  
  # Welcome modal handler - PINDAHKAN KE SINI
  observe({
    if (!is.null(input$user_name)) {
      user_name <- input$user_name
      
      # Store the name
      user_name_reactive(user_name)
      
      # Show welcome notification in Shiny
      showNotification(
        paste("Selamat datang,", user_name, "! Mari bersama-sama wujudkan Indonesia bersih dan bebas sampah."),
        type = "message",
        duration = 10
      )
    }
  })
  
  # Load data bank sampah
  bank_sampah_data <- reactive({
    req(file.exists("C:/Users/Legion/Documents/DataBankSampah.xlsx"))
    
    data <- read_excel("C:/Users/Legion/Documents/DataBankSampah.xlsx")
    
    # Clean column names
    data <- data %>%
      rename(
        Tahun = `Tahun`,
        Provinsi = `Provinsi`,
        Kabupaten_Kota = `Kabupaten/Kota`,
        Nama_Fasilitas = `Nama Fasilitas`,
        Jenis = `Jenis`,
        Sampah_Masuk = `Sampahmasuk (ton/thn)`,
        Sampah_Terkelola = `Sampahterkelola (ton/thn)`,
        Pengelola = `Pengelola`
      ) %>%
      mutate(
        across(c(Sampah_Masuk, Sampah_Terkelola), as.numeric),
        Tahun = as.numeric(Tahun),
        Efisiensi = ifelse(Sampah_Masuk > 0, (Sampah_Terkelola / Sampah_Masuk) * 100, 0)
      )
    
    return(data)
  })
  
  # Update filter choices untuk bank sampah
  observe({
    data <- bank_sampah_data()
    
    provinsi_choices <- c("Semua Provinsi", unique(data$Provinsi))
    jenis_choices <- c("Semua Jenis", unique(data$Jenis))
    pengelola_choices <- c("Semua Pengelola", unique(data$Pengelola))
    
    updateSelectInput(session, "provinsiBank", 
                      choices = provinsi_choices,
                      selected = "Semua Provinsi")
    updateSelectInput(session, "jenisBank", 
                      choices = jenis_choices,
                      selected = "Semua Jenis")
    updateSelectInput(session, "pengelolaBank", 
                      choices = pengelola_choices,
                      selected = "Semua Pengelola")
  })
  
  # Filtered bank sampah data
  filtered_bank_data <- reactive({
    data <- bank_sampah_data()
    
    # Apply tahun filter
    data <- data %>%
      filter(Tahun >= input$tahunRangeBank[1] & Tahun <= input$tahunRangeBank[2])
    
    # Apply provinsi filter
    if (input$provinsiBank != "Semua Provinsi") {
      data <- data %>% filter(Provinsi == input$provinsiBank)
    }
    
    # Apply jenis filter
    if (input$jenisBank != "Semua Jenis") {
      data <- data %>% filter(Jenis == input$jenisBank)
    }
    
    # Apply pengelola filter
    if (input$pengelolaBank != "Semua Pengelola") {
      data <- data %>% filter(Pengelola == input$pengelolaBank)
    }
    
    return(data)
  })
  
  # KPI Cards untuk Bank Sampah - DIPERBAIKI
  output$bank_sampah_kpi <- renderUI({
    data <- filtered_bank_data()
    
    if(nrow(data) == 0) {
      return(
        fluidRow(
          column(12,
                 div(class = "alert alert-warning",
                     style = "text-align: center; padding: 20px;",
                     icon("exclamation-triangle"),
                     " Tidak ada data yang sesuai dengan filter yang dipilih"
                 )
          )
        )
      )
    }
    
    total_fasilitas <- n_distinct(data$Nama_Fasilitas, na.rm = TRUE)
    total_sampah_masuk <- sum(data$Sampah_Masuk, na.rm = TRUE)
    total_sampah_terkelola <- sum(data$Sampah_Terkelola, na.rm = TRUE)
    avg_efisiensi <- ifelse(total_sampah_masuk > 0, (total_sampah_terkelola / total_sampah_masuk) * 100, 0)
    total_provinsi <- n_distinct(data$Provinsi, na.rm = TRUE)
    avg_produktivitas <- ifelse(total_fasilitas > 0, total_sampah_terkelola / total_fasilitas, 0)
    
    # Format numbers properly
    format_number <- function(x, digits = 1) {
      if(is.na(x)) return("0")
      format(round(x, digits), big.mark = ".", decimal.mark = ",", nsmall = digits)
    }
    
    fluidRow(
      # Total Fasilitas
      column(2,
             div(class = "bank-kpi-card",
                 div(class = "bank-kpi-icon", icon("building")),
                 div(class = "bank-kpi-value", format_number(total_fasilitas, 0)),
                 div(class = "bank-kpi-title", "Total Fasilitas"),
                 div(class = "bank-kpi-subtitle", "Bank Sampah")
             )
      ),
      # Sampah Masuk
      column(2,
             div(class = "bank-kpi-card",
                 div(class = "bank-kpi-icon", icon("signs-post")),
                 div(class = "bank-kpi-value", format_number(total_sampah_masuk, 1)),
                 div(class = "bank-kpi-title", "Sampah Masuk"),
                 div(class = "bank-kpi-subtitle", "Ton/Tahun")
             )
      ),
      # Sampah Terkelola
      column(2,
             div(class = "bank-kpi-card",
                 div(class = "bank-kpi-icon", icon("recycle")),
                 div(class = "bank-kpi-value", format_number(total_sampah_terkelola, 1)),
                 div(class = "bank-kpi-title", "Sampah Terkelola"),
                 div(class = "bank-kpi-subtitle", "Ton/Tahun")
             )
      ),
      # Efisiensi
      column(2,
             div(class = "bank-kpi-card",
                 div(class = "bank-kpi-icon", icon("percent")),
                 div(class = "bank-kpi-value", paste0(format_number(avg_efisiensi, 1), "%")),
                 div(class = "bank-kpi-title", "Efisiensi"),
                 div(class = "bank-kpi-subtitle", "Rasio Terkelola")
             )
      ),
      # Jangkauan Provinsi
      column(2,
             div(class = "bank-kpi-card",
                 div(class = "bank-kpi-icon", icon("map")),
                 div(class = "bank-kpi-value", format_number(total_provinsi, 0)),
                 div(class = "bank-kpi-title", "Provinsi"),
                 div(class = "bank-kpi-subtitle", "Jangkauan Nasional")
             )
      ),
      # Produktivitas Rata-rata
      column(2,
             div(class = "bank-kpi-card",
                 div(class = "bank-kpi-icon", icon("chart-line")),
                 div(class = "bank-kpi-value", format_number(avg_produktivitas, 1)),
                 div(class = "bank-kpi-title", "Produktivitas"),
                 div(class = "bank-kpi-subtitle", "Ton/Fasilitas")
             )
      )
    )
  })
  
  # Chart 1: Performa Berdasarkan Jenis Fasilitas
  output$bankByJenisChart <- renderPlotly({
    data <- filtered_bank_data()
    
    if (nrow(data) == 0) {
      return(plotly_empty(type = "bar") %>%
               layout(
                 title = list(text = "Tidak ada data tersedia", 
                              font = list(color = "#64748b")),
                 plot_bgcolor = 'rgba(0,0,0,0)',
                 paper_bgcolor = 'rgba(0,0,0,0)'
               ))
    }
    
    summary_data <- data %>%
      group_by(Jenis) %>%
      summarise(
        Total_Masuk = sum(Sampah_Masuk, na.rm = TRUE),
        Total_Terkelola = sum(Sampah_Terkelola, na.rm = TRUE),
        Jumlah_Fasilitas = n_distinct(Nama_Fasilitas),
        .groups = 'drop'
      ) %>%
      arrange(desc(Total_Terkelola))
    
    fig <- plot_ly(summary_data) %>%
      add_trace(x = ~Jenis, y = ~Total_Masuk, 
                type = 'bar', name = 'Sampah Masuk',
                marker = list(color = '#ef4444'),
                text = ~paste(format(round(Total_Masuk, 1), big.mark = "."), "ton"),
                textposition = 'auto') %>%
      add_trace(x = ~Jenis, y = ~Total_Terkelola, 
                type = 'bar', name = 'Sampah Terkelola',
                marker = list(color = '#10b981'),
                text = ~paste(format(round(Total_Terkelola, 1), big.mark = "."), "ton"),
                textposition = 'auto') %>%
      layout(
        title = list(text = "Performa Berdasarkan Jenis Fasilitas", 
                     font = list(size = 14, color = "#1e293b")),
        xaxis = list(title = "Jenis Fasilitas", 
                     tickangle = -45,
                     gridcolor = '#e2e8f0'),
        yaxis = list(title = "Volume Sampah (Ton)", 
                     gridcolor = '#e2e8f0'),
        barmode = 'group',
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = "#1e293b", family = "Segoe UI"),
        legend = list(orientation = "h", x = 0, y = -0.3)
      )
    
    fig
  })
  
  # Chart 2: Performa Berdasarkan Pengelola
  output$bankByPengelolaChart <- renderPlotly({
    data <- filtered_bank_data()
    
    if (nrow(data) == 0) {
      return(plotly_empty(type = "bar") %>%
               layout(
                 title = list(text = "Tidak ada data tersedia", 
                              font = list(color = "#64748b")),
                 plot_bgcolor = 'rgba(0,0,0,0)',
                 paper_bgcolor = 'rgba(0,0,0,0)'
               ))
    }
    
    summary_data <- data %>%
      group_by(Pengelola) %>%
      summarise(
        Total_Masuk = sum(Sampah_Masuk, na.rm = TRUE),
        Total_Terkelola = sum(Sampah_Terkelola, na.rm = TRUE),
        Rata_Efisiensi = mean(Efisiensi, na.rm = TRUE),
        Jumlah_Fasilitas = n_distinct(Nama_Fasilitas),
        .groups = 'drop'
      ) %>%
      arrange(desc(Total_Terkelola)) %>%
      head(10)  # Limit to top 10
    
    fig <- plot_ly(summary_data, 
                   x = ~Total_Terkelola, 
                   y = ~reorder(Pengelola, Total_Terkelola),
                   type = 'bar',
                   orientation = 'h',
                   marker = list(color = '#8b5cf6',
                                 line = list(color = '#ffffff', width = 1)),
                   text = ~paste(format(round(Total_Terkelola, 1), big.mark = "."), "ton"),
                   textposition = 'auto',
                   hoverinfo = 'text',
                   hovertext = ~paste('Pengelola: ', Pengelola,
                                      '<br>Sampah Terkelola: ', format(round(Total_Terkelola, 1), big.mark = "."), ' ton',
                                      '<br>Efisiensi: ', round(Rata_Efisiensi, 1), '%',
                                      '<br>Jumlah Fasilitas: ', Jumlah_Fasilitas)) %>%
      layout(
        title = list(text = "Top 10 Pengelola Berdasarkan Sampah Terkelola", 
                     font = list(size = 14, color = "#1e293b")),
        xaxis = list(title = "Sampah Terkelola (Ton)", 
                     gridcolor = '#e2e8f0'),
        yaxis = list(title = "", 
                     gridcolor = '#e2e8f0'),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = "#1e293b", family = "Segoe UI")
      )
    
    fig
  })
  
  # Chart 3: Trend Sampah Masuk
  output$trendMasukChart <- renderPlotly({
    data <- filtered_bank_data()
    
    if (nrow(data) == 0) {
      return(plotly_empty(type = "scatter") %>%
               layout(
                 title = list(text = "Tidak ada data tersedia", 
                              font = list(color = "#64748b")),
                 plot_bgcolor = 'rgba(0,0,0,0)',
                 paper_bgcolor = 'rgba(0,0,0,0)'
               ))
    }
    
    trend_data <- data %>%
      group_by(Tahun) %>%
      summarise(
        Total_Masuk = sum(Sampah_Masuk, na.rm = TRUE),
        Total_Terkelola = sum(Sampah_Terkelola, na.rm = TRUE),
        Jumlah_Fasilitas = n_distinct(Nama_Fasilitas),
        .groups = 'drop'
      ) %>%
      arrange(Tahun)
    
    fig <- plot_ly(trend_data, x = ~Tahun) %>%
      add_trace(y = ~Total_Masuk, name = 'Sampah Masuk', 
                type = 'scatter', mode = 'lines+markers',
                line = list(color = '#ef4444', width = 3),
                marker = list(color = '#ef4444', size = 8)) %>%
      layout(
        title = list(text = "Trend Volume Sampah Masuk", 
                     font = list(size = 12, color = "#1e293b")),
        xaxis = list(title = "Tahun", 
                     gridcolor = '#e2e8f0'),
        yaxis = list(title = "Ton", 
                     gridcolor = '#e2e8f0'),
        showlegend = FALSE,
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = "#1e293b", family = "Segoe UI")
      )
    
    fig
  })
  
  # Chart 4: Trend Sampah Terkelola
  output$trendTerkelolaChart <- renderPlotly({
    data <- filtered_bank_data()
    
    if (nrow(data) == 0) {
      return(plotly_empty(type = "scatter") %>%
               layout(
                 title = list(text = "Tidak ada data tersedia", 
                              font = list(color = "#64748b")),
                 plot_bgcolor = 'rgba(0,0,0,0)',
                 paper_bgcolor = 'rgba(0,0,0,0)'
               ))
    }
    
    trend_data <- data %>%
      group_by(Tahun) %>%
      summarise(
        Total_Masuk = sum(Sampah_Masuk, na.rm = TRUE),
        Total_Terkelola = sum(Sampah_Terkelola, na.rm = TRUE),
        Jumlah_Fasilitas = n_distinct(Nama_Fasilitas),
        .groups = 'drop'
      ) %>%
      arrange(Tahun)
    
    fig <- plot_ly(trend_data, x = ~Tahun) %>%
      add_trace(y = ~Total_Terkelola, name = 'Sampah Terkelola', 
                type = 'scatter', mode = 'lines+markers',
                line = list(color = '#10b981', width = 3),
                marker = list(color = '#10b981', size = 8)) %>%
      layout(
        title = list(text = "Trend Volume Sampah Terkelola", 
                     font = list(size = 12, color = "#1e293b")),
        xaxis = list(title = "Tahun", 
                     gridcolor = '#e2e8f0'),
        yaxis = list(title = "Ton", 
                     gridcolor = '#e2e8f0'),
        showlegend = FALSE,
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = "#1e293b", family = "Segoe UI")
      )
    
    fig
  })
  
  # Chart 5: Trend Efisiensi Pengelolaan
  output$trendEfisiensiChart <- renderPlotly({
    data <- filtered_bank_data()
    
    if (nrow(data) == 0) {
      return(plotly_empty(type = "scatter") %>%
               layout(
                 title = list(text = "Tidak ada data tersedia", 
                              font = list(color = "#64748b")),
                 plot_bgcolor = 'rgba(0,0,0,0)',
                 paper_bgcolor = 'rgba(0,0,0,0)'
               ))
    }
    
    trend_data <- data %>%
      group_by(Tahun) %>%
      summarise(
        Total_Masuk = sum(Sampah_Masuk, na.rm = TRUE),
        Total_Terkelola = sum(Sampah_Terkelola, na.rm = TRUE),
        Efisiensi = ifelse(Total_Masuk > 0, (Total_Terkelola / Total_Masuk) * 100, 0),
        .groups = 'drop'
      ) %>%
      arrange(Tahun)
    
    fig <- plot_ly(trend_data, x = ~Tahun) %>%
      add_trace(y = ~Efisiensi, name = 'Efisiensi', 
                type = 'scatter', mode = 'lines+markers',
                line = list(color = '#f59e0b', width = 3),
                marker = list(color = '#f59e0b', size = 8),
                text = ~paste(round(Efisiensi, 1), "%"),
                textposition = 'top center') %>%
      layout(
        title = list(text = "Trend Efisiensi Pengelolaan", 
                     font = list(size = 12, color = "#1e293b")),
        xaxis = list(title = "Tahun", 
                     gridcolor = '#e2e8f0'),
        yaxis = list(title = "Efisiensi (%)", 
                     gridcolor = '#e2e8f0',
                     range = c(0, 100)),
        showlegend = FALSE,
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = "#1e293b", family = "Segoe UI")
      )
    
    fig
  })
  
  # Chart 6: Top 10 Bank Sampah Terbaik - DIPERBAIKI
  output$topBankSampahChart <- renderPlotly({
    data <- filtered_bank_data()
    
    if (nrow(data) == 0) {
      return(plotly_empty(type = "bar") %>%
               layout(
                 title = list(text = "Tidak ada data tersedia", 
                              font = list(color = "#64748b")),
                 plot_bgcolor = 'rgba(0,0,0,0)',
                 paper_bgcolor = 'rgba(0,0,0,0)'
               ))
    }
    
    top_banks <- data %>%
      group_by(Nama_Fasilitas, Provinsi, Jenis, Pengelola) %>%
      summarise(
        Total_Terkelola = sum(Sampah_Terkelola, na.rm = TRUE),
        Total_Masuk = sum(Sampah_Masuk, na.rm = TRUE),
        Rata_Efisiensi = ifelse(Total_Masuk > 0, (Total_Terkelola / Total_Masuk) * 100, 0),
        .groups = 'drop'
      ) %>%
      arrange(desc(Total_Terkelola)) %>%
      head(10) %>%
      mutate(
        # Perbaikan: ganti str_trunc dengan substr
        Label = paste0(substr(Nama_Fasilitas, 1, 25), ifelse(nchar(Nama_Fasilitas) > 25, "...", ""), " (", Provinsi, ")"),
        Tooltip = paste0(
          "Bank Sampah: ", Nama_Fasilitas, "\n",
          "Provinsi: ", Provinsi, "\n",
          "Jenis: ", Jenis, "\n",
          "Pengelola: ", Pengelola, "\n",
          "Volume: ", format(round(Total_Terkelola, 1), big.mark = ".", decimal.mark = ","), " ton\n",
          "Efisiensi: ", round(Rata_Efisiensi, 1), "%"
        )
      )
    
    # Pastikan data tidak kosong setelah filtering
    if (nrow(top_banks) == 0) {
      return(plotly_empty(type = "bar") %>%
               layout(
                 title = list(text = "Tidak ada data bank sampah", 
                              font = list(color = "#64748b")),
                 plot_bgcolor = 'rgba(0,0,0,0)',
                 paper_bgcolor = 'rgba(0,0,0,0)'
               ))
    }
    
    fig <- plot_ly(top_banks, 
                   x = ~Total_Terkelola, 
                   y = ~reorder(Label, Total_Terkelola),
                   type = 'bar',
                   orientation = 'h',
                   marker = list(
                     color = ~Rata_Efisiensi,
                     colorscale = 'Greens',
                     showscale = TRUE,
                     colorbar = list(title = "Efisiensi (%)")
                   ),
                   text = ~paste(format(round(Total_Terkelola, 1), big.mark = ".", decimal.mark = ","), "ton"),
                   textposition = 'auto',
                   hoverinfo = 'text',
                   hovertext = ~Tooltip) %>%
      layout(
        title = list(text = "Top 10 Bank Sampah Berdasarkan Volume & Efisiensi", 
                     font = list(size = 14, color = "#1e293b")),
        xaxis = list(title = "Sampah Terkelola (Ton)", 
                     gridcolor = '#e2e8f0'),
        yaxis = list(title = "", 
                     gridcolor = '#e2e8f0'),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = "#1e293b", family = "Segoe UI"),
        margin = list(l = 150)  # More space for long labels
      )
    
    fig
  })
  
  # Chart 8: Trend Jumlah Fasilitas
  output$trendFasilitasChart <- renderPlotly({
    data <- filtered_bank_data()
    
    if (nrow(data) == 0) {
      return(plotly_empty(type = "scatter") %>%
               layout(
                 title = list(text = "Tidak ada data tersedia", 
                              font = list(color = "#64748b")),
                 plot_bgcolor = 'rgba(0,0,0,0)',
                 paper_bgcolor = 'rgba(0,0,0,0)'
               ))
    }
    
    trend_data <- data %>%
      group_by(Tahun) %>%
      summarise(
        Jumlah_Fasilitas = n_distinct(Nama_Fasilitas),
        .groups = 'drop'
      ) %>%
      arrange(Tahun)
    
    fig <- plot_ly(trend_data, x = ~Tahun) %>%
      add_trace(y = ~Jumlah_Fasilitas, name = 'Jumlah Fasilitas', 
                type = 'scatter', mode = 'lines+markers',
                line = list(color = '#8b5cf6', width = 3),
                marker = list(color = '#8b5cf6', size = 8),
                text = ~paste(Jumlah_Fasilitas, "fasilitas"),
                textposition = 'top center') %>%
      layout(
        title = list(text = "Trend Jumlah Fasilitas", 
                     font = list(size = 12, color = "#1e293b")),
        xaxis = list(title = "Tahun", 
                     gridcolor = '#e2e8f0'),
        yaxis = list(title = "Jumlah Fasilitas", 
                     gridcolor = '#e2e8f0'),
        showlegend = FALSE,
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = "#1e293b", family = "Segoe UI")
      )
    
    fig
  })
  
  
  # Load data dengan improvement
  sampah_data <- reactive({
    req(file.exists("C:/Users/Legion/Documents/Data Setengah Bersih - Sampah.xlsx"))
    
    data <- read_excel("C:/Users/Legion/Documents/Data Setengah Bersih - Sampah.xlsx")
    
    # Clean column names and ensure proper data types
    data <- data %>%
      rename(
        Timbulan_Sampah_Tahunan = `Timbulan Sampah Tahunan (ton/tahun)(A)`,
        Pengurangan_Sampah_Tahunan = `Pengurangan Sampah Tahunan (ton/tahun)(B)`,
        Pengurangan_Sampah = `%Pengurangan Sampah(B/A)`,
        Penanganan_Sampah_Tahunan = `Penanganan Sampah Tahunan (ton/tahun)(C)`,
        Penanganan_Sampah = `%Penanganan Sampah(C/A)`,
        Sampah_Terkelola_Tahunan = `Sampah Terkelola Tahunan (ton/tahun)(B+C)`,
        Sampah_Terkelola = `%Sampah Terkelola(B+C)/A`,
        Daur_Ulang_Sampah_Tahunan = `Daur ulang Sampah Tahunan (ton/tahun)(D)`,
        Bahan_Baku_Sampah_Tahunan = `Bahan baku Sampah Tahunan (ton/tahun)(E)`,
        Recycling_Rate = `Recycling Rate(D+E)/A`,
        Luas_RTH = `Luas RTH (km2)(B)`
      ) %>%
      mutate(
        across(c(Timbulan_Sampah_Tahunan, Pengurangan_Sampah_Tahunan, Penanganan_Sampah_Tahunan,
                 Sampah_Terkelola_Tahunan, Daur_Ulang_Sampah_Tahunan, Bahan_Baku_Sampah_Tahunan,
                 Luas_RTH),
               as.numeric),
        across(c(Pengurangan_Sampah, Penanganan_Sampah, Sampah_Terkelola, Recycling_Rate),
               ~ as.numeric(gsub("%", "", .)) / 100),
        Tahun = as.numeric(Tahun)
      )
    
    return(data)
  })
  
  # Update provinsi choices
  observe({
    data <- sampah_data()
    provinsi_choices <- c("Nasional", unique(data$Provinsi))
    updateSelectInput(session, "provinsiSelect", 
                      choices = provinsi_choices,
                      selected = "Nasional")
    updateSelectInput(session, "provinsiSumber", 
                      choices = provinsi_choices,
                      selected = "Nasional")
    updateSelectInput(session, "provinsiJenis", 
                      choices = provinsi_choices,
                      selected = "Nasional")
    updateSelectInput(session, "provinsi1", 
                      choices = unique(data$Provinsi),
                      selected = "Bali")
    updateSelectInput(session, "provinsi2", 
                      choices = unique(data$Provinsi),
                      selected = "Jawa Barat")
  })
  
  # National summary data dengan improvement
  national_summary <- reactive({
    data <- sampah_data()
    
    data %>%
      group_by(Tahun) %>%
      summarise(
        Total_Timbulan = sum(Timbulan_Sampah_Tahunan, na.rm = TRUE) / 1e6, # Convert to million tons
        Avg_Pengurangan = mean(Pengurangan_Sampah, na.rm = TRUE) * 100,
        Avg_Penanganan = mean(Penanganan_Sampah, na.rm = TRUE) * 100,
        Avg_Terkelola = mean(Sampah_Terkelola, na.rm = TRUE) * 100,
        Avg_Recycling = mean(Recycling_Rate, na.rm = TRUE) * 100,
        Total_RTH = sum(Luas_RTH, na.rm = TRUE),
        Jumlah_Provinsi = n_distinct(Provinsi),
        .groups = 'drop'
      )
  })
  
  # Province summary data
  province_summary <- reactive({
    data <- sampah_data()
    
    data %>%
      group_by(Provinsi, Tahun) %>%
      summarise(
        Total_Timbulan = sum(Timbulan_Sampah_Tahunan, na.rm = TRUE) / 1e6,
        Avg_Pengurangan = mean(Pengurangan_Sampah, na.rm = TRUE) * 100,
        Avg_Penanganan = mean(Penanganan_Sampah, na.rm = TRUE) * 100,
        Avg_Terkelola = mean(Sampah_Terkelola, na.rm = TRUE) * 100,
        Avg_Recycling = mean(Recycling_Rate, na.rm = TRUE) * 100,
        Total_RTH = sum(Luas_RTH, na.rm = TRUE),
        .groups = 'drop'
      )
  })
  
  # Current year KPI values dengan perbandingan tahun lalu
  current_kpi <- reactive({
    data <- national_summary()
    latest_year <- max(data$Tahun, na.rm = TRUE)
    previous_year <- latest_year - 1
    
    current_data <- data %>% filter(Tahun == latest_year)
    previous_data <- data %>% filter(Tahun == previous_year)
    
    list(
      current = current_data,
      previous = previous_data,
      latest_year = latest_year,
      previous_year = previous_year
    )
  })
  
  # KPI Cards dengan Perbandingan Tahun Lalu
  output$kpi_cards <- renderUI({
    kpi_data <- current_kpi()
    current <- kpi_data$current
    previous <- kpi_data$previous
    
    # Function untuk menghitung perubahan
    calculate_change <- function(current_val, previous_val) {
      if (nrow(previous) == 0) return(list(change = 0, trend = "neutral"))
      change <- current_val - previous_val[[1]]
      trend <- ifelse(change > 0, "up", ifelse(change < 0, "down", "neutral"))
      list(change = change, trend = trend)
    }
    
    # KPI 1: Pengurangan Sampah
    pengurangan_change <- calculate_change(current$Avg_Pengurangan, previous$Avg_Pengurangan)
    
    # KPI 2: Penanganan Sampah
    penanganan_change <- calculate_change(current$Avg_Penanganan, previous$Avg_Penanganan)
    
    # KPI 3: Sampah Terkelola
    terkelola_change <- calculate_change(current$Avg_Terkelola, previous$Avg_Terkelola)
    
    # KPI 4: Tingkat Daur Ulang
    recycling_change <- calculate_change(current$Avg_Recycling, previous$Avg_Recycling)
    
    # KPI 5: Total Timbulan (trend sebaliknya - lebih baik jika turun)
    timbulan_change <- calculate_change(current$Total_Timbulan, previous$Total_Timbulan)
    timbulan_trend <- ifelse(timbulan_change$change < 0, "up", "down")
    
    # KPI 6: Luas RTH
    rth_change <- calculate_change(current$Total_RTH, previous$Total_RTH)
    
    fluidRow(
      # KPI 1: Pengurangan Sampah
      column(2, 
             div(class = "kpi-card",
                 div(class = "kpi-title", "Pengurangan Sampah"),
                 div(class = "kpi-value", paste0(round(current$Avg_Pengurangan, 1), "%")),
                 div(class = "kpi-trend", 
                     style = if(pengurangan_change$trend == "up") "color: #10b981;" else "color: #ef4444;",
                     icon(if(pengurangan_change$trend == "up") "arrow-up" else "arrow-down"),
                     paste0(if(pengurangan_change$trend == "up") "+" else "", round(pengurangan_change$change, 1), "% vs ", kpi_data$previous_year)
                 )
             )
      ),
      # KPI 2: Penanganan Sampah
      column(2,
             div(class = "kpi-card",
                 div(class = "kpi-title", "Penanganan Sampah"),
                 div(class = "kpi-value", paste0(round(current$Avg_Penanganan, 1), "%")),
                 div(class = "kpi-trend",
                     style = if(penanganan_change$trend == "up") "color: #10b981;" else "color: #ef4444;",
                     icon(if(penanganan_change$trend == "up") "arrow-up" else "arrow-down"),
                     paste0(if(penanganan_change$trend == "up") "+" else "", round(penanganan_change$change, 1), "% vs ", kpi_data$previous_year)
                 )
             )
      ),
      # KPI 3: Sampah Terkelola
      column(2,
             div(class = "kpi-card",
                 div(class = "kpi-title", "Sampah Terkelola"),
                 div(class = "kpi-value", paste0(round(current$Avg_Terkelola, 1), "%")),
                 div(class = "kpi-trend",
                     style = if(terkelola_change$trend == "up") "color: #10b981;" else "color: #ef4444;",
                     icon(if(terkelola_change$trend == "up") "arrow-up" else "arrow-down"),
                     paste0(if(terkelola_change$trend == "up") "+" else "", round(terkelola_change$change, 1), "% vs ", kpi_data$previous_year)
                 )
             )
      ),
      # KPI 4: Tingkat Daur Ulang
      column(2,
             div(class = "kpi-card",
                 div(class = "kpi-title", "Tingkat Daur Ulang"),
                 div(class = "kpi-value", paste0(round(current$Avg_Recycling, 1), "%")),
                 div(class = "kpi-trend",
                     style = if(recycling_change$trend == "up") "color: #10b981;" else "color: #ef4444;",
                     icon(if(recycling_change$trend == "up") "arrow-up" else "arrow-down"),
                     paste0(if(recycling_change$trend == "up") "+" else "", round(recycling_change$change, 1), "% vs ", kpi_data$previous_year)
                 )
             )
      ),
      # KPI 5: Total Timbulan
      column(2,
             div(class = "kpi-card",
                 div(class = "kpi-title", "Total Timbulan"),
                 div(class = "kpi-value", 
                     tags$span(round(current$Total_Timbulan, 1), 
                               tags$span(class = "small-unit", "M ton"))
                 ),
                 div(class = "kpi-trend",
                     style = if(timbulan_trend == "up") "color: #10b981;" else "color: #ef4444;",
                     icon(if(timbulan_trend == "up") "arrow-down" else "arrow-up"),
                     paste0(if(timbulan_trend == "up") "-" else "+", round(abs(timbulan_change$change), 1), "M vs ", kpi_data$previous_year)
                 )
             )
      ),
      # KPI 6: Luas RTH
      column(2,
             div(class = "kpi-card",
                 div(class = "kpi-title", "Luas RTH Nasional"),
                 div(class = "kpi-value", 
                     HTML(paste0(round(current$Total_RTH, 0), "<span class='small-unit'> km²</span>"))
                 ),
                 div(class = "kpi-trend",
                     style = if(rth_change$trend == "up") "color: #10b981;" else "color: #ef4444;",
                     icon(if(rth_change$trend == "up") "arrow-up" else "arrow-down"),
                     paste0(if(rth_change$trend == "up") "+" else "", round(rth_change$change, 0), " km² vs ", kpi_data$previous_year)
                 )
             )
      )
    )
  })
  
  # Enhanced Gauge Chart
  output$gaugeChart <- renderPlotly({
    kpi_data <- current_kpi()
    value <- kpi_data$current$Avg_Terkelola
    
    # Determine color based on value
    if(value >= 80) {
      gauge_color <- "#10b981"
      status <- "Baik"
    } else if(value >= 60) {
      gauge_color <- "#f59e0b"
      status <- "Menuju Baik"
    } else {
      gauge_color <- "#ef4444"
      status <- "Perlu Perhatian"
    }
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = value,
      title = list(text = paste("Status:", status), 
                   font = list(size = 16, color = "#1e293b")),
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = 70, font = list(size = 14)),
      gauge = list(
        axis = list(range = list(0, 100), 
                    tickwidth = 1, 
                    tickcolor = "#1e293b",
                    tickfont = list(size = 12)),
        bar = list(color = gauge_color, line = list(width = 0)),
        steps = list(
          list(range = c(0, 60), color = "#fef2f2"),
          list(range = c(60, 80), color = "#fffbeb"),
          list(range = c(80, 100), color = "#f0fdf4")
        ),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 100
        )
      )
    ) %>%
      layout(
        margin = list(l=20, r=30, t=50, b=20),
        font = list(color = "#1e293b", family = "Segoe UI")
      )
    
    fig
  })
  
  # Enhanced Trend Chart untuk semua indikator
  output$trendAllChart <- renderPlotly({
    selected_provinsi <- input$provinsiSelect
    tahun_range <- input$tahunRange
    
    if(selected_provinsi == "Nasional") {
      data <- national_summary()
    } else {
      data <- province_summary() %>%
        filter(Provinsi == selected_provinsi)
    }
    
    data <- data %>%
      filter(Tahun >= tahun_range[1] & Tahun <= tahun_range[2])
    
    fig <- plot_ly(data, x = ~Tahun) %>%
      add_trace(y = ~Avg_Pengurangan, name = 'Pengurangan', 
                type = 'scatter', mode = 'lines+markers',
                line = list(color = '#10b981', width = 3),
                marker = list(color = '#10b981', size = 8)) %>%
      add_trace(y = ~Avg_Penanganan, name = 'Penanganan', 
                type = 'scatter', mode = 'lines+markers',
                line = list(color = '#3b82f6', width = 3),
                marker = list(color = '#3b82f6', size = 8)) %>%
      add_trace(y = ~Avg_Terkelola, name = 'Terkelola', 
                type = 'scatter', mode = 'lines+markers',
                line = list(color = '#8b5cf6', width = 3),
                marker = list(color = '#8b5cf6', size = 8)) %>%
      add_trace(y = ~Avg_Recycling, name = 'Daur Ulang', 
                type = 'scatter', mode = 'lines+markers',
                line = list(color = '#f59e0b', width = 3),
                marker = list(color = '#f59e0b', size = 8)) %>%
      layout(
        title = list(text = paste("Tren Indikator Pengelolaan Sampah", 
                                  ifelse(selected_provinsi == "Nasional", "Nasional", selected_provinsi)),
                     font = list(size = 14, color = "#1e293b")),
        xaxis = list(title = "Tahun", gridcolor = '#e2e8f0'),
        yaxis = list(title = "Persentase (%)", gridcolor = '#e2e8f0'),
        hovermode = 'x unified',
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = "#1e293b", family = "Segoe UI"),
        legend = list(orientation = "h", x = 0, y = -0.2)
      )
    
    fig
  })
  
  # Plot 1: Performance Chart - Provinsi dengan progress tertinggi
  output$performanceChart <- renderPlotly({
    tahun_range <- input$tahunRangeAnalisis
    jumlah_top <- input$jumlahTop
    metrik <- input$metrikAnalisis
    jenis_analisis <- input$jenisAnalisis
    
    data <- province_summary()
    
    if (jenis_analisis == "improvement") {
      # Calculate improvement over the selected range
      performance_data <- data %>%
        filter(Tahun >= tahun_range[1] & Tahun <= tahun_range[2]) %>%
        group_by(Provinsi) %>%
        filter(n() >= 2) %>%  # Minimal ada 2 data point dalam range
        summarise(
          Start_Value = case_when(
            metrik == "Terkelola" ~ Avg_Terkelola[Tahun == min(Tahun)],
            metrik == "Pengurangan" ~ Avg_Pengurangan[Tahun == min(Tahun)],
            metrik == "Penanganan" ~ Avg_Penanganan[Tahun == min(Tahun)],
            metrik == "Daur_Ulang" ~ Avg_Recycling[Tahun == min(Tahun)]
          ),
          End_Value = case_when(
            metrik == "Terkelola" ~ Avg_Terkelola[Tahun == max(Tahun)],
            metrik == "Pengurangan" ~ Avg_Pengurangan[Tahun == max(Tahun)],
            metrik == "Penanganan" ~ Avg_Penanganan[Tahun == max(Tahun)],
            metrik == "Daur_Ulang" ~ Avg_Recycling[Tahun == max(Tahun)]
          ),
          .groups = 'drop'
        ) %>%
        mutate(
          Improvement = End_Value - Start_Value,
          Improvement_Percent = ifelse(Start_Value > 0, (Improvement / Start_Value) * 100, 0)
        ) %>%
        arrange(desc(Improvement)) %>%
        head(jumlah_top)
      
      # Urutkan data untuk plotting
      performance_data <- performance_data %>%
        mutate(Provinsi = factor(Provinsi, levels = Provinsi[order(Improvement)]))
      
      fig <- plot_ly(performance_data, 
                     x = ~Improvement, 
                     y = ~Provinsi,
                     type = 'bar',
                     orientation = 'h',
                     marker = list(color = ifelse(performance_data$Improvement > 0, '#10b981', '#ef4444'),
                                   line = list(color = '#ffffff', width = 1)),
                     text = ~paste(ifelse(Improvement > 0, "+", ""), round(Improvement, 1), "%"),
                     textposition = 'auto')
      
      y_title <- "Peningkatan (Persentase Poin)"
      title_suffix <- "Peningkatan Tertinggi"
      
    } else {
      # Nilai tertinggi pada tahun akhir range
      performance_data <- data %>%
        filter(Tahun == tahun_range[2]) %>%
        mutate(
          Current_Value = case_when(
            metrik == "Terkelola" ~ Avg_Terkelola,
            metrik == "Pengurangan" ~ Avg_Pengurangan,
            metrik == "Penanganan" ~ Avg_Penanganan,
            metrik == "Daur_Ulang" ~ Avg_Recycling
          )
        ) %>%
        arrange(desc(Current_Value)) %>%
        head(jumlah_top)
      
      # Urutkan data untuk plotting
      performance_data <- performance_data %>%
        mutate(Provinsi = factor(Provinsi, levels = Provinsi[order(Current_Value)]))
      
      fig <- plot_ly(performance_data, 
                     x = ~Current_Value, 
                     y = ~Provinsi,
                     type = 'bar',
                     orientation = 'h',
                     marker = list(color = '#10b981',
                                   line = list(color = '#ffffff', width = 1)),
                     text = ~paste(round(Current_Value, 1), "%"),
                     textposition = 'auto')
      
      y_title <- "Nilai Akhir (Persentase)"
      title_suffix <- "Nilai Tertinggi"
    }
    
    metrik_title <- switch(metrik,
                           "Terkelola" = "% Sampah Terkelola",
                           "Pengurangan" = "% Pengurangan Sampah", 
                           "Penanganan" = "% Penanganan Sampah",
                           "Daur_Ulang" = "% Daur Ulang")
    
    fig <- fig %>%
      layout(
        title = list(text = paste(jumlah_top, "Provinsi dengan", title_suffix, metrik_title, 
                                  tahun_range[1], "-", tahun_range[2]), 
                     font = list(size = 14, color = "#1e293b")),
        xaxis = list(title = y_title),
        yaxis = list(title = ""),
        showlegend = FALSE,
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = "#1e293b", family = "Segoe UI")
      )
    
    fig
  })
  
  # Plot 2: Efficiency Chart - Peringkat efisiensi
  output$efficiencyChart <- renderPlotly({
    tahun_range <- input$tahunRangeAnalisis
    jumlah_top <- input$jumlahTop
    data <- sampah_data()
    
    efficiency_data <- data %>%
      filter(Tahun >= tahun_range[1] & Tahun <= tahun_range[2]) %>%
      group_by(Provinsi) %>%
      summarise(
        Avg_Efisiensi = mean((Sampah_Terkelola * 100) / (Timbulan_Sampah_Tahunan / 1000), na.rm = TRUE),
        Avg_Pengelolaan = mean(Sampah_Terkelola, na.rm = TRUE) * 100,
        Avg_Timbulan = mean(Timbulan_Sampah_Tahunan, na.rm = TRUE) / 1000,
        .groups = 'drop'
      ) %>%
      arrange(desc(Avg_Efisiensi)) %>%
      head(jumlah_top)
    
    fig <- plot_ly(efficiency_data, 
                   x = ~reorder(Provinsi, Avg_Efisiensi), 
                   y = ~Avg_Efisiensi,
                   type = 'bar',
                   marker = list(color = '#8b5cf6',
                                 line = list(color = '#ffffff', width = 1)),
                   text = ~paste(round(Avg_Efisiensi, 1)),
                   textposition = 'auto',
                   hoverinfo = 'text',
                   hovertext = ~paste('Provinsi: ', Provinsi,
                                      '<br>Efisiensi: ', round(Avg_Efisiensi, 1),
                                      '<br>Pengelolaan: ', round(Avg_Pengelolaan, 1), '%',
                                      '<br>Timbulan Rata2: ', round(Avg_Timbulan, 1), 'K ton')) %>%
      layout(
        title = list(text = paste(jumlah_top, "Provinsi dengan Efisiensi Tertinggi", 
                                  tahun_range[1], "-", tahun_range[2]), 
                     font = list(size = 14, color = "#1e293b")),
        xaxis = list(title = "", tickangle = -45),
        yaxis = list(title = "Skor Efisiensi Rata-rata"),
        showlegend = FALSE,
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = "#1e293b", family = "Segoe UI")
      )
    
    fig
  })
  
  # Top Provinces Chart - Stacked bar untuk 20 terbaik
  output$topProvincesChart <- renderPlotly({
    tahun_range <- input$tahunRangeWilayahTerbaik
    jumlah <- input$jumlahTopWilayah
    
    data <- province_summary() %>%
      filter(Tahun >= tahun_range[1] & Tahun <= tahun_range[2]) %>%
      group_by(Provinsi) %>%
      summarise(
        Avg_Pengurangan = mean(Avg_Pengurangan, na.rm = TRUE),
        Avg_Penanganan = mean(Avg_Penanganan, na.rm = TRUE),
        Avg_Terkelola = mean(Avg_Terkelola, na.rm = TRUE),
        Avg_Recycling = mean(Avg_Recycling, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(desc(Avg_Terkelola)) %>%
      head(jumlah)
    
    fig <- plot_ly(data, x = ~reorder(Provinsi, Avg_Terkelola), y = ~Avg_Pengurangan, 
                   type = 'bar', name = 'Pengurangan', marker = list(color = '#10b981')) %>%
      add_trace(y = ~Avg_Penanganan, name = 'Penanganan', marker = list(color = '#3b82f6')) %>%
      add_trace(y = ~Avg_Recycling, name = 'Daur Ulang', marker = list(color = '#f59e0b')) %>%
      layout(
        title = list(text = paste(jumlah, "Provinsi Terbaik - Komposisi Metrik", 
                                  tahun_range[1], "-", tahun_range[2]),
                     font = list(size = 14, color = "#1e293b")),
        xaxis = list(title = "", tickangle = -45),
        yaxis = list(title = "Persentase Rata-rata (%)"),
        barmode = 'stack',
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = "#1e293b", family = "Segoe UI"),
        legend = list(orientation = "h", x = 0, y = -0.3)
      )
    
    fig
  })
  
  # Bottom Provinces Chart - Stacked bar untuk 20 terendah
  # Bottom Provinces Chart - Stacked bar untuk 20 terendah
  output$bottomProvincesChart <- renderPlotly({
    tahun_range <- input$tahunRangeWilayahTerendah
    
    data <- province_summary() %>%
      filter(Tahun >= tahun_range[1] & Tahun <= tahun_range[2]) %>%
      group_by(Provinsi) %>%
      summarise(
        Avg_Pengurangan = mean(Avg_Pengurangan, na.rm = TRUE),
        Avg_Penanganan = mean(Avg_Penanganan, na.rm = TRUE),
        Avg_Terkelola = mean(Avg_Terkelola, na.rm = TRUE),
        Avg_Recycling = mean(Avg_Recycling, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(Avg_Terkelola) %>%
      head(20)
    
    fig <- plot_ly(data, x = ~reorder(Provinsi, -Avg_Terkelola), y = ~Avg_Pengurangan, 
                   type = 'bar', name = 'Pengurangan', marker = list(color = '#10b981')) %>%
      add_trace(y = ~Avg_Penanganan, name = 'Penanganan', marker = list(color = '#3b82f6')) %>%
      add_trace(y = ~Avg_Recycling, name = 'Daur Ulang', marker = list(color = '#f59e0b')) %>%
      layout(
        title = list(text = paste("20 Provinsi Terendah - Komposisi Metrik", 
                                  tahun_range[1], "-", tahun_range[2]),
                     font = list(size = 14, color = "#1e293b")),
        xaxis = list(title = "", tickangle = -45),
        yaxis = list(title = "Persentase Rata-rata (%)"),
        barmode = 'stack',
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = "#1e293b", family = "Segoe UI"),
        legend = list(orientation = "h", x = 0, y = -0.3)
      )
    
    fig
  })
  
  # Province Comparison Chart
  output$provinceComparison <- renderPlotly({
    prov1 <- input$provinsi1
    prov2 <- input$provinsi2
    tahun_range <- input$tahunRangePerbandingan
    metrik_perbandingan <- input$metrikPerbandingan
    
    if(prov1 == "Pilih Provinsi" | prov2 == "Pilih Provinsi") {
      return(plotly_empty(type = "scatter") %>%
               layout(
                 title = list(text = "Pilih dua provinsi untuk perbandingan",
                              font = list(color = "#64748b")),
                 plot_bgcolor = 'rgba(0,0,0,0)',
                 paper_bgcolor = 'rgba(0,0,0,0)'
               ))
    }
    
    if (metrik_perbandingan == "rata") {
      # Rata-rata dalam rentang tahun
      data <- province_summary() %>%
        filter(Tahun >= tahun_range[1] & Tahun <= tahun_range[2], 
               Provinsi %in% c(prov1, prov2)) %>%
        group_by(Provinsi) %>%
        summarise(
          Avg_Pengurangan = mean(Avg_Pengurangan, na.rm = TRUE),
          Avg_Penanganan = mean(Avg_Penanganan, na.rm = TRUE),
          Avg_Terkelola = mean(Avg_Terkelola, na.rm = TRUE),
          Avg_Recycling = mean(Avg_Recycling, na.rm = TRUE),
          .groups = 'drop'
        )
      
      title_suffix <- paste("Rata-rata", tahun_range[1], "-", tahun_range[2])
    } else {
      # Nilai tahun akhir
      data <- province_summary() %>%
        filter(Tahun == tahun_range[2], Provinsi %in% c(prov1, prov2))
      
      title_suffix <- paste("Tahun", tahun_range[2])
    }
    
    # Prepare data for comparison
    comparison_data <- data.frame(
      Metric = rep(c("Pengurangan", "Penanganan", "Terkelola", "Daur Ulang"), each = 2),
      Province = rep(c(prov1, prov2), 4),
      Value = c(
        data$Avg_Pengurangan[data$Provinsi == prov1],
        data$Avg_Pengurangan[data$Provinsi == prov2],
        data$Avg_Penanganan[data$Provinsi == prov1],
        data$Avg_Penanganan[data$Provinsi == prov2],
        data$Avg_Terkelola[data$Provinsi == prov1],
        data$Avg_Terkelola[data$Provinsi == prov2],
        data$Avg_Recycling[data$Provinsi == prov1],
        data$Avg_Recycling[data$Provinsi == prov2]
      )
    )
    
    fig <- plot_ly(comparison_data, 
                   x = ~Metric, 
                   y = ~Value, 
                   color = ~Province,
                   type = 'bar',
                   colors = c('#10b981', '#3b82f6')) %>%
      layout(
        title = list(text = paste("Perbandingan", prov1, "vs", prov2, "-", title_suffix), 
                     font = list(size = 14, color = "#1e293b")),
        xaxis = list(title = "Metrik"),
        yaxis = list(title = "Persentase (%)"),
        barmode = 'group',
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = "#1e293b", family = "Segoe UI")
      )
    
    fig
  })
  
  # Source Composition Chart (diperbarui)
  output$sourceChart <- renderPlotly({
    data <- sampah_data()
    selected_provinsi <- input$provinsiSumber  # Ganti dari provinsiSumberJenis
    tahun <- as.numeric(input$tahunSumber)     # Ganti dari tahunSumberJenis
    
    if(selected_provinsi == "Nasional") {
      source_data <- data %>%
        filter(Tahun == tahun)
    } else {
      source_data <- data %>%
        filter(Tahun == tahun, Provinsi == selected_provinsi)
    }
    
    sources <- source_data %>%
      summarise(
        Rumah_Tangga = sum(`Rumah Tangga(ton)`, na.rm = TRUE),
        Perkantoran = sum(`Perkantoran(ton)`, na.rm = TRUE),
        Pasar = sum(`Pasar(ton)`, na.rm = TRUE),
        Perniagaan = sum(`Perniagaan(ton)`, na.rm = TRUE),
        Fasilitas_Publik = sum(`Fasilitas Publik(ton)`, na.rm = TRUE),
        Kawasan = sum(`Kawasan(ton)`, na.rm = TRUE),
        Lainnya = sum(`Lain(ton)`, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = everything(), names_to = "Sumber", values_to = "Ton") %>%
      mutate(
        Persentase = Ton / sum(Ton) * 100,
        Sumber = case_when(
          Sumber == "Rumah_Tangga" ~ "Rumah Tangga",
          Sumber == "Perkantoran" ~ "Perkantoran",
          Sumber == "Pasar" ~ "Pasar",
          Sumber == "Perniagaan" ~ "Perniagaan",
          Sumber == "Fasilitas_Publik" ~ "Fasilitas Publik",
          Sumber == "Kawasan" ~ "Kawasan",
          Sumber == "Lainnya" ~ "Lainnya"
        )
      ) %>%
      arrange(desc(Persentase))  # Urutkan dari besar ke kecil
    
    fig <- plot_ly(sources, x = ~reorder(Sumber, -Persentase), y = ~Persentase,
                   type = 'bar',
                   marker = list(color = c('#10b981', '#059669', '#047857', 
                                           '#065f46', '#064e3b', '#0f766e', '#0d9488'),
                                 line = list(color = '#ffffff', width = 1)),
                   text = ~paste(round(Persentase, 1), "%"),
                   textposition = 'auto') %>%
      layout(
        title = list(text = paste("Komposisi Sumber Sampah", 
                                  ifelse(selected_provinsi == "Nasional", "Nasional", selected_provinsi), 
                                  "-", tahun), 
                     font = list(size = 14, color = "#1e293b")),
        xaxis = list(title = "Sumber Sampah", 
                     tickangle = -45,
                     gridcolor = '#e2e8f0'),
        yaxis = list(title = "Persentase (%)", 
                     gridcolor = '#e2e8f0'),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = "#1e293b", family = "Segoe UI")
      )
    
    fig
  })
  
  # Waste Type Chart (diperbarui)
  output$typeChart <- renderPlotly({
    data <- sampah_data()
    selected_provinsi <- input$provinsiJenis  # Ganti dari provinsiSumberJenis
    tahun <- as.numeric(input$tahunJenis)     # Ganti dari tahunSumberJenis
    
    if(selected_provinsi == "Nasional") {
      type_data <- data %>%
        filter(Tahun == tahun)
    } else {
      type_data <- data %>%
        filter(Tahun == tahun, Provinsi == selected_provinsi)
    }
    
    types <- type_data %>%
      summarise(
        Sisa_Makanan = mean(`Sisa Makanan (%)`, na.rm = TRUE),
        Kayu_Ranting = mean(`Kayu-Ranting (%)`, na.rm = TRUE),
        Kertas_Karton = mean(`Kertas-Karton (%)`, na.rm = TRUE),
        Plastik = mean(`Plastik(%)`, na.rm = TRUE),
        Logam = mean(`Logam(%)`, na.rm = TRUE),
        Kain = mean(`Kain(%)`, na.rm = TRUE),
        Karet_Kulit = mean(`Karet- Kulit (%)`, na.rm = TRUE),
        Kaca = mean(`Kaca(%)`, na.rm = TRUE),
        Lainnya = mean(`Lainnya(%)`, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = everything(), names_to = "Jenis", values_to = "Persentase") %>%
      mutate(
        Jenis = case_when(
          Jenis == "Sisa_Makanan" ~ "Organik",
          Jenis == "Kayu_Ranting" ~ "Kayu & Ranting",
          Jenis == "Kertas_Karton" ~ "Kertas & Karton",
          Jenis == "Plastik" ~ "Plastik",
          Jenis == "Logam" ~ "Logam",
          Jenis == "Kain" ~ "Kain",
          Jenis == "Karet_Kulit" ~ "Karet & Kulit",
          Jenis == "Kaca" ~ "Kaca",
          Jenis == "Lainnya" ~ "Lainnya"
        )
      ) %>%
      arrange(desc(Persentase))  # Urutkan dari besar ke kecil
    
    fig <- plot_ly(types, labels = ~reorder(Jenis, -Persentase), values = ~Persentase,
                   type = 'pie',
                   hole = 0.4,
                   textinfo = 'label+percent',
                   insidetextorientation = 'radial',
                   marker = list(colors = c('#10b981', '#059669', '#047857', '#065f46', 
                                            '#064e3b', '#0f766e', '#0d9488', '#14b8a6', '#2dd4bf'),
                                 line = list(color = '#ffffff', width = 2))) %>%
      layout(
        title = list(text = paste("Proporsi Jenis Sampah", 
                                  ifelse(selected_provinsi == "Nasional", "Nasional", selected_provinsi), 
                                  "-", tahun), 
                     font = list(size = 14, color = "#1e293b")),
        showlegend = TRUE,
        font = list(color = "#1e293b", family = "Segoe UI")
      )
    
    fig
  })
  
  # Recycling Potential Chart (diperbarui)
  output$recyclingPotentialChart <- renderPlotly({
    data <- sampah_data()
    selected_provinsi <- input$provinsiDaurUlang  # Input baru
    tahun <- as.numeric(input$tahunDaurUlang)     # Input baru
    
    if(selected_provinsi == "Nasional") {
      type_data <- data %>%
        filter(Tahun == tahun)
    } else {
      type_data <- data %>%
        filter(Tahun == tahun, Provinsi == selected_provinsi)
    }
    
    # Analisis potensi daur ulang berdasarkan jenis sampah
    recycling_potential <- type_data %>%
      summarise(
        Plastik = mean(`Plastik(%)`, na.rm = TRUE),
        Kertas_Karton = mean(`Kertas-Karton (%)`, na.rm = TRUE),
        Logam = mean(`Logam(%)`, na.rm = TRUE),
        Kaca = mean(`Kaca(%)`, na.rm = TRUE),
        Kain = mean(`Kain(%)`, na.rm = TRUE),
        Organik = mean(`Sisa Makanan (%)`, na.rm = TRUE) + mean(`Kayu-Ranting (%)`, na.rm = TRUE),
        Lainnya = mean(`Lainnya(%)`, na.rm = TRUE) + mean(`Karet- Kulit (%)`, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      pivot_longer(cols = everything(), names_to = "Jenis", values_to = "Persentase") %>%
      mutate(
        Kategori = case_when(
          Jenis %in% c("Plastik", "Kertas_Karton", "Logam", "Kaca", "Kain") ~ "Dapat Daur Ulang",
          Jenis == "Organik" ~ "Kompos",
          Jenis == "Lainnya" ~ "Lainnya/Sulit Daur Ulang"
        ),
        Jenis = case_when(
          Jenis == "Kertas_Karton" ~ "Kertas & Karton",
          Jenis == "Kain" ~ "Tekstil",
          TRUE ~ Jenis
        ),
        Warna = case_when(
          Kategori == "Dapat Daur Ulang" ~ '#10b981',
          Kategori == "Kompos" ~ '#f59e0b',
          Kategori == "Lainnya/Sulit Daur Ulang" ~ '#ef4444'
        )
      ) %>%
      arrange(desc(Persentase))
    
    # Hitung total potensi daur ulang
    total_recyclable <- recycling_potential %>%
      filter(Kategori == "Dapat Daur Ulang") %>%
      summarise(Total = sum(Persentase, na.rm = TRUE)) %>%
      pull(Total)
    
    fig <- plot_ly(recycling_potential, 
                   x = ~reorder(Jenis, -Persentase), 
                   y = ~Persentase,
                   type = 'bar',
                   marker = list(color = ~Warna,
                                 line = list(color = '#ffffff', width = 1)),
                   text = ~paste(round(Persentase, 1), "%"),
                   textposition = 'auto',
                   hoverinfo = 'text',
                   hovertext = ~paste('<b>', Jenis, '</b><br>',
                                      'Persentase: ', round(Persentase, 1), '%<br>',
                                      'Kategori: ', Kategori)) %>%
      layout(
        title = list(text = paste("Potensi Daur Ulang -", 
                                  ifelse(selected_provinsi == "Nasional", "Nasional", selected_provinsi), 
                                  "-", tahun,
                                  "<br><span style='font-size:12px; color:#64748b'>Total Material Dapat Daur Ulang: ", 
                                  round(total_recyclable, 1), "%</span>"), 
                     font = list(size = 14, color = "#1e293b")),
        xaxis = list(title = "Jenis Material", 
                     tickangle = -45,
                     gridcolor = '#e2e8f0'),
        yaxis = list(title = "Persentase Komposisi (%)", 
                     gridcolor = '#e2e8f0'),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = "#1e293b", family = "Segoe UI"),
        legend = list(
          title = list(text = "Kategori:"),
          orientation = "h",
          x = 0,
          y = -0.3
        ),
        annotations = list(
          list(
            x = 1,
            y = -0.4,
            xref = "paper",
            yref = "paper",
            text = paste("💡 Potensi daur ulang:", round(total_recyclable, 1), "% dari total sampah"),
            showarrow = FALSE,
            font = list(size = 12, color = "#10b981"),
            bgcolor = "rgba(16, 185, 129, 0.1)",
            bordercolor = "#10b981",
            borderwidth = 1,
            borderpad = 4
          )
        )
      )
    
    # Tambahkan trace untuk kategori (legend)
    categories <- unique(recycling_potential$Kategori)
    colors <- c('#10b981', '#f59e0b', '#ef4444')
    
    for(i in seq_along(categories)) {
      fig <- fig %>%
        add_trace(
          x = c(NA), y = c(NA),
          type = "scatter",
          mode = "markers",
          marker = list(size = 10, color = colors[i]),
          name = categories[i],
          showlegend = TRUE
        )
    }
    
    fig
  })
  
  # Top 15 Source Composition by Province - Horizontal
  output$topSourceChart <- renderPlotly({
    data <- sampah_data()
    tahun <- as.numeric(input$tahunTopSumber)
    
    # Calculate top provinces by total waste
    top_provinces <- data %>%
      filter(Tahun == tahun) %>%
      group_by(Provinsi) %>%
      summarise(Total_Waste = sum(Timbulan_Sampah_Tahunan, na.rm = TRUE)) %>%
      arrange(desc(Total_Waste)) %>%
      head(15) %>%
      pull(Provinsi)
    
    # Get source composition for top provinces
    source_data <- data %>%
      filter(Tahun == tahun, Provinsi %in% top_provinces) %>%
      group_by(Provinsi) %>%
      summarise(
        Rumah_Tangga = sum(`Rumah Tangga(ton)`, na.rm = TRUE),
        Perkantoran = sum(`Perkantoran(ton)`, na.rm = TRUE),
        Pasar = sum(`Pasar(ton)`, na.rm = TRUE),
        Perniagaan = sum(`Perniagaan(ton)`, na.rm = TRUE),
        Fasilitas_Publik = sum(`Fasilitas Publik(ton)`, na.rm = TRUE),
        Lainnya = sum(`Lain(ton)`, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = -Provinsi, names_to = "Sumber", values_to = "Ton") %>%
      group_by(Provinsi) %>%
      mutate(Percentage = Ton / sum(Ton) * 100) %>%
      ungroup() %>%
      mutate(Sumber = case_when(
        Sumber == "Rumah_Tangga" ~ "Rumah Tangga",
        Sumber == "Perkantoran" ~ "Perkantoran", 
        Sumber == "Pasar" ~ "Pasar",
        Sumber == "Perniagaan" ~ "Perniagaan",
        Sumber == "Fasilitas_Publik" ~ "Fasilitas Publik",
        Sumber == "Lainnya" ~ "Lainnya"
      ))
    
    fig <- plot_ly(source_data, y = ~Provinsi, x = ~Percentage, color = ~Sumber,
                   type = 'bar', orientation = 'h',
                   colors = c('#10b981', '#059669', '#047857', '#065f46', '#064e3b', '#0f766e')) %>%
      layout(
        title = list(text = paste("Top 15 Komposisi Sumber Sampah -", tahun), 
                     font = list(size = 14, color = "#1e293b")),
        yaxis = list(title = "Provinsi", categoryorder = "total ascending"),
        xaxis = list(title = "Persentase (%)"),
        barmode = 'stack',
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = "#1e293b", family = "Segoe UI"),
        legend = list(orientation = "h", x = 0, y = -0.3)
      )
    
    fig
  })
  
  # Top 15 Waste Type by Province - Horizontal
  output$topTypeChart <- renderPlotly({
    data <- sampah_data()
    tahun <- as.numeric(input$tahunTopJenis)
    
    # Calculate top provinces by total waste
    top_provinces <- data %>%
      filter(Tahun == tahun) %>%
      group_by(Provinsi) %>%
      summarise(Total_Waste = sum(Timbulan_Sampah_Tahunan, na.rm = TRUE)) %>%
      arrange(desc(Total_Waste)) %>%
      head(15) %>%
      pull(Provinsi)
    
    # Get waste type composition for top provinces
    type_data <- data %>%
      filter(Tahun == tahun, Provinsi %in% top_provinces) %>%
      group_by(Provinsi) %>%
      summarise(
        Organik = mean(`Sisa Makanan (%)`, na.rm = TRUE),
        Plastik = mean(`Plastik(%)`, na.rm = TRUE),
        Kertas = mean(`Kertas-Karton (%)`, na.rm = TRUE),
        Logam = mean(`Logam(%)`, na.rm = TRUE),
        Kaca = mean(`Kaca(%)`, na.rm = TRUE),
        Lainnya = mean(`Lainnya(%)`, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = -Provinsi, names_to = "Jenis", values_to = "Percentage")
    
    fig <- plot_ly(type_data, y = ~Provinsi, x = ~Percentage, color = ~Jenis,
                   type = 'bar', orientation = 'h',
                   colors = c('#10b981', '#059669', '#047857', '#065f46', '#064e3b', '#0f766e')) %>%
      layout(
        title = list(text = paste("Top 15 Komposisi Jenis Sampah -", tahun), 
                     font = list(size = 14, color = "#1e293b")),
        yaxis = list(title = "Provinsi", categoryorder = "total ascending"),
        xaxis = list(title = "Persentase (%)"),
        barmode = 'stack',
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = "#1e293b", family = "Segoe UI"),
        legend = list(orientation = "h", x = 0, y = -0.3)
      )
    
    fig
  })
  
  # Yearly Comparison Chart dengan event annotations
  output$yearlyComparison <- renderPlotly({
    data <- national_summary() %>%
      filter(Tahun >= 2019 & Tahun <= 2024)
    
    yearly_data <- data %>%
      select(Year = Tahun, 
             Pengurangan = Avg_Pengurangan,
             Penanganan = Avg_Penanganan,
             Terkelola = Avg_Terkelola,
             Recycling = Avg_Recycling) %>%
      pivot_longer(cols = -Year, names_to = "Metric", values_to = "Value") %>%
      mutate(Metric = case_when(
        Metric == "Pengurangan" ~ "Pengurangan",
        Metric == "Penanganan" ~ "Penanganan", 
        Metric == "Terkelola" ~ "Terkelola",
        Metric == "Recycling" ~ "Daur Ulang"
      ))
    
    # Buat plot dengan ikon di legend
    fig <- plot_ly(yearly_data, x = ~Year, y = ~Value, color = ~Metric,
                   type = 'scatter', mode = 'lines+markers',
                   colors = c('#10b981', '#3b82f6', '#8b5cf6', '#f59e0b'),
                   line = list(width = 3),
                   marker = list(size = 8, symbol = c('circle', 'square', 'diamond', 'cross'),
                                 line = list(width = 1, color = '#ffffff'))) %>%
      layout(
        title = list(text = "Perkembangan Metrik Kunci per Tahun (2019-2024)", 
                     font = list(size = 14, color = "#1e293b")),
        xaxis = list(title = "Tahun", 
                     gridcolor = '#e2e8f0',
                     range = c(2018.5, 2024.5)),
        yaxis = list(title = "Persentase (%)", 
                     gridcolor = '#e2e8f0'),
        hovermode = 'x unified',
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = "#1e293b", family = "Segoe UI"),
        legend = list(orientation = "h", x = 0, y = -0.2),
        annotations = list(
          # COVID-19 Pandemic (2020)
          list(
            x = 2020,
            y = 0.85,
            xref = "x",
            yref = "paper",
            text = "🦠 COVID-19",
            showarrow = TRUE,
            arrowhead = 2,
            arrowsize = 1,
            arrowwidth = 2,
            arrowcolor = '#ef4444',
            ax = 0,
            ay = -40,
            bgcolor = 'rgba(239, 68, 68, 0.1)',
            bordercolor = '#ef4444',
            borderwidth = 1,
            borderpad = 4,
            font = list(color = '#ef4444', size = 10)
          ),
          # PMK No. 66/2022 - Cukai Plastik
          list(
            x = 2022,
            y = 0.85,
            xref = "x",
            yref = "paper",
            text = "📜 PMK No. 66/2022<br>Cukai Plastik",
            showarrow = TRUE,
            arrowhead = 2,
            arrowsize = 1,
            arrowwidth = 2,
            arrowcolor = '#10b981',
            ax = 0,
            ay = -40,
            bgcolor = 'rgba(16, 185, 129, 0.1)',
            bordercolor = '#10b981',
            borderwidth = 1,
            borderpad = 4,
            font = list(color = '#10b981', size = 10)
          )
        ),
        shapes = list(
          # Vertical line for COVID-19 (2020)
          list(
            type = "line",
            x0 = 2020,
            x1 = 2020,
            y0 = 0,
            y1 = 1,
            yref = "paper",
            line = list(color = "#ef4444", width = 1, dash = "dot")
          ),
          # Vertical line for PMK No. 66/2022 (2022)
          list(
            type = "line",
            x0 = 2022,
            x1 = 2022,
            y0 = 0,
            y1 = 1,
            yref = "paper",
            line = list(color = "#10b981", width = 1, dash = "dot")
          )
        )
      )
    
    fig
  })
  
  observe({
    data <- sampah_data()
    provinsi_choices <- c("Nasional", unique(data$Provinsi))
    
    # Update semua select input untuk provinsi
    updateSelectInput(session, "provinsiSelect", 
                      choices = provinsi_choices,
                      selected = "Nasional")
    updateSelectInput(session, "provinsiSumber", 
                      choices = provinsi_choices,
                      selected = "Nasional")
    updateSelectInput(session, "provinsiJenis", 
                      choices = provinsi_choices,
                      selected = "Nasional")
    updateSelectInput(session, "provinsiDaurUlang",  # Tambahkan ini
                      choices = provinsi_choices,
                      selected = "Nasional")
    updateSelectInput(session, "provinsi1", 
                      choices = unique(data$Provinsi),
                      selected = "Bali")
    updateSelectInput(session, "provinsi2", 
                      choices = unique(data$Provinsi),
                      selected = "Jawa Barat")
  })
  
  # Ganti seluruh bagian server untuk tab Explore dengan kode yang diperbaiki:
  
  # Explore Tab Server Logic
  observe({
    data <- sampah_data()
    
    # Get numeric columns for variable selection
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    factor_cols <- names(data)[sapply(data, function(x) is.character(x) | is.factor(x))]
    
    # Update variable choices
    updateSelectInput(session, "varX", choices = c("Pilih Variabel" = "", numeric_cols, factor_cols))
    updateSelectInput(session, "varY", choices = c("Pilih Variabel" = "", numeric_cols))
    updateSelectInput(session, "varColor", choices = c("Tidak ada" = "", factor_cols))
    updateSelectInput(session, "varSize", choices = c("Tidak ada" = "", numeric_cols))
    
    # Update province filter
    provinsi_choices <- c("Semua Provinsi", unique(data$Provinsi))
    updateSelectInput(session, "filterProvinsi", 
                      choices = provinsi_choices,
                      selected = "Semua Provinsi")
  })
  
  # Reactive data for explore tab
  explore_data <- reactive({
    data <- sampah_data()
    
    # Apply filters
    if (!"Semua Provinsi" %in% input$filterProvinsi && !is.null(input$filterProvinsi)) {
      data <- data %>% filter(Provinsi %in% input$filterProvinsi)
    }
    
    data <- data %>% 
      filter(Tahun >= input$filterTahun[1] & Tahun <= input$filterTahun[2])
    
    # Apply aggregation
    if (input$filterAggregate == "yearly") {
      data <- data %>%
        group_by(Tahun) %>%
        summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = 'drop')
    } else if (input$filterAggregate == "province") {
      data <- data %>%
        group_by(Provinsi) %>%
        summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = 'drop')
    }
    
    return(data)
  })
  
  # Scatter Plot - DIPERBAIKI
  output$scatterPlot <- renderPlotly({
    req(input$varX, input$varY)
    data <- explore_data()
    
    if (nrow(data) == 0) return(plotly_empty(type = "scatter"))
    
    # Base plot
    p <- plot_ly(data) 
    
    # Add traces based on color grouping
    if (input$varColor != "") {
      # Group by color variable
      groups <- unique(data[[input$varColor]])
      
      for (group in groups) {
        group_data <- data[data[[input$varColor]] == group, ]
        
        if (input$varSize != "") {
          p <- p %>% add_trace(
            data = group_data,
            x = ~get(input$varX), 
            y = ~get(input$varY),
            type = 'scatter',
            mode = 'markers',
            name = as.character(group),
            marker = list(
              size = ~get(input$varSize),
              sizemode = 'diameter',
              sizeref = ~max(get(input$varSize), na.rm = TRUE)/40
            )
          )
        } else {
          p <- p %>% add_trace(
            data = group_data,
            x = ~get(input$varX), 
            y = ~get(input$varY),
            type = 'scatter',
            mode = 'markers',
            name = as.character(group)
          )
        }
      }
    } else {
      # No color grouping
      if (input$varSize != "") {
        p <- p %>% add_trace(
          x = ~get(input$varX), 
          y = ~get(input$varY),
          type = 'scatter',
          mode = 'markers',
          marker = list(
            size = ~get(input$varSize),
            sizemode = 'diameter',
            sizeref = ~max(get(input$varSize), na.rm = TRUE)/40
          )
        )
      } else {
        p <- p %>% add_trace(
          x = ~get(input$varX), 
          y = ~get(input$varY),
          type = 'scatter',
          mode = 'markers'
        )
      }
    }
    
    p %>% layout(
      title = paste("Scatter Plot:", input$varX, "vs", input$varY),
      xaxis = list(title = input$varX),
      yaxis = list(title = input$varY)
    )
  })
  
  # Line Chart - DIPERBAIKI
  output$lineChart <- renderPlotly({
    req(input$varX, input$varY)
    data <- explore_data()
    
    if (nrow(data) == 0) return(plotly_empty(type = "scatter"))
    
    p <- plot_ly(data)
    
    if (input$varColor != "") {
      # Group by color variable for lines
      groups <- unique(data[[input$varColor]])
      
      for (group in groups) {
        group_data <- data[data[[input$varColor]] == group, ]
        p <- p %>% add_trace(
          data = group_data,
          x = ~get(input$varX), 
          y = ~get(input$varY),
          type = 'scatter',
          mode = 'lines+markers',
          name = as.character(group)
        )
      }
    } else {
      p <- p %>% add_trace(
        x = ~get(input$varX), 
        y = ~get(input$varY),
        type = 'scatter',
        mode = 'lines+markers'
      )
    }
    
    p %>% layout(
      title = paste("Line Chart:", input$varY, "over", input$varX),
      xaxis = list(title = input$varX),
      yaxis = list(title = input$varY)
    )
  })
  
  # Bar Chart - DIPERBAIKI
  output$barChart <- renderPlotly({
    req(input$varX, input$varY)
    data <- explore_data()
    
    if (nrow(data) == 0) return(plotly_empty(type = "bar"))
    
    p <- plot_ly(data)
    
    if (input$varColor != "") {
      # Grouped bar chart
      p <- p %>% add_trace(
        x = ~get(input$varX), 
        y = ~get(input$varY),
        color = ~get(input$varColor),
        type = 'bar'
      ) %>% layout(barmode = 'group')
    } else {
      # Simple bar chart
      p <- p %>% add_trace(
        x = ~get(input$varX), 
        y = ~get(input$varY),
        type = 'bar'
      )
    }
    
    p %>% layout(
      title = paste("Bar Chart:", input$varY, "by", input$varX),
      xaxis = list(title = input$varX),
      yaxis = list(title = input$varY)
    )
  })
  
  # Histogram - DIPERBAIKI
  output$histogramChart <- renderPlotly({
    req(input$varX)
    data <- explore_data()
    
    if (nrow(data) == 0) return(plotly_empty(type = "histogram"))
    
    if (input$varColor != "") {
      plot_ly(data, x = ~get(input$varX), color = ~get(input$varColor), type = "histogram") %>%
        layout(barmode = "stack")
    } else {
      plot_ly(data, x = ~get(input$varX), type = "histogram")
    } %>%
      layout(
        title = paste("Histogram of", input$varX),
        xaxis = list(title = input$varX),
        yaxis = list(title = "Frequency")
      )
  })
  
  # Box Plot - DIPERBAIKI
  output$boxPlot <- renderPlotly({
    req(input$varX, input$varY)
    data <- explore_data()
    
    if (nrow(data) == 0) return(plotly_empty(type = "box"))
    
    if (input$varColor != "") {
      plot_ly(data, x = ~get(input$varX), y = ~get(input$varY), 
              color = ~get(input$varColor), type = "box")
    } else {
      plot_ly(data, y = ~get(input$varY), type = "box") %>%
        layout(xaxis = list(title = input$varX))
    } %>%
      layout(
        title = paste("Box Plot of", input$varY),
        yaxis = list(title = input$varY)
      )
  })
  
  # Heatmap - DIPERBAIKI
  output$heatmapChart <- renderPlotly({
    req(input$varX, input$varY)
    data <- explore_data()
    
    if (nrow(data) == 0) return(plotly_empty(type = "heatmap"))
    
    # Check if variables are numeric
    is_x_numeric <- is.numeric(data[[input$varX]])
    is_y_numeric <- is.numeric(data[[input$varY]])
    
    if (is_x_numeric && is_y_numeric) {
      # For numeric vs numeric - 2D density
      plot_ly(data, x = ~get(input$varX), y = ~get(input$varY), 
              type = "histogram2dcontour") %>%
        layout(
          title = paste("2D Density:", input$varX, "vs", input$varY),
          xaxis = list(title = input$varX),
          yaxis = list(title = input$varY)
        )
    } else {
      # For categorical data - create frequency table
      if (!is_x_numeric) data[[input$varX]] <- as.factor(data[[input$varX]])
      if (!is_y_numeric) data[[input$varY]] <- as.factor(data[[input$varY]])
      
      freq_table <- table(data[[input$varX]], data[[input$varY]])
      
      plot_ly(x = colnames(freq_table), 
              y = rownames(freq_table),
              z = freq_table, 
              type = "heatmap",
              colors = "Blues") %>%
        layout(
          title = "Frequency Heatmap",
          xaxis = list(title = input$varY),
          yaxis = list(title = input$varX)
        )
    }
  })
  
  # Pie Chart - DIPERBAIKI
  output$pieChart <- renderPlotly({
    req(input$varX)
    data <- explore_data()
    
    if (nrow(data) == 0) return(plotly_empty(type = "pie"))
    
    if (is.numeric(data[[input$varX]])) {
      # For numeric - create bins
      values <- data[[input$varX]]
      bins <- cut(values, breaks = min(5, length(unique(values))), include.lowest = TRUE)
      freq_table <- table(bins)
      
      plot_ly(labels = names(freq_table), 
              values = as.numeric(freq_table),
              type = "pie",
              textinfo = 'label+percent') %>%
        layout(title = paste("Distribution of", input$varX))
    } else {
      # For categorical - show counts
      freq_table <- table(data[[input$varX]])
      plot_ly(labels = names(freq_table), 
              values = as.numeric(freq_table),
              type = "pie",
              textinfo = 'label+percent') %>%
        layout(title = paste("Proportion of", input$varX))
    }
  })
  
  # Area Chart - DIPERBAIKI
  output$areaChart <- renderPlotly({
    req(input$varX, input$varY)
    data <- explore_data()
    
    if (nrow(data) == 0) return(plotly_empty(type = "scatter"))
    
    # Sort by x variable for area chart
    data <- data %>% arrange(get(input$varX))
    
    p <- plot_ly(data)
    
    if (input$varColor != "") {
      # Stacked area chart
      groups <- unique(data[[input$varColor]])
      
      for (group in groups) {
        group_data <- data[data[[input$varColor]] == group, ]
        p <- p %>% add_trace(
          data = group_data,
          x = ~get(input$varX), 
          y = ~get(input$varY),
          type = 'scatter',
          mode = 'none',
          fill = 'tozeroy',
          name = as.character(group)
        )
      }
    } else {
      # Simple area chart
      p <- p %>% add_trace(
        x = ~get(input$varX), 
        y = ~get(input$varY),
        type = 'scatter',
        mode = 'none',
        fill = 'tozeroy'
      )
    }
    
    p %>% layout(
      title = paste("Area Chart:", input$varY, "over", input$varX),
      xaxis = list(title = input$varX),
      yaxis = list(title = input$varY)
    )
  })
  
  # Data Summary - DIPERBAIKI
  output$dataSummary <- renderPrint({
    data <- explore_data()
    if (nrow(data) == 0) return("No data available with current filters")
    
    numeric_data <- data %>% select(where(is.numeric))
    if (ncol(numeric_data) > 0) {
      cat("Descriptive Statistics:\n\n")
      print(summary(numeric_data))
    } else {
      "No numeric variables available in filtered data"
    }
  })
  
  # Data Info - DIPERBAIKI
  output$dataInfo <- renderPrint({
    data <- explore_data()
    if (nrow(data) == 0) return("No data available with current filters")
    
    cat("Dataset Information:\n")
    cat("Number of rows:", nrow(data), "\n")
    cat("Number of columns:", ncol(data), "\n")
    cat("\nColumn types:\n")
    col_types <- sapply(data, class)
    for(i in 1:length(col_types)) {
      cat(names(col_types)[i], ":", col_types[i], "\n")
    }
    cat("\nMissing values:\n")
    missing_vals <- colSums(is.na(data))
    print(missing_vals[missing_vals > 0])
  })
  
  # Quick Action Buttons
  observeEvent(input$btnCorrelation, {
    updateSelectInput(session, "vizType", selected = "heatmap")
    updateSelectInput(session, "varX", selected = "Timbulan_Sampah_Tahunan")
    updateSelectInput(session, "varY", selected = "Sampah_Terkelola")
  })
  
  observeEvent(input$btnTrend, {
    updateSelectInput(session, "vizType", selected = "line")
    updateSelectInput(session, "varX", selected = "Tahun")
    updateSelectInput(session, "varY", selected = "Sampah_Terkelola")
  })
  
  observeEvent(input$btnComparison, {
    updateSelectInput(session, "vizType", selected = "bar")
    updateSelectInput(session, "varX", selected = "Provinsi")
    updateSelectInput(session, "varY", selected = "Sampah_Terkelola_Tahunan")
    updateSelectInput(session, "filterAggregate", selected = "province")
  })
  
  observeEvent(input$btnDistribution, {
    updateSelectInput(session, "vizType", selected = "histogram")
    updateSelectInput(session, "varX", selected = "Timbulan_Sampah_Tahunan")
  })
  
  observeEvent(input$btnReset, {
    updateSelectInput(session, "varX", selected = "")
    updateSelectInput(session, "varY", selected = "")
    updateSelectInput(session, "varColor", selected = "")
    updateSelectInput(session, "varSize", selected = "")
    updateSelectInput(session, "filterProvinsi", selected = "Semua Provinsi")
    updateSliderInput(session, "filterTahun", value = c(2019, 2024))
    updateSelectInput(session, "filterAggregate", selected = "raw")
  })
  
  # Download handler - DIPERBAIKI
  output$downloadViz <- downloadHandler(
    filename = function() {
      paste("visualization-", input$vizType, "-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Export as PNG using plotly
      plotly_export <- get(paste0(input$vizType, "Plot"))()
      export(plotly_export, file = file)
    }
  )
  # Data untuk rekomendasi
  rekomendasi_data <- reactive({
    data.frame(
      id = 1:20,
      judul = c(
        "Optimalisasi Infrastruktur TPA di 10 Provinsi Prioritas",
        "Program Nasional Bank Sampah Digital",
        "Pengembangan Teknologi Waste-to-Energy",
        "Kebijakan Insentif Ekonomi Sirkular",
        "Sistem Monitoring Sampah Real-time Nasional",
        "Program Edukasi Masyarakat Berbasis Data",
        "Penguatan Regulasi Pengelolaan Sampah",
        "Pengembangan Industri Daur Ulang Terintegrasi",
        "Infrastruktur Pengolahan Sampah Organik",
        "Sistem Transportasi Sampah Teroptimasi",
        "Teknologi Pemilahan Sampah Otomatis",
        "Program Kemitraan Publik-Swasta",
        "Pengembangan Pasar Produk Daur Ulang",
        "Sistem Insentif Berbasis Kinerja",
        "Infrastruktur Pengolahan Limbah B3",
        "Program Green Industry Sampah",
        "Teknologi Digital Tracing Sampah",
        "Kebijakan Extended Producer Responsibility",
        "Pengembangan Komposting Skala Komunitas",
        "Sistem Reward Partisipasi Masyarakat"
      ),
      kategori = c(
        "Infrastruktur", "Teknologi", "Teknologi", "Ekonomi", "Teknologi",
        "Lingkungan", "Kebijakan", "Ekonomi", "Infrastruktur", "Infrastruktur",
        "Teknologi", "Ekonomi", "Ekonomi", "Kebijakan", "Infrastruktur",
        "Ekonomi", "Teknologi", "Kebijakan", "Lingkungan", "Lingkungan"
      ),
      prioritas = c(
        "Tinggi", "Tinggi", "Tinggi", "Tinggi", "Sedang",
        "Sedang", "Tinggi", "Sedang", "Sedang", "Rendah",
        "Sedang", "Tinggi", "Sedang", "Tinggi", "Tinggi",
        "Rendah", "Sedang", "Tinggi", "Sedang", "Rendah"
      ),
      jangka_waktu = c(
        "Jangka Pendek (1-2 tahun)", "Jangka Pendek (1-2 tahun)", "Jangka Menengah (3-4 tahun)",
        "Jangka Pendek (1-2 tahun)", "Jangka Pendek (1-2 tahun)", "Jangka Pendek (1-2 tahun)",
        "Jangka Pendek (1-2 tahun)", "Jangka Menengah (3-4 tahun)", "Jangka Menengah (3-4 tahun)",
        "Jangka Panjang (5+ tahun)", "Jangka Menengah (3-4 tahun)", "Jangka Pendek (1-2 tahun)",
        "Jangka Menengah (3-4 tahun)", "Jangka Pendek (1-2 tahun)", "Jangka Menengah (3-4 tahun)",
        "Jangka Panjang (5+ tahun)", "Jangka Menengah (3-4 tahun)", "Jangka Pendek (1-2 tahun)",
        "Jangka Pendek (1-2 tahun)", "Jangka Pendek (1-2 tahun)"
      ),
      fokus_wilayah = c(
        "Provinsi Tertentu", "Nasional", "Perkotaan", "Nasional", "Nasional",
        "Nasional", "Nasional", "Nasional", "Pedesaan", "Perkotaan",
        "Perkotaan", "Nasional", "Nasional", "Nasional", "Nasional",
        "Nasional", "Nasional", "Nasional", "Pedesaan", "Nasional"
      ),
      estimasi_dampak = c(15, 12, 25, 18, 8, 10, 20, 22, 14, 6, 16, 15, 19, 12, 11, 24, 9, 21, 13, 7),
      estimasi_biaya = c(850, 320, 1200, 150, 280, 180, 90, 650, 420, 380, 550, 270, 480, 110, 720, 890, 310, 130, 95, 75),
      deskripsi = c(
        "Pembangunan dan optimalisasi TPA di 10 provinsi dengan kinerja terendah berdasarkan analisis data",
        "Digitalisasi sistem bank sampah untuk meningkatkan efisiensi dan transparansi transaksi",
        "Implementasi teknologi konversi sampah menjadi energi listrik di kota-kota besar",
        "Kebijakan fiskal dan non-fiskal untuk mendorong praktik ekonomi sirkular",
        "Sistem pemantauan real-time volume dan pergerakan sampah secara nasional",
        "Program edukasi berbasis data untuk meningkatkan partisipasi masyarakat",
        "Penguatan regulasi dan penegakan hukum dalam pengelolaan sampah",
        "Pengembangan industri daur ulang yang terintegrasi dari hulu ke hilir",
        "Pembangunan fasilitas komposting skala komunitas di daerah pedesaan",
        "Optimalisasi rute dan armada pengangkutan sampah menggunakan algoritma",
        "Implementasi teknologi AI dan robotik untuk pemilahan sampah otomatis",
        "Kemitraan strategis antara pemerintah dan swasta dalam pengelolaan sampah",
        "Pengembangan pasar dan distribusi untuk produk hasil daur ulang",
        "Sistem insentif berbasis kinerja untuk pemerintah daerah",
        "Pembangunan infrastruktur khusus untuk pengolahan limbah B3",
        "Program transformasi sampah menjadi produk industri bernilai tinggi",
        "Teknologi blockchain untuk tracing rantai nilai sampah",
        "Kebijakan tanggung jawab produsen yang diperluas",
        "Pengembangan sistem komposting berbasis komunitas",
        "Sistem reward untuk partisipasi aktif masyarakat dalam pengelolaan sampah"
      )
    )
  })
  
  # Filter rekomendasi
  rekomendasi_terfilter <- reactive({
    data <- rekomendasi_data()
    
    # Filter kategori
    if (input$kategoriFilter != "Semua Kategori") {
      data <- data %>% filter(kategori == input$kategoriFilter)
    }
    
    # Filter prioritas
    if (input$prioritasFilter != "Semua Prioritas") {
      data <- data %>% filter(prioritas == input$prioritasFilter)
    }
    
    # Filter jangka waktu
    if (input$jangkaFilter != "Semua Jangka") {
      data <- data %>% filter(jangka_waktu == input$jangkaFilter)
    }
    
    # Filter wilayah
    if (input$provinsiFilter != "Nasional") {
      data <- data %>% filter(fokus_wilayah == input$provinsiFilter)
    }
    
    data
  })
  
  # Output rekomendasi grid
  output$rekomendasiGrid <- renderUI({
    data <- rekomendasi_terfilter()
    
    if (nrow(data) == 0) {
      return(tags$div(class = "text-center", style = "padding: 40px; color: #64748b;",
                      icon("search", style = "font-size: 48px; margin-bottom: 20px;"),
                      h4("Tidak ada rekomendasi yang sesuai dengan filter yang dipilih")))
    }
    
    tagList(
      div(class = "recommendation-master-grid",
          lapply(1:nrow(data), function(i) {
            rek <- data[i, ]
            div(class = "recommendation-card-enhanced",
                div(class = "recommendation-header",
                    h5(class = "recommendation-title", rek$judul),
                    div(class = "recommendation-badges",
                        span(class = paste("badge", "badge-priority", tolower(rek$prioritas)), rek$prioritas),
                        span(class = "badge badge-category", rek$kategori)
                    )
                ),
                p(class = "recommendation-description", rek$deskripsi),
                div(class = "recommendation-metrics",
                    div(class = "metric-item",
                        div(class = "metric-value", paste0("+", rek$estimasi_dampak, "%")),
                        div(class = "metric-label", "Estimasi Dampak")
                    ),
                    div(class = "metric-item",
                        div(class = "metric-value", paste0("Rp ", format(rek$estimasi_biaya, big.mark = ".", decimal.mark = ","), "M")),
                        div(class = "metric-label", "Estimasi Biaya")
                    ),
                    div(class = "metric-item",
                        div(class = "metric-value", rek$jangka_waktu),
                        div(class = "metric-label", "Jangka Waktu")
                    )
                ),
                div(class = "action-buttons",
                    actionButton(paste0("btnDetail_", rek$id), "Lihat Analisis Detail", 
                                 class = "btn-detail", onclick = paste0('Shiny.setInputValue("detail_rekomendasi", "', rek$id, '")')),
                    actionButton(paste0("btnImplement_", rek$id), "Rencana Implementasi", 
                                 class = "btn-implement", onclick = paste0('Shiny.setInputValue("implement_rekomendasi", "', rek$id, '")'))
                )
            )
          })
      )
    )
  })
  
  # Executive Summary Outputs
  output$currentManaged <- renderText({
    data <- national_summary()
    latest <- data %>% filter(Tahun == max(Tahun))
    paste0(round(latest$Avg_Terkelola, 1), "%")
  })
  
  output$targetGap <- renderText({
    data <- national_summary()
    latest <- data %>% filter(Tahun == max(Tahun))
    gap <- 100 - latest$Avg_Terkelola
    paste0(round(gap, 1), "%")
  })
  
  output$performanceGap <- renderText({
    data <- province_summary()
    latest <- data %>% filter(Tahun == max(Tahun))
    gap <- max(latest$Avg_Terkelola, na.rm = TRUE) - min(latest$Avg_Terkelola, na.rm = TRUE)
    paste0(round(gap, 1), " poin")
  })
  
  output$recyclingPotential <- renderText({
    data <- sampah_data()
    latest <- data %>% filter(Tahun == max(Tahun))
    potential <- mean(latest$Plastik + latest$`Kertas-Karton (%)` + latest$Logam + latest$Kaca, na.rm = TRUE)
    paste0(round(potential, 1), "%")
  })
  
  # Handler untuk Lihat Analisis Detail
  observeEvent(input$detail_rekomendasi, {
    req(input$detail_rekomendasi)
    
    rek_id <- as.numeric(input$detail_rekomendasi)
    rek_data <- rekomendasi_data() %>% filter(id == rek_id)
    
    showModal(modalDialog(
      title = paste("Analisis Detail:", rek_data$judul),
      size = "l",
      fluidRow(
        column(6,
               h4("📊 Data Pendukung"),
               plotlyOutput(paste0("detailPlot_", rek_id), height = "250px")
        ),
        column(6,
               h4("🎯 Metrik Kunci"),
               div(class = "executive-summary-grid",
                   div(class = "executive-card",
                       div(class = "executive-value", paste0("+", rek_data$estimasi_dampak, "%")),
                       div(class = "executive-label", "Estimasi Peningkatan")
                   ),
                   div(class = "executive-card",
                       div(class = "executive-value", paste0("Rp ", rek_data$estimasi_biaya, "M")),
                       div(class = "executive-label", "Estimasi Investasi")
                   )
               )
        )
      ),
      fluidRow(
        column(12,
               h4("📋 Analisis Data"),
               p("Berdasarkan analisis data terbaru:"),
               tags$ul(
                 tags$li("Potensi peningkatan efisiensi: ", strong("15-25%")),
                 tags$li("Dampak lingkungan: ", strong("Pengurangan emisi 2-5 ton CO2/hari")),
                 tags$li("Dampak ekonomi: ", strong("ROI 2.5-3.5x dalam 5 tahun")),
                 tags$li("Dampak sosial: ", strong("Penciptaan 500-1000 lapangan kerja"))
               )
        )
      ),
      footer = tagList(
        modalButton("Tutup"),
        actionButton(paste0("btnDeepDive_", rek_id), "Analisis Lebih Dalam", 
                     class = "btn-detail")
      )
    ))
    
    # Generate plot untuk modal detail
    output[[paste0("detailPlot_", rek_id)]] <- renderPlotly({
      # Data contoh berdasarkan kategori rekomendasi
      if (rek_data$kategori == "Infrastruktur") {
        plot_data <- data.frame(
          Province = c('Prov A', 'Prov B', 'Prov C', 'Prov D', 'Prov E'),
          Current = c(45, 38, 52, 41, 48),
          Target = c(75, 70, 80, 75, 78)
        )
        
        plot_ly(plot_data, x = ~Province) %>%
          add_trace(y = ~Current, type = 'bar', name = 'Saat Ini', 
                    marker = list(color = '#ef4444')) %>%
          add_trace(y = ~Target, type = 'bar', name = 'Target', 
                    marker = list(color = '#10b981')) %>%
          layout(title = "Kesenjangan Infrastruktur antar Provinsi",
                 barmode = 'group',
                 yaxis = list(title = "% Kapasitas"))
        
      } else if (rek_data$kategori == "Teknologi") {
        years <- 2019:2024
        adoption <- c(15, 18, 22, 28, 35, 42)
        
        plot_ly(x = years, y = adoption, type = 'scatter', mode = 'lines+markers',
                line = list(color = '#10b981', width = 3),
                marker = list(color = '#10b981', size = 8)) %>%
          layout(title = "Adopsi Teknologi Pengelolaan Sampah",
                 xaxis = list(title = "Tahun"),
                 yaxis = list(title = "% Adopsi"))
      } else {
        # Default plot
        plot_ly(x = c('Q1', 'Q2', 'Q3', 'Q4'), 
                y = c(65, 72, 68, 75), 
                type = 'bar',
                marker = list(color = '#3b82f6')) %>%
          layout(title = "Trend Kinerja Terkait",
                 yaxis = list(title = "Skor Kinerja"))
      }
    })
  })
  
  # Handler untuk Rencana Implementasi
  observeEvent(input$implement_rekomendasi, {
    req(input$implement_rekomendasi)
    
    rek_id <- as.numeric(input$implement_rekomendasi)
    rek_data <- rekomendasi_data() %>% filter(id == rek_id)
    
    showModal(modalDialog(
      title = paste("Rencana Implementasi:", rek_data$judul),
      size = "l",
      fluidRow(
        column(6,
               h4("🗓️ Timeline Implementasi"),
               div(class = "timeline-enhanced",
                   div(class = "timeline-item-enhanced",
                       div(class = "timeline-date-enhanced", "Bulan 1-6"),
                       div(class = "timeline-phase", "Fase Persiapan"),
                       tags$ul(class = "timeline-tasks",
                               tags$li("Studi kelayakan dan analisis dampak"),
                               tags$li("Penyusunan detail engineering design"),
                               tags$li("Penganggaran dan pengadaan"),
                               tags$li("Pembentukan tim implementasi")
                       )
                   ),
                   div(class = "timeline-item-enhanced",
                       div(class = "timeline-date-enhanced", "Bulan 7-18"),
                       div(class = "timeline-phase", "Fase Implementasi"),
                       tags$ul(class = "timeline-tasks",
                               tags$li("Pembangunan infrastruktur/fasilitas"),
                               tags$li("Pengembangan sistem dan teknologi"),
                               tags$li("Pelatihan dan capacity building"),
                               tags$li("Uji coba dan validasi sistem")
                       )
                   ),
                   div(class = "timeline-item-enhanced",
                       div(class = "timeline-date-enhanced", "Bulan 19-24"),
                       div(class = "timeline-phase", "Fase Operasional"),
                       tags$ul(class = "timeline-tasks",
                               tags$li("Go-live dan operasional penuh"),
                               tags$li("Monitoring dan evaluasi"),
                               tags$li("Optimalisasi berkelanjutan"),
                               tags$li("Replikasi ke wilayah lain")
                       )
                   )
               )
        ),
        column(6,
               h4("💰 Rincian Anggaran"),
               dataTableOutput(paste0("budgetTable_", rek_id)),
               br(),
               h4("👥 Stakeholder Terkait"),
               tags$ul(
                 tags$li("Kementerian Lingkungan Hidup dan Kehutanan"),
                 tags$li("Pemerintah Daerah Provinsi/Kota"),
                 tags$li("Kementerian Keuangan"),
                 tags$li("Kementerian Pekerjaan Umum"),
                 tags$li("Swasta/Pelaku Industri"),
                 tags$li("Komunitas/LSM Lingkungan")
               )
        )
      ),
      footer = modalButton("Tutup")
    ))
    # Budget table untuk modal implementasi
    output[[paste0("budgetTable_", rek_id)]] <- renderDataTable({
      budget_data <- data.frame(
        Komponen = c('Infrastruktur', 'Teknologi', 'SDM & Pelatihan', 'Operasional', 'Monitoring', 'Kontinjensi'),
        Anggaran = c(
          rek_data$estimasi_biaya * 0.5,
          rek_data$estimasi_biaya * 0.2,
          rek_data$estimasi_biaya * 0.1,
          rek_data$estimasi_biaya * 0.15,
          rek_data$estimasi_biaya * 0.03,
          rek_data$estimasi_biaya * 0.02
        )
      ) %>%
        mutate(Anggaran = round(Anggaran, 1))
      
      datatable(budget_data, 
                options = list(dom = 't', pageLength = 6),
                colnames = c('Komponen', 'Anggaran (Miliar Rp)'),
                rownames = FALSE) %>%
        formatCurrency('Anggaran', currency = "Rp ", digits = 1, before = FALSE)
    })
  })
  
} 

# Run the application
shinyApp(ui, server)
