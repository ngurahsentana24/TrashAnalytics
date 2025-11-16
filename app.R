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
      menuItem("Rencana Aksi", tabName = "rencana", icon = icon("tasks")),
      menuItem("Tim Pengembang", tabName = "team", icon = icon("users")),
      
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
    )
  ),
  
  dashboardBody(
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
          column(
            width = 12,
            box(
              width = 12,
              status = "success",
              solidHeader = TRUE,
              title = "Sistem Bank Sampah",
              tags$div(
                style = "text-align: center; padding: 50px;",
                tags$h3(style = "color: #10b981; margin-bottom: 20px; font-weight: 700;", 
                        "Fitur Dalam Pengembangan"),
                tags$p(style = "font-size: 16px; color: #64748b;", 
                       "Kami sedang menyusun sistem monitoring dan analisis bank sampah yang komprehensif."),
                tags$div(
                  style = "font-size: 48px; color: #10b981; margin: 30px 0;",
                  icon("piggy-bank", class = "fa-3x")
                ),
                tags$p(style = "color: #94a3b8; font-size: 14px;", 
                       "Fitur ini akan segera hadir dengan analisis transaksi, volume sampah terkelola, dan dampak ekonomi.")
              )
            )
          )
        )
      ),
      
      # Tab Rencana Aksi ----
      tabItem(
        tabName = "rencana",
        fluidRow(
          column(
            width = 12,
            box(
              width = 12,
              status = "success",
              solidHeader = TRUE,
              title = "Rencana Aksi & Rekomendasi Strategis",
              tags$div(
                style = "text-align: center; padding: 50px;",
                tags$h3(style = "color: #10b981; margin-bottom: 20px; font-weight: 700;", 
                        "Fitur Dalam Pengembangan"),
                tags$p(style = "font-size: 16px; color: #64748b;", 
                       "Kami sedang menyusun rekomendasi berbasis data yang komprehensif untuk mendukung pengambilan keputusan strategis."),
                tags$div(
                  style = "font-size: 48px; color: #10b981; margin: 30px 0;",
                  icon("cogs", class = "fa-3x")
                ),
                tags$p(style = "color: #94a3b8; font-size: 14px;", 
                       "Fitur ini akan segera hadir dengan analisis mendalam dan rekomendasi terukur.")
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
}

# Run the application
shinyApp(ui, server)