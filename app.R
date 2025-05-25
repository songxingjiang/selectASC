library(shiny)
library(fuzzySim)
library(terra)
library(DT)
library(corrplot)

options(shiny.maxRequestSize = 10000 * 1024^2)

ui <- fluidPage(
  titlePanel("环境变量筛选（corSelect 基于 ASC 栅格文件）"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput(
        inputId = "asc_upload",
        label = "上传 ASC 栅格文件（可多选）",
        multiple = TRUE,
        accept = ".asc"
      ),
      
      uiOutput("var_selector"),
      
      numericInput(
        inputId = "cor_thresh",
        label = "相关性阈值 (cor.thresh)",
        value = 0.8,
        min = 0.5,
        max = 0.99,
        step = 0.01
      ),
      
      selectInput(
        inputId = "select_method",
        label = "筛选方法",
        choices = c("VIF", "AIC", "random")
      ),
      
      actionButton(
        inputId = "run_analysis",
        label = "运行变量筛选"
      )
    ),
    
    mainPanel(
      h4("✅ 保留变量"),
      DTOutput("var_table"),
      
      h4("📋 分析摘要"),
      verbatimTextOutput("summary_out"),
      
      h4("📈 高相关变量对"),
      verbatimTextOutput("cor_pairs"),
      
      h4("📊 初始变量相关性图"),
      plotOutput("cor_before"),
      
      h4("📊 筛选后变量相关性图"),
      plotOutput("cor_after")
    )
  )
)

server <- function(input, output, session) {
  asc_data <- reactiveVal(NULL)
  
  observeEvent(input$asc_upload, {
    req(input$asc_upload)
    
    withProgress(message = "正在处理 ASC 栅格文件...", value = 0, {
      incProgress(0.1, detail = "读取栅格文件")
      files <- input$asc_upload$datapath
      filenames <- tools::file_path_sans_ext(basename(input$asc_upload$name))
      
      rasters <- tryCatch(
        terra::rast(files),
        error = function(e) {
          showNotification("❌ 无法读取 ASC 栅格，请检查文件格式。", type = "error")
          return(NULL)
        }
      )
      
      if (is.null(rasters)) return()
      names(rasters) <- filenames
      
      incProgress(0.3, detail = "转换为数据框")
      df <- tryCatch(
        terra::as.data.frame(rasters, na.rm = TRUE, xy = FALSE),
        error = function(e) {
          showNotification("❌ 无法转换为数据框，请检查栅格内容。", type = "error")
          return(NULL)
        }
      )
      
      incProgress(0.5, detail = "清洗无效变量")
      df <- df[, sapply(df, function(x) {
        is.numeric(x) && !all(is.na(x)) && sd(x, na.rm = TRUE) != 0
      })]
      
      if (ncol(df) == 0) {
        showNotification("⚠️ 所有变量都被剔除，可能是全 NA 或无方差。", type = "error")
        return()
      }
      
      incProgress(1, detail = "完成")
      asc_data(df)
    })
  })
  
  output$var_selector <- renderUI({
    df <- asc_data()
    req(df)
    
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    
    if (length(numeric_vars) == 0) {
      return(h4("⚠️ 未找到任何数值型变量"))
    }
    
    checkboxGroupInput(
      inputId = "var_cols",
      label = "选择用于分析的变量（可多选）",
      choices = numeric_vars,
      selected = numeric_vars
    )
  })
  
  result <- eventReactive(input$run_analysis, {
    df <- asc_data()
    req(df, input$var_cols)
    
    withProgress(message = "正在运行 corSelect 分析...", value = 0, {
      incProgress(0.3, detail = "正在计算变量相关性")
      res <- tryCatch(
        fuzzySim::corSelect(
          data = df,
          var.cols = input$var_cols,
          cor.thresh = input$cor_thresh,
          select = input$select_method
        ),
        error = function(e) {
          showNotification("❌ corSelect 执行失败，请检查输入变量。", type = "error")
          return(NULL)
        }
      )
      
      incProgress(1, detail = "分析完成")
      res
    })
  })
  
  output$var_table <- renderDT({
    res <- result()
    req(res)
    datatable(data.frame(保留变量 = res$selected.vars), rownames = FALSE)
  })
  
  output$summary_out <- renderPrint({
    res <- result()
    req(res)
    
    selected <- res$selected.vars
    all_vars <- input$var_cols
    removed <- setdiff(all_vars, selected)
    
    cat("📦 初始变量数量：", length(all_vars), "\n")
    cat("✅ 保留变量数量：", length(selected), "\n")
    cat("❌ 被剔除变量数量：", length(removed), "\n\n")
    
    cat("✅ 保留变量：\n")
    print(selected)
    
    cat("\n❌ 被剔除变量：\n")
    print(removed)
  })
  
  output$cor_pairs <- renderPrint({
    res <- result()
    req(res)
    print(res$high.correlations)
  })
  
  output$cor_before <- renderPlot({
    df <- asc_data()
    req(df, input$var_cols)
    
    cor_mat <- cor(df[, input$var_cols, drop = FALSE], use = "complete.obs")
    corrplot::corrplot(cor_mat, method = "color", type = "upper", tl.cex = 0.8)
  })
  
  output$cor_after <- renderPlot({
    res <- result()
    req(res)
    df <- asc_data()
    selected_vars <- res$selected.vars
    
    cor_mat <- cor(df[, selected_vars, drop = FALSE], use = "complete.obs")
    corrplot::corrplot(cor_mat, method = "color", type = "upper", tl.cex = 0.8)
  })
}

shinyApp(ui = ui, server = server)
