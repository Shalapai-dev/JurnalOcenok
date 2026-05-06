library(shiny)
library(bslib)
library(DT)
library(readxl)
library(writexl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(fontawesome)
library(stringi)


my_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#0d6efd",
  secondary = "#10ac8d",
  success = "#9ec0a6",
  danger = "#dc3545",
)


ui <- page_navbar(
  title = tags$span(
    icon("book-open", class = "me-2"),
    "Журнал оценок"
  ),
  theme = my_theme,
  

  nav_panel(
    title = tagList(icon("upload"), "Загрузка оценок"),
    value = "load",
    
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Загрузка файла"),
        fileInput("file", NULL, 
                  accept = c(".csv", ".txt", ".xlsx", ".xls"),
                  buttonLabel = "Выбрать файл",
                  placeholder = "Выберите файл..."),
        radioButtons("sep", "Разделитель для CSV/TXT:",
                     choices = c("Точка с запятой (;)" = ";", 
                                 "Запятая (,)" = ","),
                     inline = TRUE,
                     selected = ";"),
        actionButton("load_btn", "Загрузить данные", 
                     icon = icon("upload"), 
                     class = "btn-success w-100 mt-3")
      ),
      card(
        card_header("Просмотр загруженных данных"),
        DTOutput("loaded_table")
      )
    )
  ),
  

  nav_panel(
    title = tagList(icon("table"), "Журнал оценок"),
    value = "journal",
    
    card(
      card_header(
        div(class = "d-flex justify-content-between align-items-center",
          "Журнал оценок",
          div(class = "d-flex gap-2",
            actionButton("add_row_btn", "Добавить ученика", 
                         icon = icon("plus"), class = "btn-sm btn-primary"),
            actionButton("delete_row_btn", "Удалить строку", 
                         icon = icon("trash"), class = "btn-sm btn-danger")
          )
        )
      ),
      DTOutput("journal_table")
    ),
    
    card(
      card_header("Экспорт данных"),
      div(class = "d-flex flex-wrap gap-2",
        downloadButton("save_csv", "Сохранить как CSV", class = "btn btn-outline-primary"),
        downloadButton("save_xlsx", "Сохранить как Excel", class = "btn btn-outline-success")
      )
    )
  ),
  

  nav_panel(
    title = tagList(icon("chart-bar"), "Статистика"),
    value = "stats",
    
    layout_column_wrap(
      width = 1/3,
      value_box(
        title = "Всего учеников",
        value = textOutput("total_students"),
        icon = icon("users"),
        theme_color = "primary"
      ),
      value_box(
        title = "Средняя оценка",
        value = textOutput("avg_mark"),
        icon = icon("graduation-cap"),
        theme_color = "success"
      ),
      value_box(
        title = "Количество предметов",
        value = textOutput("subjects_count"),
        icon = icon("book"),
        theme_color = "info"
      )
    ),
    
    card(
      card_header("Статистика по классам и предметам"),
      DTOutput("stat_class_subject")
    ),
    
    card(
      card_header("Общая статистика по предметам (все классы)"),
      DTOutput("stat_overall")
    )
  ),
  

  nav_panel(
    title = tagList(icon("chart-line"), "Графики"),
    value = "graphs",
    
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Средняя оценка по предметам"),
        plotOutput("plot_mean", height = "420px")
      ),
      card(
        card_header("Распределение оценок по предметам"),
        plotOutput("plot_dist", height = "420px")
      )
    ),
    
    card(
      card_header("Boxplot оценок по предметам"),
      plotOutput("plot_boxplot", height = "420px")
    )
  ),
  

  nav_panel(
    title = tagList(icon("question-circle"), "Помощь"),
    value = "help",
    card(
      card_header("Как пользоваться приложением"),
      tags$ul(
        tags$li("Загрузка оценок — поддерживает файлы CSV, TXT и Excel"),
        tags$li("Журнал оценок — добавляйте новых учеников, редактируйте (клик по строке) и удаляйте записи"),
        tags$li("Все изменения сразу отражаются в статистике и графиках"),
        tags$li("Не забудьте сохранить журнал перед закрытием приложения"),
        tags$li("Данные хранятся только в браузере до закрытия вкладки")
      )
    )
  ),
  

  nav_panel(
    title = tagList(icon("info-circle"), "О программе"),
    value = "about",
    card(
      card_header("О приложении"),
      p("Современное веб-приложение для ведения журнала оценок учеников."),
      h5("Разработчик:"),
      p("Остренко Иван Андреевич"),
      p("Группа: БС-304"),
      p("Контакты: ostrenko_05@list.ru | Telegram: @ostrenkoib"),
      br(),
      img(src = "https://via.placeholder.com/280x350?text=Ваше+фото", 
          height = "320px", style = "border-radius: 12px; box-shadow: 0 4px 15px rgba(0,0,0,0.1);"),
      p("© 2026 Все права защищены", class = "text-muted mt-4")
    )
  )
)

server <- function(input, output, session) {
  
  journal <- reactiveValues(
    df = data.frame(
      ФИО = character(),
      Класс = character(),
      Предмет = character(),
      Оценка = numeric(),
      stringsAsFactors = FALSE
    )
  )
  

preview_data <- reactiveVal(NULL)

observeEvent(input$load_btn, {
  req(input$file)
  tryCatch({

    ext <- tolower(tools::file_ext(input$file$name))
    data <- NULL
    

    if (ext %in% c("csv", "txt")) {
      data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    } else if (ext %in% c("xlsx", "xls")) {
      data <- as.data.frame(readxl::read_excel(input$file$datapath))
    } else {

      data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    }
    

    data <- as.data.frame(data)
    data[] <- lapply(data, function(x) {
      if(is.character(x)) {
        stringi::stri_enc_toutf8(x, validate = TRUE)
      } else x
    })
    
    preview_data(data) 
    journal$df <- rbind(journal$df, data)
    showNotification(paste("✅ Данные успешно загружены! Строк:", nrow(data)), type = "message", duration = 5)
    
  }, error = function(e) {
    showNotification(paste("Ошибка чтения файла:", e$message), type = "error")
  })
})


output$loaded_table <- renderDT({
  req(preview_data())
  datatable(preview_data(), options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
})
  

  output$journal_table <- renderDT({
    datatable(journal$df, 
              selection = "single",
              rownames = FALSE,
              editable = TRUE,
              options = list(scrollX = TRUE, pageLength = 12,
                           dom = 'ltipr'))
  })
  

  observeEvent(input$journal_table_cell_edit, {
  info <- input$journal_table_cell_edit
  i <- info$row
  j <- info$col + 1
  v <- info$value
  
  if (j == 4) { 
    v <- suppressWarnings(as.numeric(v))
    if (is.na(v) || !(v %in% 1:5)) {
      showNotification(" Оценка должна быть целым числом от 1 до 5", type = "warning")
      return() 
    }
  }
  
  journal$df[i, j] <- v 
  showNotification(" Запись обновлена", type = "message", duration = 2)
})
  
 
  observeEvent(input$add_row_btn, {
    new_row <- data.frame(
      ФИО = "Новый ученик",
      Класс = "",
      Предмет = "",
      Оценка = 3,
      stringsAsFactors = FALSE
    )
    journal$df <- rbind(journal$df, new_row)
    showNotification("Новая строка добавлена. Отредактируйте данные в таблице", type = "message")
  })
  

  observeEvent(input$delete_row_btn, {
    selected <- input$journal_table_rows_selected
    if (!is.null(selected) && length(selected) > 0) {
      journal$df <- journal$df[-selected, , drop = FALSE]
      showNotification("Строка удалена", type = "warning")
    } else {
      showNotification("Выберите строку для удаления", type = "error")
    }
  })
  

  output$save_csv <- downloadHandler(
    filename = function() { paste0("journal_", Sys.Date(), ".csv") },
    content = function(file) {
      tryCatch({
        if (nrow(journal$df) == 0) {
          showNotification("Нет данных для экспорта", type = "warning")
          return()
        }

        write.csv(journal$df, file = file, row.names = FALSE, fileEncoding = "UTF-8")
        showNotification("CSV файл успешно сохранен", type = "message")
      }, error = function(e) {
        showNotification(paste("Ошибка при сохранении CSV:", e$message), type = "error")
      })
    },
    contentType = "text/csv;charset=UTF-8"
  )
  
  output$save_xlsx <- downloadHandler(
    filename = function() { paste0("journal_", Sys.Date(), ".xlsx") },
    content = function(file) {
      tryCatch({
        if (nrow(journal$df) == 0) {
          showNotification("Нет данных для экспорта", type = "warning")
          return()
        }

        writexl::write_xlsx(journal$df, path = file)
        showNotification("Excel файл успешно сохранен", type = "message")
      }, error = function(e) {
        showNotification(paste("Ошибка при сохранении Excel:", e$message), type = "error")
      })
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  

  output$total_students <- renderText({ nrow(journal$df) })
  output$avg_mark <- renderText({ 
    if(nrow(journal$df) > 0) round(mean(journal$df$Оценка), 2) else "—"
  })
  output$subjects_count <- renderText({ 
    if(nrow(journal$df) > 0) length(unique(journal$df$Предмет)) else "—"
  })
  

 output$stat_class_subject <- renderDT({
  req(nrow(journal$df) > 0)
  journal$df %>%
    group_by(Класс, Предмет) %>%
    summarise(
      `Средняя` = round(mean(Оценка, na.rm = TRUE), 2),
      `Медиана` = median(Оценка, na.rm = TRUE),
      `Учеников` = n(),
      `5 (кол/%)` = paste0(sum(Оценка == 5), " / ", round(100*sum(Оценка == 5)/n(), 1), "%"),
      `4 (кол/%)` = paste0(sum(Оценка == 4), " / ", round(100*sum(Оценка == 4)/n(), 1), "%"),
      `3 (кол/%)` = paste0(sum(Оценка == 3), " / ", round(100*sum(Оценка == 3)/n(), 1), "%"),
      `2 (кол/%)` = paste0(sum(Оценка == 2), " / ", round(100*sum(Оценка == 2)/n(), 1), "%"),
      `1 (кол/%)` = paste0(sum(Оценка == 1), " / ", round(100*sum(Оценка == 1)/n(), 1), "%"),
      .groups = "drop"
    ) %>%
    datatable(options = list(scrollX = TRUE, pageLength = 10))
})
  
  output$stat_overall <- renderDT({
    req(nrow(journal$df) > 0)
    journal$df %>%
      group_by(Предмет) %>%
      summarise(
        `Средняя оценка` = round(mean(Оценка), 2),
        Медиана = median(Оценка),
        `Учеников` = n(),
        `5 (%)` = round(100 * mean(Оценка == 5), 1),
        `4 (%)` = round(100 * mean(Оценка == 4), 1),
        `3 (%)` = round(100 * mean(Оценка == 3), 1),
        `2 (%)` = round(100 * mean(Оценка == 2), 1),
        `1 (%)` = round(100 * mean(Оценка == 1), 1),
        .groups = "drop"
      ) %>%
      datatable(options = list(scrollX = TRUE))
  })
  

  output$plot_mean <- renderPlot({
    req(nrow(journal$df) > 0)
    journal$df %>%
      group_by(Предмет) %>%
      summarise(Средняя = mean(Оценка)) %>%
      ggplot(aes(x = reorder(Предмет, Средняя), y = Средняя, fill = Предмет)) +
      geom_col() +
      coord_flip() +
      labs(title = "", y = "Средняя оценка", x = "") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$plot_dist <- renderPlot({
    req(nrow(journal$df) > 0)
    journal$df %>%
      ggplot(aes(x = factor(Оценка), fill = factor(Оценка))) +
      geom_bar() +
      facet_wrap(~ Предмет, scales = "free_y") +
      labs(title = "", x = "Оценка", y = "Количество учеников") +
      theme_minimal()
  })
  
  output$plot_boxplot <- renderPlot({
    req(nrow(journal$df) > 0)
    journal$df %>%
      ggplot(aes(x = Предмет, y = Оценка, fill = Предмет)) +
      geom_boxplot() +
      labs(title = "", x = "", y = "Оценка") +
      theme_minimal() +
      theme(legend.position = "none")
  })
}


shinyApp(ui = ui, server = server)