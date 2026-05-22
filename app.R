install.packages("shiny")
library(shiny)

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Orbitron:wght@500;700&display=swap');

      body {
        margin: 0;
        padding: 0;
        font-family: 'Orbitron', sans-serif;
        background:
          linear-gradient(rgba(5,5,16,0.25), rgba(5,5,16,0.45)),
          url('background_2.png');
        background-size: cover;
        background-position: center;
        background-repeat: no-repeat;
        background-attachment: fixed;
        color: white;
        overflow-x: hidden;
      }

      .game-title {
        text-align: center;
        font-size: 40px;
        font-weight: 700;
        color: #00f7ff;
        text-shadow: 0 0 10px #00f7ff, 0 0 25px #00f7ff;
        margin: 20px 0;
      }

      .game-content {
        margin-top: 60px;
      }

      .npc-container {
        display: flex;
        align-items: flex-end;
        gap: 25px;
        margin: 25px 50px;
        transform: scale(1.3);
        transform-origin: left top;
      }

      .npc-container img {
        height: 170px !important;
        filter: drop-shadow(0 0 18px rgba(0,247,255,0.6));
      }

      .host-box {
        background: rgba(255,255,255,0.07);
        border: 1px solid rgba(0,247,255,0.5);
        border-radius: 22px;
        padding: 24px 30px;
        font-size: 20px;
        max-width: 650px;
        backdrop-filter: blur(10px);
        box-shadow: 0 0 25px rgba(0,247,255,0.25);
        position: relative;
        transform: translateY(-50px);
      }

      .door-img {
        transition: all 0.25s ease-in-out;
        cursor: pointer;
        filter: drop-shadow(0 0 10px rgba(0,247,255,0.5));
      }

      .door-img:hover {
        transform: translateY(-12px) scale(1.07);
        filter: drop-shadow(0 0 20px #00f7ff);
      }

      .btn {
        background: linear-gradient(90deg, #00f7ff, #7a5cff) !important;
        color: black !important;
        border: none !important;
        border-radius: 12px !important;
        font-size: 16px;
        padding: 10px 20px;
        margin: 5px;
        box-shadow: 0 0 15px rgba(0,247,255,0.4);
        font-weight: bold;
      }

      .btn:hover {
        transform: scale(1.05);
        box-shadow: 0 0 25px rgba(0,247,255,0.8);
      }
    ")),
    
    tags$link(rel = "preload", as = "image", href = "car.png"),
    tags$link(rel = "preload", as = "image", href = "goat.png"),
    tags$link(rel = "preload", as = "image", href = "door.png"),
    tags$link(rel = "preload", as = "image", href = "host.png"),
    tags$link(rel = "preload", as = "image", href = "junyan.jpg"),
    tags$link(rel = "preload", as = "image", href = "lincaixuan.jpg")
  ),
  
  uiOutput("page")
)

server <- function(input, output, session) {
  
  state <- reactiveValues(
    page = "intro",
    selected = NULL,
    goat = NULL,
    final = NULL,
    stage = "choose",
    locked = FALSE,
    car = sample(1:3, 1)
  )
  
  reset_game <- function() {
    state$selected <- NULL
    state$goat <- NULL
    state$final <- NULL
    state$stage <- "choose"
    state$locked <- FALSE
    state$car <- sample(1:3, 1)
  }
  
  observeEvent(input$start_game, {
    reset_game()
    state$page <- "game"
  })
  
  observeEvent(input$about, {
    state$page <- "about"
  })
  
  observeEvent(input$back_home, {
    reset_game()
    state$page <- "intro"
  })
  
  observeEvent(input$restart, {
    reset_game()
  })
  
  output$page <- renderUI({
    
    if (state$page == "intro") {
      
      fluidPage(
        div(
          style = "text-align:center;margin-top:80px;",
          
          div(class = "game-title", "⚡ Monty Hall Neon Challenge ⚡"),
          
          div(
            style = "
              margin: 30px auto;
              display: inline-block;
              padding: 30px 40px;
              background: rgba(0,0,0,0.4);
              border: 2px solid #00f7ff;
              border-radius: 22px;
              box-shadow: 0 0 25px rgba(0,247,255,0.7);
            ",
            
            tags$h3(
              style = "
                font-size: 46px;
                color: #ffea00;
                text-shadow: 0 0 10px #ffea00, 0 0 30px #ffea00;
                margin-bottom: 18px;
                font-weight: 900;
              ",
              "🎮 遊戲說明"
            ),
            
            tags$p(
              style = "
                font-size: 26px;
                color: white;
                text-shadow: 0 0 6px rgba(0,0,0,0.9);
                margin-bottom: 18px;
              ",
              "三扇門中有一台車，其餘是山羊"
            ),
            
            tags$ul(
              style = "
                font-size: 24px;
                color: #00f7ff;
                text-shadow: 0 0 10px rgba(0,247,255,0.8);
                line-height: 2;
                text-align: left;
                display: inline-block;
              ",
              tags$li("🎯 選一扇門"),
              tags$li("🚪 主持人會打開一扇山羊門"),
              tags$li("🔁 你可以選擇換或不換")
            )
          ),
          
          br(),
          actionButton("start_game", "🚀 開始遊戲", class = "btn"),
          actionButton("about", "👤 作者介紹", class = "btn")
        )
      )
      
    } else if (state$page == "about") {
      
      fluidPage(
        div(
          style = "text-align:center;margin-top:60px;",
          
          div(class = "game-title", "👤 作者介紹"),
          
          div(
            style = "display:flex; justify-content:center; gap:40px; flex-wrap:wrap; margin-top:40px;",
            
            div(
              style = "
                width:320px;
                background:rgba(0,0,0,0.45);
                border:2px solid #00f7ff;
                border-radius:20px;
                padding:20px;
                box-shadow:0 0 20px rgba(0,247,255,0.6);
                text-align:center;
              ",
              
              tags$img(
                src = "junyan.jpg",
                style = "
                  width:140px;
                  height:140px;
                  border-radius:50%;
                  border:3px solid #00f7ff;
                  box-shadow:0 0 15px #00f7ff;
                "
              ),
              
              tags$h3(
                style = "color:#ffea00;text-shadow:0 0 10px #ffea00;",
                "賴俊延"
              ),
              
              tags$a(
                href = "https://github.com/Lai-jun-yan",
                target = "_blank",
                style = "
                  display:inline-block;
                  margin-top:10px;
                  padding:8px 14px;
                  border-radius:10px;
                  background:rgba(0,247,255,0.15);
                  border:1px solid #00f7ff;
                  color:#00f7ff;
                  text-decoration:none;
                  font-weight:bold;
                  cursor:pointer;
                ",
                "🔗 GitHub"
              )
            ),
            
            div(
              style = "
                width:320px;
                background:rgba(0,0,0,0.45);
                border:2px solid #7a5cff;
                border-radius:20px;
                padding:20px;
                box-shadow:0 0 20px rgba(122,92,255,0.6);
                text-align:center;
              ",
              
              tags$img(
                src = "lincaixuan.jpg",
                style = "
                  width:140px;
                  height:140px;
                  border-radius:50%;
                  border:3px solid #7a5cff;
                  box-shadow:0 0 15px #7a5cff;
                "
              ),
              
              tags$h3(
                style = "color:#ffea00;text-shadow:0 0 10px #ffea00;",
                "林采宣"
              ),
              
              tags$a(
                href = "https://github.com/Sheena0210",
                target = "_blank",
                style = "
                  display:inline-block;
                  margin-top:10px;
                  padding:8px 14px;
                  border-radius:10px;
                  background:rgba(0,247,255,0.15);
                  border:1px solid #00f7ff;
                  color:#00f7ff;
                  text-decoration:none;
                  font-weight:bold;
                  cursor:pointer;
                ",
                "🔗 GitHub"
              )
            )
          ),
          
          br(),
          actionButton("back_home", "🏠 回主頁", class = "btn")
        )
      )
      
    } else {
      
      tagList(
        div(class = "game-title", "⚡ Monty Hall Neon Challenge ⚡"),
        
        div(
          class = "game-content",
          
          div(
            class = "npc-container",
            img(src = "host.png"),
            div(
              class = "host-box",
              uiOutput("host_text")
            )
          ),
          
          br(), br(),
          
          fluidRow(
            column(4, uiOutput("door1"), align = "center"),
            column(4, uiOutput("door2"), align = "center"),
            column(4, uiOutput("door3"), align = "center")
          ),
          
          br(), br(),
          
          fluidRow(
            column(
              12,
              align = "center",
              actionButton("confirm", "確定選擇", class = "btn"),
              actionButton("restart", "重新開始", class = "btn"),
              actionButton("back_home", "🏠 回主頁", class = "btn")
            )
          )
        )
      )
    }
  })
  
  output$host_text <- renderUI({
    
    req(state$page == "game")
    
    if (state$stage == "choose") {
      
      if (is.null(state$selected)) {
        "請選擇一扇門 👇"
      } else {
        paste0("你選擇了 Door ", state$selected, "，請按下確定選擇。")
      }
      
    } else if (state$stage == "switch") {
      
      tagList(
        h4(paste("主持人打開 Door", state$goat)),
        p("這扇門後面是山羊 🐐"),
        h4("現在你要換門嗎？"),
        br(),
        actionButton("switch", "🔁 換門", class = "btn"),
        actionButton("stay", "🙅 不換", class = "btn")
      )
      
    } else {
      
      req(!is.null(state$final))
      
      if (state$final == state$car) {
        tagList(
          h3("🎉 恭喜！你贏了汽車！"),
          p(paste("汽車在 Door", state$car))
        )
      } else {
        tagList(
          h3("😢 很可惜，你選到山羊。"),
          p(paste("汽車其實在 Door", state$car))
        )
      }
    }
  })
  
  render_door <- function(num) {
    
    is_selected <- !is.null(state$selected) && state$selected == num
    is_final <- !is.null(state$final) && state$final == num
    is_goat_opened <- !is.null(state$goat) && state$goat == num
    
    style <- if (is_selected && state$stage == "choose") {
      "
      border:3px solid #ff2bd6;
      transform:scale(1.1);
      box-shadow:0 0 10px #ff2bd6,0 0 25px #ff2bd6,0 0 50px #00f7ff;
      filter:brightness(1.2);
      "
    } else {
      ""
    }
    
    if (state$stage == "end") {
      
      img(
        src = if (num == state$car) "car.png" else "goat.png",
        height = "300px",
        class = "door-img"
      )
      
    } else if (is_goat_opened) {
      
      img(
        src = "goat.png",
        height = "300px",
        class = "door-img"
      )
      
    } else {
      
      actionButton(
        inputId = paste0("door", num),
        label = tagList(
          img(
            src = "door.png",
            height = "300px",
            style = style
          )
        ),
        style = "border:none;background:none;",
        disabled = state$locked
      )
    }
  }
  
  output$door1 <- renderUI(render_door(1))
  output$door2 <- renderUI(render_door(2))
  output$door3 <- renderUI(render_door(3))
  
  observeEvent(input$door1, {
    if (!state$locked && state$stage == "choose") {
      state$selected <- 1
    }
  })
  
  observeEvent(input$door2, {
    if (!state$locked && state$stage == "choose") {
      state$selected <- 2
    }
  })
  
  observeEvent(input$door3, {
    if (!state$locked && state$stage == "choose") {
      state$selected <- 3
    }
  })
  
  observeEvent(input$confirm, {
    
    req(state$selected)
    
    if (state$stage == "choose") {
      state$locked <- TRUE
      
      candidates <- setdiff(1:3, c(state$selected, state$car))
      state$goat <- sample(candidates, 1)
      
      state$stage <- "switch"
    }
  })
  
  observeEvent(input$switch, {
    
    req(state$stage == "switch")
    
    state$final <- setdiff(1:3, c(state$selected, state$goat))
    state$stage <- "end"
  })
  
  observeEvent(input$stay, {
    
    req(state$stage == "switch")
    
    state$final <- state$selected
    state$stage <- "end"
  })
}

shinyApp(ui, server)