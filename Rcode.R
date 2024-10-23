##The game Dots and Boxes is a two-player pencil-and-paper game. It begins with an empty grid of
##dots, where players alternate turns, drawing a horizontal or vertical line between two adjacent, 
##unconnected dots. If a player completes the fourth side of a 1x1 box, they score a point and get an 
##extra turn. The completed box is marked with the player's color. The game concludes when no more 
##lines can be drawn, and the player with the most points wins. In cases where the grid has an even 
##number of boxes, a tie is possible.

dots_and_boxes = function(n_row = 3, n_col = 3){
  # keep user's par settings 
  par_original <- par(no.readonly = TRUE)
  on.exit(par(par_original))
  
  #layout of the game
  if(n_col > n_row) {
    y_axis <- rep(1:n_row, n_col)
    x_axis <- rep(1:n_col, each = n_row)
  } else if (n_col < n_row){
    y_axis <- rep(1:n_row, n_col)
    x_axis <- rep(1:n_col, each = n_row)
  } else {
    y_axis <- rep(1:n_row, n_row)
    x_axis <- rep(1:n_col, each = n_col) 
  }
  
  plot.new()

  plot(x_axis,y_axis, pch = 20,
       xlab = "",
       ylab = "",
       xlim = range(1,n_col, by =1),
       ylim = rev(range(y_axis)),
       frame.plot = FALSE,
       yaxt = "n",
       xaxt = "n")
  
  axis(3, at = seq(1,n_col, by =1), tick = FALSE, line = NA)
  axis(2, at = seq(1,n_row, by = 1), tick = FALSE, line = NA, las = 2)
  
  #check direction 
  player_dir <- function(dir_possible){
    dir_valid <- FALSE
    while(!dir_valid){
      direction <- scan(what = character(), n = 1, quiet = TRUE)
      if(!(direction %in% dir_possible)) {
        cat(paste0("Direction must be one of d or r, corresponding to down or right. Again:\n"))
      } else if (dots[1] == n_row & direction == "d"){
        cat(paste0("Chosen dot is outside of grid. Choose another direction (d or r). Again:\n"))
      } else if (dots[2] == n_col & direction == "r"){
        cat(paste0("Chosen dot is outside of grid. Choose another direction (d or r). Again:\n"))
      } else if ((direction == "d") && (d.check[dots[1],dots[2]]== 1)) {
        cat(paste0("Chosen direction is not available. Choose another direction (d or r). Again:\n"))
      } else if ((direction == "r") && (r.check[dots[1],dots[2]]== 1)) {
        cat(paste0("Chosen direction is not available. Choose another direction (d or r). Again:\n"))
      }
      else {
        dir_valid <- TRUE
      } 
    }
    return(direction)
  }
  
  #check movement
  player_move <- function(move_possible){
    move_valid <- FALSE
    while(!move_valid){
      dots <- scan(what = numeric(), n = 2, quiet = TRUE)
      if (!(dots[1] %in% move_possible)) {
        cat(paste0("Chosen dot is outside of grid. Again(1: row, 2: column):\n")) #row
      } else if (!(dots[2] %in% move_possible)) {
        cat(paste0("Chosen dot is outside of grid. Again(1: row, 2: column):\n")) #column
      } else if ((dots[1]==n_row) && (dots[2]==n_col)) {
        cat(paste0("Chosen dot is outside of grid. Again(1: row, 2: column):\n"))
      } else if ((r.check[dots[1],dots[2]]== 1) && (d.check[dots[1],dots[2]]== 1)) {
        cat(paste0("Chosen dot is is already connected with bottom dot and right dot (or they don't exist on the grid). Again(1: row, 2: column):\n"))
      } else if (dots[1] == n_row && (r.check[dots[1],dots[2]]== 1)) {
        cat(paste0("Chosen dot is is already connected with bottom dot and right dot (or they don't exist on the grid). Again(1: row, 2: column):\n"))
      } else if (dots[2] == n_col && (d.check[dots[1],dots[2]]== 1)) {
        cat(paste0("Chosen dot is is already connected with bottom dot and right dot (or they don't exist on the grid). Again(1: row, 2: column):\n"))
      } 
      else {
        move_valid <- TRUE
      }
    }
    return(dots)
  }
  
  #leaderboard
  r.check <- matrix(0, nrow = (n_row+1), ncol = (n_col)) #to record movement of "right" direction
  d.check <- matrix(0, nrow = (n_row), ncol = (n_col+1)) #to record movement of "down" direction
  box.check <- matrix(0, nrow = ((n_row-1)*(n_col-1)), ncol = 1) # check if a box is created or not.
  
  leaderboard = matrix(0, nrow = 1, ncol = 2)
  colnames(leaderboard) <- c("Player 1", "Player 2")
  rownames(leaderboard) <- "Score"
  
  #initialization of the game
  move_possible <- 1:if(n_col > n_row) {n_col} else {n_row} #possible choice of movement
  dir_possible <- as.character(c("d", "r")) #possible choice of direction
  player_turn  <- sample(1:2,1, replace = TRUE)
  change_player <- matrix(0, nrow = (n_row*(n_col-1)+n_col*(n_row-1)), ncol = 1) #vector to record the turn of each player
  move_count <- 1
  complete <- 1
  is_game_over <- FALSE
  X <- "X"
  
  #print start message
  cat(paste0("Player ", player_turn," starts!\n",
             "In each move you have to choose a dot and a direction (d: down, r: right).\n"))
  
  #mechanisms (loop) of the game  
  while (!is_game_over) {
    
    #print game instructions for movements
    cat(paste0("Player ", player_turn," choose a dot (1: row, 2: column)!"))
    #initiating the game for movements  
    dots <- player_move(move_possible) 
    
    #print game instructions for direction
    cat(paste0("Player ", player_turn," choose the direction (d = down, r = right)!\n"))
    #initiating the game for directions
    direction <- player_dir(dir_possible) 
    
    #creating lines and updating movement board
    if (direction == "d") {
      arrows(x0=dots[2],y0=dots[1],y1=dots[1]+1,code =0,
             col = c("black", "red")[player_turn]) 
      d.check[dots[1],dots[2]] <- 1 #updating board for direction "down"
      
    } else if (direction == "r") {
      arrows(x0=dots[2],y0=dots[1],x1=dots[2]+1,code =0,
             col = c("black", "red")[player_turn]) 
      r.check[dots[1],dots[2]] <- 1 #updating board for direction "right"
    } else {""}
    
    #recording each turn of each player
    change_player[move_count] <- player_turn
    
    #if a box is created
    if (direction == "r"){
      if (dots[1] > 0){ 
        if((r.check[dots[1],dots[2]] == 1) && (r.check[dots[1]+1,dots[2]] == 1) && (d.check[dots[1],dots[2]] == 1) && (d.check[dots[1],dots[2]+1] == 1)){
          text(dots[1]+0.5,dots[1]+0.5,X,col=c("black", "red")[change_player[move_count]])
          box.check[complete] <- 1  
          
          #update score board
          cat(paste0("Player ", (change_player[move_count]),
                     " completed a square and takes another turn! Current leaderboard:\n"))
          leaderboard[,change_player[move_count]] <- leaderboard[,change_player[move_count]]+1 
          show(leaderboard)
          
          complete <- complete+1
          player_turn <- change_player[move_count-1] #add another turn for the player that formed a box
          
        } 
      }
      
      if(dots[1] > 1){
        if ((r.check[dots[1],dots[2]] == 1) && (r.check[dots[1]-1,dots[2]] == 1) && (d.check[dots[1]-1,dots[2]] == 1) && (d.check[dots[1]-1,dots[2]+1] == 1)){
          text(dots[2]+0.5,dots[1]-0.5,X,col=c("black", "red")[change_player[move_count]])
          box.check[complete] <- 1  
          
          #update score board
          cat(paste0("Player ", (change_player[move_count]),
                     " completed a square and takes another turn! Current leaderboard:\n"))
          leaderboard[,change_player[move_count]] <- leaderboard[,change_player[move_count]]+1 
          show(leaderboard)
          
          
          complete <- complete+1
          player_turn <- change_player[move_count-1] #add annother turn for the player that formed a box
          
        } 
      } 
    }
    
    if (direction == "d"){
      if (dots[2] > 0){
        if((d.check[dots[1],dots[2]] == 1) && (d.check[dots[1],dots[2]+1] == 1) && (r.check[dots[1],dots[2]] == 1) && (r.check[dots[1]+1,dots[2]] == 1)){
          text(dots[2]+0.5,dots[1]+0.5,X,col=c("black", "red")[change_player[move_count]])
          box.check[complete] <- 1  
          
          #update score board
          cat(paste0("Player ", (change_player[move_count]),
                     " completed a square and takes another turn! Current leaderboard:\n"))
          leaderboard[,change_player[move_count]] <- leaderboard[,change_player[move_count]]+1 
          show(leaderboard)
          
          
          complete <- complete+1
          player_turn <- change_player[move_count-1] #add annother turn for the player that formed a box
          
        }
      }
      
      if (dots[2] > 1){
        if ((d.check[dots[1],dots[2]] == 1) && (d.check[dots[1],dots[2]-1] == 1) && (r.check[dots[1],dots[2]-1] == 1) && (r.check[dots[1]+1,dots[2]-1] == 1)){
          text(dots[2]-0.5,dots[1]+0.5,X,col=c("black", "red")[change_player[move_count]])
          box.check[complete] <- 1  
          
          #update score board
          cat(paste0("Player ", (change_player[move_count]),
                     " completed a square and takes another turn! Current leaderboard:\n"))
          leaderboard[,change_player[move_count]] <- leaderboard[,change_player[move_count]]+1 
          show(leaderboard)
          
          complete <- complete+1
          player_turn <- change_player[move_count-1] #add annother turn for the player that formed a box
          
        }
      }
    }
    else {""}
    
    #check winners
    if(sum(box.check) == length(box.check)) {
      is_game_over <- TRUE
      if (leaderboard[,1] > leaderboard[,2]){
        cat(paste0("Player 1 wins! Final leaderboard:\n"))
        show(leaderboard)
      } else if (leaderboard[,1] < leaderboard[,2]) {
        cat(paste0("Player 2 wins! Final leaderboard:\n"))
        show(leaderboard)
      } else {
        cat(paste0("Player 1 and Player 2 are tied winners! Final leaderboard:\n"))
        show(leaderboard)
      }
    }
    #changing player turn
    move_count <- move_count + 1
    if ((player_turn == 1)) {
      player_turn <- 2
    } else {
      player_turn <- 1
    }
  }
}

dots_and_boxes() #default game 3x3 grid
dots_and_boxes(n_row = 4, n_col = 5) #n_row x n_col grid
