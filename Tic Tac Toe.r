# install.packages("plotrix")
library("plotrix")

### Draws the board outline to the plot
drawBoard <- function() {
  plot.new()
  plot.window(c(0, 60), c(0, 70), asp=1)
  abline(h = c(40, 40)) # Draw top horiz line
  abline(h = c(20, 20)) # Draw bottom horiz line
  segments(x0 = c(40), x1 = c(40), y0 = c(0), y1 = c(60))  # Draw left vert line
  segments(x0 = c(20), x1 = c(20), y0 = c(0), y1 = c(60))  # Draw left vert line
}

### Draws the interface to select a player piece and takes the input of the user to choose
###
### Returns the piece selected; X or O
drawSelectPlayer <- function() {
  plot.new()
  plot.window(c(0, 60), c(0, 70), asp=1)
  segments(x0 = c(30), x1 = c(30), y0 = c(0), y1 = c(60))  # Draw left vert line
  text(30, 65, "Click X or O to select your piece!", col = "red", cex=1.5) # Draw text at top
  drawX(1, 2)
  drawO(3, 2)
  
  # Get the type that is clicked on
  colClick <- 0 # Stores the column the user clicks
  rowClick <- 0 # Stores the row the user clicks
  type <- ""
  otherType <- ""
  while (type == "") {
    clickCoords <- locator(n = 1)
    if (clickCoords$x < 30) {
      type <- "X"
      otherType <- "O"
    } else {
      type <- "O"
      otherType <- "X"
    }
  }
  return (c(type, otherType)) # Return player type
}

### Draws an X to the board
###
### col: The column to draw it at
### row: The row to draw it at
drawX <- function(col, row) {
  # row and col are coming in from 1:3 so subtract 1
  col <- col - 1
  row <- row - 1
  xLength <- 15
  yLength <- 15
  sepDist <- 15
  colOffset <- col * sepDist * 1.5;
  rowOffset <- row * sepDist * 1.5;
  lines(x = c(colOffset, xLength + colOffset), y = c(rowOffset, yLength + rowOffset)) # Draw first line of x
  segments(x0=c(xLength + colOffset), x1=c(colOffset), y0=c(rowOffset), y1=c(yLength + rowOffset)) # Draw second, decreasing line of x
}

### Draws an O to the board
###
### col: The column to draw it at
### row: The row to draw it at
drawO <- function(col, row) {
  # row and col are coming in from 1:3 so subtract 1
  col <- col - 1
  row <- row - 1
  xLength <- 10
  yLength <- 10
  sepDist <- 15
  radius <- 7.5
  colOffset <- (col * sepDist) + (xLength * col * 0.5);
  rowOffset <- (row * sepDist) + (yLength * row * 0.5);
  draw.circle((xLength + colOffset), (yLength + rowOffset), radius)
}

### Checks if a board is solved by X or O
###
### board: The board to check for wins
### Returns the piece that won (X or O) or "" if none
checkSolved <- function(board) {
  # Check rows
  for (y in 1:3) {
    solved = checkListSolved(board[,y])
    if (solved != "") {
      return (solved)
    }
  }
  # Check cols
  for (x in 1:3) {
    solved = checkListSolved(board[x,])
    if (solved != "") {
      return (solved)
    }
  }
  
  # Check Diagnols
  if (board[1,1] == board[2,2] && board[1,1] == board[3,3] && board[1,1] != "") {
    return (board[1,1])
  }
  if (board[3,1] == board[2,2] && board[3,1] == board[1, 3] && board[3,1] != "") {
    return (board[3,1])
  }
  return ("")
}

### Checks if a vector contains all same pieces
###
### list: The vector to check
### Returns the piece if won (X or O) or "" if none
checkListSolved <- function(list) {
  if (list[1] == list[2] && list[1] == list[3] && list[1] != "") {
    return (list[1])
  }
  return ("")
}

### Gets the users next move by them clicking on the row and column on the plot
###
### Returns the column and row in a vector with column at index 1 and row at 2
getUserMove <- function(board) {
  text(30, 65, "Click on screen to move!", col = "red", cex=1.5) # Draw text at top
  colClick <- 0 # Stores the column the user clicks
  rowClick <- 0 # Stores the row the user clicks
  while (colClick == 0 || rowClick == 0 || board[colClick,rowClick] != "") {
    clickCoords <- locator(n = 1)
    
    colClick <- floor(abs(clickCoords$x) / 20) + 1 # Get the x position; b/c every cell is 20px, mod by 20, add one in case < 20
    rowClick <- floor(abs(clickCoords$y) / 20) + 1 # Get the y position; b/c every cell is 20px, mod by 20, add one in case < 20
    if (colClick > 3 || rowClick > 3) { # Make sure they clicked within the bounds of the array to escape exception
      colClick <- 0
      rowClick <- 0
    }
  }
  return (c(colClick, rowClick)) # Return both
}

### A matrix that is used to check possibe win positions
possibleWinsMatrix <- matrix(c(1, 4, 7, 1, 2, 3, 1, 3, 2, 5, 8, 4, 5, 6, 5, 5, 3, 6, 9, 7, 8, 9, 9, 7), nrow = 8, ncol = 3)
### Used to get the value based on the number of possible "wins" there is
heuristicMatrix <- matrix(c(0, 10, 100, 1000, -10, 0, 0, 0, -100, 0, 0, 0, -1000, 0, 0, 0), nrow = 4, ncol = 4)

### Gets the heuristical score for the board
###
### board: The board
### pieceType: The piece we're checking for
### Returns a board score value
getBoardScore <- function(board, pieceType) {
  heur <- 0 # init heuristic value to 0
  for (i in 1:8) { # Go through all possible ways to win
    playerCount <- 1 # Set init count to 1
    oppCount <- 1 # Set init count to 1
    for (j in 1:3) { # Get the individual column needed to win
      boardPos <- possibleWinsMatrix[i, j] - 1 # Get the board position (1-9)
      row <- floor(boardPos / 3) # Pull out row
      col <- floor((boardPos - row) / 3) + 1 # Pull out the col
      row <- row + 1 # Add 1 because index starts at 1 not 0
      piece <- board[col, row] # Get the piece there
      if (piece == pieceType) {
        playerCount <- playerCount + 1 # Add the possibilty for the players win if matches
      } else if (piece != "") {
        oppCount <- oppCount + 1 # Add the possibilty for the opponents win if matches
      }
    }
    heur <- heur + heuristicMatrix[playerCount, oppCount] # Add to the heuristic value
  }
  return (heur)
}

getComputerMove <- function(board, computerPiece, otherPiece) {
  best <- minimax(board, 3, computerPiece, otherPiece, FALSE) # Minimax
  return (c(best[2], best[3])) # Get the col and row and return it
}

### Finds the most optimal solution to maximize the minimum score possible
###
### minimaxBoard: The game board
### depth: The depth (ply) to look at
### pieceType: The piece to check
### otherPiece: The opposing piece
### min: Whether to minimize or not
### Returns the best score and the position
minimax <- function(minmaxBoard, depth, pieceType, otherPiece, min) {
  if (depth == 0) { # if at max depth return value of the board
    return (getBoardScore(minmaxBoard, pieceType))
  }
  emptyPositions <- getEmptyBoardSpots(minmaxBoard) # Get all empty spots
  emptyTotal <- length(emptyPositions) / 2 # Get the total positions
  if (min == TRUE) { # If we're minimizing 
    best <- c(100000000, 0, 0) # init best
    for(i in 1:emptyTotal) {
      
      # Get position
      pos <- (i - 1) * 2
      col <- emptyPositions[pos + 1]
      row <- emptyPositions[pos + 2]
      
      # Add the piece to the board
      newBoard <- matrix(minmaxBoard, nrow = 3, ncol = 3)
      newBoard[col, row] = otherPiece
      if (checkSolved(newBoard) != "") { # If solved then return 
        return (c(-100000 * depth, col, row))
      }
      
      #Check piece moves
      score <- minimax(newBoard, depth - 1, pieceType, otherPiece, FALSE)
      if (length(score) > 1) {
        score <- score[1]
      }
      if (score <= best[1]) { # Check best
        best <- c(score, col, row)
      }
    }
    return (best) # return the best
  } else {
    best <- c(-100000000, 0, 0) # init best
    for(i in 1:emptyTotal) {
      
      # Get the position
      pos <- (i - 1) * 2
      col <- emptyPositions[pos + 1]
      row <- emptyPositions[pos + 2]
      
      # Get the board and set the new piece
      newBoard <- matrix(minmaxBoard, nrow = 3, ncol = 3)
      newBoard[col, row] = pieceType
      
      # Check if solved and return if it is
      if (checkSolved(newBoard) != "") {
        return (c(100000 * depth, col, row))
      }
      
      #Check piece moves
      score <- minimax(newBoard, depth - 1, pieceType, otherPiece, TRUE) # Get score
      if (length(score) > 1) {
        score <- score[1]
      }
      if (score >= best[1]) { # If best set it
        best <- c(score, col, row)
      }
    }
    return (best) # Return the best
  }
}

### Find empty spots within the board
###
### board: The board
### Returns a vector containing the x and y coordinates of each empty spot
getEmptyBoardSpots <- function(board) {
  empty <- c()
  for (y in 1:3) {
    for (x in 1:3) {
      if (board[x, y] == "") {
        empty <- c(empty, x, y) # Push to vector
      }
    }
  }
  return (empty)
}

### Draws all positions to the plot
###
### board: A 3x3 matrix containing X, O's and ""
drawPositions <- function(board) {
  for (y in 1:3) {
    for (x in 1:3) {
      if (board[x, y] == "X") {
        drawX(x, y)
      } else if (board[x, y] == "O") {
        drawO(x, y)
      }
    }
  }
}

drawRestart <- function() {
  text(5, 65, "Restart", col = "blue", cex=1.5) # Draw text at top
  text(55, 65, "Exit", col = "blue", cex=1.5) # Draw text at top
  
  while (TRUE) {
    clickCoords <- locator(n = 1)
    if (clickCoords$x <= 15 && clickCoords$y >= 60) {
      return (TRUE)
    } else if (clickCoords$x >= 50 && clickCoords$y >= 60) {
      return (FALSE) 
    }
  }
}

drawPlayerWon <- function() {
  text(30, 65, "YOU WON!!!", col = "green", cex=2)
}

drawPlayerLost <- function() {
  text(30, 65, "YOU LOST!!!", col = "red", cex=2)
}

drawTied <- function() {
  text(30, 65, "YOU TIED!!!", col = "orange", cex=2)
}

restart <- TRUE

while(restart == TRUE) {
  # Get the player type
  types <- drawSelectPlayer()
  playerType <- types[1]
  computerType <- types[2]
  
  nextMove <- playerType # Give first move to the player
    
  # Initialize board
  board <- matrix(rep("", 9), nrow = 3, ncol = 3)
  getBoardScore(board, playerType)
  
  while(checkSolved(board) == "" && length(getEmptyBoardSpots(board)) > 0) { # While no one won and not empty
    # Draw board
    drawBoard()
    drawPositions(board)
    
    if (nextMove == playerType) { # If player's move
      move <- getUserMove(board) # Get move
      board[move[1], move[2]] = playerType # Set move
      nextMove <- computerType # Set next move to computer
    } else {
      move <- getComputerMove(board, computerType, playerType) # Get move
      board[move[1], move[2]] = computerType # Set move 
      nextMove <- playerType # Set next move to player
    }
    # Redraw incase of a win
    drawBoard()
    drawPositions(board)
  }
  
  winner <- checkSolved(board) # Check if solved
  if (winner != "") {
    if (winner == playerType) {
      drawPlayerWon() # Draw won 
    } else {
      drawPlayerLost() # Draw lost
    }
  } else {
    if (length(getEmptyBoardSpots(board)) == 0) {
      drawTied() # Draw tied
    }
  }
  restart <- drawRestart() # Check if restart or not
}

plot.new() # wipe screen when done