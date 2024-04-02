import Data.List (elemIndex)

--encontra a posiçao do rei branco
findWhiteKing :: [[Char]] -> (Int, Int)
findWhiteKing board = go board 0
  where
    go [] _ = error "Rei branco não encontrado!"
    go (row:rows) rowNum = case elemIndex 'R' row of
      Just colNum -> (rowNum, colNum)
      Nothing -> go rows (rowNum + 1)

--verifica ameacas na linha horizontal
checkHorizontal :: [[Char]] -> (Int, Int) -> Bool
checkHorizontal board (row, col) =
  let rowContent = board !! row
      left = take col rowContent
      right = drop (col + 1) rowContent
  in 't' `elem` left || 'q' `elem` left || 't' `elem` right || 'q' `elem` right

--verifica ameacas na linha vertical
checkVertical :: [[Char]] -> (Int, Int) -> Bool
checkVertical board (row, col) =
  let columnContent = map (!! col) board
      above = take row columnContent
      below = drop (row + 1) columnContent
  in 't' `elem` above || 'q' `elem` above || 't' `elem` below || 'q' `elem` below

--verifica ameacas na diagonal
checkDiagonal :: [[Char]] -> (Int, Int) -> Bool
checkDiagonal board (row, col) =
  let diagonal1 = [board !! (row + k) !! (col + k) | k <- [-min row col .. min (length board - row - 1) (length (head board) - col - 1)]]
      diagonal2 = [board !! (row + k) !! (col - k) | k <- [-min row (length (head board) - col - 1) .. min (length board - row - 1) col]]
      threats = filter (`elem` "qb") (diagonal1 ++ diagonal2)
  in not (null threats)

-- verifica ameacas na forma de padrao L do cavalo
checkKnight :: [[Char]] -> (Int, Int) -> Bool
checkKnight board (row, col) =
  let possibleMoves = [(row + x, col + y) | x <- [-2, -1, 1, 2], y <- [-2, -1, 1, 2], abs x + abs y == 3]
      threats = [board !! x !! y | (x, y) <- possibleMoves, x >= 0 && x < length board && y >= 0 && y < length (head board), board !! x !! y == 'c']
  in not (null threats)

--verifica se o rei branco está em xeque, chama as outras funcoes
isWhiteKingInCheck :: [[Char]] -> Bool
isWhiteKingInCheck board =
  let kingPos = findWhiteKing board
  in checkHorizontal board kingPos ||
     checkVertical board kingPos ||
     checkDiagonal board kingPos ||
     checkKnight board kingPos



main :: IO ()
main = do
  input <- getLine
  let board = words input
  print (isWhiteKingInCheck board)
