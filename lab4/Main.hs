--import Cards

data Player = Player String | Empty
              deriving Show

addNewPlayers :: IO [Player]
addNewPlayers = addNewPlayers'

addNewPlayers' :: IO [Player]
addNewPlayers' =      do   player1 <- addNewPlayer
                           player2 <- addNewPlayer
                           return (player1:player2:[])

                           --if (player /= Empty)
                           --else return list

addNewPlayer :: IO Player
addNewPlayer = do putStrLn "0) Add new player"
                  putStrLn "1) Start game"
                  n <- readLn
                  if (n < 0 || n > 1)
                  then do
                    putStrLn "invalid input"
                    player <- addNewPlayer
                    return player
                  else
                     if n == 0 then do
                       putStrLn "Set player name: "
                       name <- getLine
                       return (Player name)
                     else
                       return Empty


getPlayerName :: Player -> String
getPlayerName (Player name) = name

start :: IO ()
start = do putStrLn "Welcome to Exploding Kittens. Make your choice:"
           players <- addNewPlayers
           putStrLn ("Players: " ++ " \n 1 " ++ (getPlayerName (players !! 0))

                     ++ "\n 2 " ++ (getPlayerName (players !! 1)))
           start
