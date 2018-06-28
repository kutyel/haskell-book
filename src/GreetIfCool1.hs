module GreetIfCool1 where

greetIfCool :: String -> IO ()
greetIfCool coolness =
  if cool
    then putStrLn "eyyyy. What's shakin'?"
  else
    putStrLn "pshhhh."
  where cool =
          coolness == "downright frosty yo"
