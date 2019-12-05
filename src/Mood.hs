module Mood where

data Mood = Blah | Woot deriving (Show)

changeMood :: Mood -> Mood
changeMood Woot = Blah
changeMood _ = Woot
