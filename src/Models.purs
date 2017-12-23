module Models where

import Data.List (List)

type Article = { id :: Int
               , title :: String
               , slug :: String
               , content :: String
               , created_at :: String
               }

type Articles = List Article
