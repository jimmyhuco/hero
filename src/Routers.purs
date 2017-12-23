module Routers where

import Control.Alt ((<$>), (<$), (<|>))
import Prelude (class Show, show, (*>), (<>))
import Routing.Match (Match)
import Routing.Match.Class (lit, str)

type Slug = String

-- Home -> /
-- About -> /about
-- Articles -> /articles
-- Article Slug -> /articles/hello-world
-- Tag Slug -> /tags/purescript
data Routes = Home
            | About
            | Articles
            | Article Slug
            | Tag Slug

instance showRoutes :: Show Routes where
  show Home = "Home"
  show About = "About"
  show Articles = "Articles"
  show (Article slug) = "Articles/" <> show slug
  show (Tag slug) = "Tags/" <> show slug

routing :: Match Routes
routing = about
          <|> article
          <|> articles
          <|> tag
          <|> home
  where
    home = Home <$ lit ""
    about = About <$ lit "about"
    articles = Articles <$ lit "articles"
    article = Article <$> (lit "articles" *> str)
    tag = Tag <$> (lit "tags" *> str)
