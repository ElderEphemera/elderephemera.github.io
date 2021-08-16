{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Aeson (Value(..), eitherDecode, (.=))
import Data.HashMap.Strict ((!?))
import Data.Text (pack, unpack)
import GHC.Exts (toList)

import Hakyll

import Style (style)

--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  create ["css/style.css"] $ do
    route   idRoute
    compile $ makeItem style

  match "posts/**" $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  create ["archive.html"] $ do
    route   idRoute
    compile $ do
      let archiveCtx = listCtx "Archives"
      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  match "index.html" $ do
    route   idRoute
    compile $ do
      let indexCtx = listCtx "Home"
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "404.html" $ do
    route   idRoute
    compile $ getResourceBody
      >>= loadAndApplyTemplate "templates/default.html" defaultContext

  match "templates/*" $ compile templateBodyCompiler

  match "projects.html" $ do
    route   idRoute
    compile $ do
      let projects = traverse (either fail pure . traverse eitherDecode)
            =<< loadAll "projects/*/info.json"
          projectsContext =
               listField "projects" jsonCtx projects
            <> defaultContext
      getResourceBody
        >>= applyAsTemplate projectsContext
        >>= loadAndApplyTemplate "templates/default.html" projectsContext
        >>= relativizeUrls

  match "projects/*/info.json" $ compile getResourceLBS

  match "projects/*/content/**" $ do
    route   $ gsubRoute "content/" mempty
    compile copyFileCompiler

--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
     dateField "date" "%B %e, %Y"
  <> defaultContext

listCtx :: String -> Context String
listCtx title =
     listField "posts" postCtx (recentFirst =<< loadAll "posts/*")
  <> constField "title" title
  <> defaultContext

jsonCtx :: Context Value
jsonCtx = Context $ \name args (Item _ meta) ->
  if null args
    then getField meta (splitName name)
    else pure EmptyField
  where
    getField (Object obj) (n:ns) | Just val <- obj !? pack n = getField val ns
    getField (Object obj) [] = ListField jsonCtx
      <$> traverse (makeItem . uncurry object) (toList obj)
    getField (String txt) [] = pure . StringField $ unpack txt
    getField (Array arr) [] = ListField jsonCtx
      <$> traverse makeItem (toList arr)
    getField _ _ = pure EmptyField

    object key value = Object $ "key" .= key <> "value" .= value 

    splitName ('.':cs) = "" : splitName cs
    splitName (c:cs) = let n:ns = splitName cs in (c:n):ns
    splitName [] = [""]
