{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Aeson (Value(..), eitherDecode, encode, (.=))
import qualified Data.ByteString.Lazy.Char8 as BS (unpack)
import Data.HashMap.Strict ((!?))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T (pack, unpack)
import GHC.Exts (toList)
import System.FilePath (dropExtension, (</>))

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
    route   cleanRoute
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  create ["archive.html"] $ do
    route   cleanRoute
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
    route   cleanRoute
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

cleanRoute :: Routes
cleanRoute = customRoute $ (</> "index.html") . dropExtension . toFilePath

--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
     dateField "date" "%B %e, %Y"
  <> defaultContext

listCtx :: String -> Context String
listCtx title =
     listField "posts" postCtx (recentFirst =<< loadAll "posts/**")
  <> constField "title" title
  <> defaultContext

jsonCtx :: Context Value
jsonCtx = Context $ \name _ (Item _ meta) ->
  fromMaybe (failure meta) . getField meta $ splitName name
  where
    getField (Object obj) (n:ns)
      | Just val <- obj !? T.pack n = getField val ns
    getField (Object obj) [] = Just
      $ ListField jsonCtx <$> traverse (makeItem . uncurry object) (toList obj)
    getField (Array arr) [] = Just
      $ ListField jsonCtx <$> traverse makeItem (toList arr)
    getField (String txt) [] = Just . pure . StringField $ T.unpack txt
    getField _ _ = Nothing

    object key value = Object $ "key" .= key <> "value" .= value 

    splitName ('.':cs) = "" : splitName cs
    splitName (c:cs) = let n:ns = splitName cs in (c:n):ns
    splitName [] = [""]

    failure json = noResult $ "Tried JSON context " ++ BS.unpack (encode json)
