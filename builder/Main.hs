module Main (main) where

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

  match (fromList ["about.rst", "contact.markdown"]) $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  match "posts/*" $ do
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
      >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

  match "projects.html" $ do
    route   idRoute
    compile $ do
      let projectContext = field "url" (pure . toFilePath . itemIdentifier)
          projects = loadAll "projects/*/index.html" :: Compiler [Item CopyFile]
          projectsContext =
               listField "projects" projectContext projects
            <> defaultContext
      getResourceBody
        >>= applyAsTemplate projectsContext
        >>= loadAndApplyTemplate "templates/default.html" projectsContext
        >>= relativizeUrls

  match "projects/**" $ do
    route   idRoute
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
