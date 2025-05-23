{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.Aeson (Value(..), eitherDecode, encode, (.=))
import Data.Aeson.Key qualified as Key (fromString)
import Data.Aeson.KeyMap qualified as KM (lookup)
import Data.ByteString.Lazy.Char8 qualified as BS (unpack)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T (length, lines, unpack)
import Data.Text.Lazy qualified as LT (pack)
import GHC.Exts (toList)
import Image.LaTeX.Render (defaultEnv, latexFontSize, RenderError(..))
import Image.LaTeX.Render.Pandoc (defaultPandocFormulaOptions, errorDisplay)
import Skylighting (defaultSyntaxMap)
import Skylighting.Parser (addSyntaxDefinition, parseSyntaxDefinitionFromText)
import System.FilePath (dropExtension, dropFileName, takeFileName, (</>))
import Text.Pandoc
  ( Block(..), Format(..), Inline(..), MathType(..), Pandoc(..)
  , WriterOptions(..)
  )
import Text.Pandoc.Highlighting (Style, breezeDark, styleToCss)
import Text.Pandoc.Walk (walk)

import Hakyll
import Hakyll.Contrib.LaTeX (initFormulaCompilerSVG)

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

  create ["css/syntax.css"] $ do
    route   idRoute
    compile $ makeItem $ styleToCss pandocCodeStyle

  match "posts/**" $ do
    route   cleanRoute
    processLatex <- preprocess initLatexCompiler
    compile $ pandocCompiler' processLatex
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  match "index.html" $ do
    route   idRoute
    compile $ getResourceBody
      >>= applyAsTemplate listCtx
      >>= loadAndApplyTemplate "templates/default.html" listCtx
      >>= relativizeUrls

  match "about.html" $ do
    route   cleanRoute
    compile $ getResourceBody
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
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

  match "syntax/*" $ compile getResourceString

--------------------------------------------------------------------------------

cleanRoute :: Routes
cleanRoute = customRoute $ (</> "index.html") . dropExtension . toFilePath

--------------------------------------------------------------------------------

pandocCodeStyle :: Style
pandocCodeStyle = breezeDark

pandocCompiler' :: (Pandoc -> Compiler Pandoc) -> Compiler (Item String)
pandocCompiler' processLatex = do
  syntaxDescriptions <- loadAll "syntax/*" :: Compiler [Item String]
  let parse (Item itemId syntaxDesc) =
        parseSyntaxDefinitionFromText (toFilePath itemId) (LT.pack syntaxDesc)
  syntaxDefinitions <- traverse (either fail pure . parse) syntaxDescriptions
  let syntaxMap = foldr addSyntaxDefinition defaultSyntaxMap syntaxDefinitions
  pandocCompilerWithTransformM
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
      { writerHighlightStyle = Just pandocCodeStyle
      , writerSyntaxMap = syntaxMap
      }
    $ processLatex . collapsableCodeBlocks

initLatexCompiler :: IO (Pandoc -> Compiler Pandoc)
initLatexCompiler = do
  formulaComp <- initFormulaCompilerSVG 1000 envOptions
  pure $ formulaComp formulaOptions . walk wrapBlock
  where
    envOptions = defaultEnv { latexFontSize = 15 }

    formulaOptions =
      defaultPandocFormulaOptions { errorDisplay = error . displayRenderError }

    displayRenderError (LaTeXFailure str) = "LaTeXFailure: \n" <> str
    displayRenderError (DVISVGMFailure str) = "DVISVGMFailure: \n" <> str
    displayRenderError (IOException ex) = "IOException: \n" <> show ex

    wrapBlock math@(Para [Math DisplayMath _]) = Div ("", ["math"], []) [math]
    wrapBlock other = other

collapsableCodeBlocks :: Pandoc -> Pandoc
collapsableCodeBlocks = walk $ \case
  code@(CodeBlock (_, classes, _) text)
    | "wrap-code" `elem` classes && T.length text > 400
    || length (T.lines text) > 10
    -> Div ("", ["collapsable"], [])
      [ RawBlock (Format "html") $
        "<input type='checkbox' class='collapse'  " <> checked <> " />"
      , code
      ]
    where checked = if "collapsed" `elem` classes then "checked" else ""
  other -> other

--------------------------------------------------------------------------------

postCtx :: Context String
postCtx
  =  dateField "date" "%B %e, %Y"
  <> mapContext stripIndex (urlField "url")
  <> defaultContext
  where
    stripIndex url
      | takeFileName url == "index.html" = dropFileName url
      | otherwise = url

listCtx :: Context String
listCtx
  =  listField "posts" postCtx (recentFirst =<< loadAll "posts/**")
  <> defaultContext

jsonCtx :: Context Value
jsonCtx = Context $ \name _ (Item _ meta) ->
  fromMaybe (failure meta) . getField meta $ splitName name
  where
    getField (Object obj) (n:ns)
      | Just val <- KM.lookup (Key.fromString n) obj = getField val ns
    getField (Object obj) [] = Just
      $ ListField jsonCtx <$> traverse (makeItem . uncurry object) (toList obj)
    getField (Array arr) [] = Just
      $ ListField jsonCtx <$> traverse makeItem (toList arr)
    getField (String txt) [] = Just . pure . StringField $ T.unpack txt
    getField (Number num) [] = Just . pure . StringField $ show num
    getField (Bool True) [] = Just $ pure EmptyField
    getField (Bool False) [] = Just $ noResult "Field is false"
    getField Null [] = Just $ noResult "Field is null"
    getField _ _ = Nothing

    object key value = Object $ "key" .= key <> "value" .= value 

    splitName ('.':cs) = "" : splitName cs
    splitName (c:cs) = let n:ns = splitName cs in (c:n):ns
    splitName [] = [""]

    failure json = noResult $ "Tried JSON context " ++ BS.unpack (encode json)
