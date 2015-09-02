--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid            (mappend)
import           Hakyll
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
--------------------------------------------------------------------------------
cfg :: Configuration
cfg = defaultConfiguration {deployCommand = "rsync -av ./_site/ ../"}

main :: IO ()
main = hakyllWith cfg $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler
    match "font-awesome/**" $ do
        route   idRoute
        compile copyFileCompiler
    match "font/*" $ do
        route   idRoute
        compile copyFileCompiler
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler
    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
    match "org-docs/*" $ compile $ pandocCompiler
    match "index.html" $ do
        route idRoute
        compile $ do
            agenda <- load "org-docs/meeting-planner.org"
            let indexCtx =
                    agendaField agenda `mappend`
                    constField "title" "Gear Girls Info"                `mappend`
                    defaultContext
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext


agendaField :: Item String -> Context String
agendaField i = field "agenda" (const $ return . itemBody $ i)
