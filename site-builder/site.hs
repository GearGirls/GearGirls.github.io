--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid              (mappend)
import           GitHub.Endpoints.Issues.Milestones
import           Hakyll
import qualified Data.Vector as Vector
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Github
--------------------------------------------------------------------------------
cfg :: Configuration
cfg = defaultConfiguration {deployCommand = "rsync -av ./_site/ ../"}

main :: IO ()
main = do
  (Right gear) <- milestones "geargirls" "fll"
  hakyllWith cfg $ do
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
    match "org-sections/*" $ compile $ pandocCompiler >>=
            loadAndApplyTemplate "templates/section.html" (constField "title" "" `mappend` defaultContext)
    create ["Milestones"] $ compile $ do
          section <- loadBody "templates/section.html"
          milestonesCompiler (Vector.toList gear) >>=
                    applyTemplate section (constField "title" "Work needing to be done" `mappend` defaultContext)
    match "index.html" $ do
        route idRoute
        compile $ do
            videos <- load "org-docs/videos.org"
            agenda <- load "org-sections/meeting-planner.org"
            tutorials <- load "org-docs/tutorials.org"
            documents <- load "org-docs/documents.org"
            milestones <- load "Milestones"
            let indexCtx =
                    itemField "videos" videos `mappend`
                    itemField "agenda" agenda `mappend`
                    itemField "Milestones" milestones `mappend`
                    itemField "tutorials" tutorials `mappend`
                    itemField "documents" documents `mappend`
                    constField "title" "Gear Girls Info" `mappend`
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


-- agendaField :: Item String -> Context String
itemField lbl i = field lbl (const $ return . itemBody $ i)
