{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}


{- |
Module      :  Hakyll.Github
Description :  Github compilers for Hakyll
Copyright   :  Scott Murphy
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portabl

Use this to render hakyll stuff from github

-}

module Hakyll.Github ( renderTableMilestones
                     ,milestonesCompiler
                     ,tableMilestones
                     ,testMilestone) where


import           Data.Default
import           Data.Maybe                      (fromMaybe)
import           Data.Monoid                     (mconcat)
import           Data.Time
import           GitHub.Endpoints.Issues.Milestones
import qualified Data.Text as Text
import           Hakyll
import           Text.Blaze.Html.Renderer.Pretty
import           Text.Blaze.XHtml5
import qualified Text.Blaze.XHtml5               as H
import           Text.Blaze.XHtml5.Attributes
import           Text.Pandoc.Readers.Markdown
import           Text.Pandoc.Writers.HTML
import           Data.Proxy 


milestonesCompiler :: [Milestone] -> Compiler (Item String)
milestonesCompiler = makeItem . renderTableMilestones

renderTableMilestones :: [Milestone] -> String
renderTableMilestones = renderHtml . tableMilestones

testMilestone :: Milestone
testMilestone = Milestone owner dueDate 3 43 0 description title url fixedDate state
  where
    userid = mkId (Proxy :: Proxy User) 1 
    owner = SimpleUser userid "user name" (URL "takes the") (URL "test") OwnerUser 
    dueDate@(Just fixedDate) = parseTimeM True defaultTimeLocale
                                          (iso8601DateFormat  (Just "%H:%M:%S")) $ "2015-03-20T03:00:00" :: Maybe UTCTime
    description = Just "Here is a brilliant description"
    title = "Test Milestone"
    url = URL "test url"
    state = "Standard"

tableMilestones milestones = tableWithAttrs .tbody.mappend header .mconcat . fmap (trWithAttrs.tdMilestone) $ milestones
   where
     trWithAttrs = tr ! class_ "text-left"
     tableWithAttrs = table ! class_ "table-striped table"
     header = mconcat . fmap (th . toHtml ) $ (["Number","Title","Due date","Description","State"]:: [String])

-- | Render a single table entry milestone
tdMilestone :: Milestone -> Html
tdMilestone mileStone = mconcat dataLine
 where
  (Milestone {milestoneCreator ,
   milestoneDueOn ,
   milestoneOpenIssues ,
   milestoneNumber ,
   milestoneClosedIssues ,
   milestoneDescription ,
   milestoneTitle ,
   milestoneUrl ,
   milestoneCreatedAt ,
   milestoneState }) = mileStone
  dataLine :: [Html]
  dataLine = td <$> [ toHtml milestoneNumber
                    , renderLinkWithTitle (toHtml milestoneTitle) (stringValue . show $ milestoneUrl)
                    , toHtml.maybe "" renderGithubDate $ milestoneDueOn
                    , toHtml.renderDescription . fmap Text.unpack  $ milestoneDescription
                    , (H.span ! class_ "label label-success") . toHtml $ milestoneState]


-- | Render Help

renderGithubDate :: UTCTime -> String
renderGithubDate = formatTimeISO
   where
      formatTimeISO = formatTime defaultTimeLocale (iso8601DateFormat  (Just "%H:%M:%S"))


renderLinkWithTitle title url = a ! href url $ title

renderDescription :: Maybe String -> Html
renderDescription (Nothing) = toHtml (""::String)
renderDescription (Just str)= case fmap (writeHtml def) . readMarkdown def $ str of
                                  (Left _) -> toHtml (""::String)
                                  (Right str) -> toHtml str
