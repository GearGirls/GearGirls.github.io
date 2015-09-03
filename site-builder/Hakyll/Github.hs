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


import           Data.Maybe                      (fromMaybe)
import           Data.Monoid                     (mconcat)
import           Data.Time
import           Github.Issues.Milestones
import           Hakyll
import           Text.Blaze.Html.Renderer.Pretty
import           Text.Blaze.XHtml5
import           Text.Blaze.XHtml5.Attributes
import           Text.Pandoc.Readers.Markdown

milestonesCompiler :: [Milestone] -> Compiler (Item String)
milestonesCompiler = makeItem . renderTableMilestones

renderTableMilestones :: [Milestone] -> String
renderTableMilestones = renderHtml . tableMilestones

testMilestone :: Milestone
testMilestone = Milestone owner dueDate 3 43 0 description title url fixedDate state
  where
    owner = GithubUser "my baby" "takes the" "morning train" 3 Nothing
    dueDate@(Just fixedDate) = fmap GithubDate . parseTimeM True defaultTimeLocale
                                                            (iso8601DateFormat  (Just "%H:%M:%S")) $ "2015-03-20T03:00:00" :: Maybe GithubDate
    description = Just "Here is a brilliant description"
    title = "Test Milestone"
    url = "test url"
    state = "Standard"

renderGithubDate :: GithubDate -> String
renderGithubDate = formatTimeISO.fromGithubDate
   where
      formatTimeISO = formatTime defaultTimeLocale (iso8601DateFormat  (Just "%H:%M:%S"))
tableMilestones milestones = tableWithAttrs .tbody.mappend header .mconcat . fmap (trWithAttrs.tdMilestone) $ milestones
   where
     trWithAttrs = tr ! class_ "text-left"
     tableWithAttrs = table ! class_ "table-striped table"
     header = mconcat . fmap (th . toHtml ) $ (["Number","Title","Due date","Url","State"]:: [String])
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
                    , toHtml milestoneTitle
                    , toHtml.maybe "" renderGithubDate $ milestoneDueOn
                    , toHtml milestoneUrl
                    , toHtml milestoneState]
