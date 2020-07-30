{-# language BangPatterns #-}
{-# language DerivingStrategies #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language ViewPatterns #-}

module Main (main) where

import Control.Monad (void)
import Data.Functor.Contravariant (Predicate(..))
import Data.Functor.Identity (Identity(..))
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Text (Text)
import Patience.Map (Delta(..))
import Text.HTML.Parser (Token(..))
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.Extra as Extra
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Patience.Map as Patience
import qualified Text.HTML.Parser as Html
import Options.Applicative as O

main :: IO ()
main = do
  Options{..} <- execParser
    $ info (opts <**> helper)
           (   fullDesc
            <> progDesc "Diff Hydra Jobset Configurations"
            <> header "hjd - diff hydra jobset configurations"
           )
  diff <- Patience.diff
          <$> (htmlToHydraInputs <$> TIO.readFile optionsOld)
          <*> (htmlToHydraInputs <$> TIO.readFile optionsNew)

  let prettyPair :: InputName -> InputValue -> Text
      prettyPair name val = name <> " = " <> val
  let pretty :: Text -> Text -> InputName -> Delta (InputType, InputValue) -> Text
      pretty old new name = \case
        Old (_, val) -> "[" <> old <> " only]: " <> prettyPair name val
        New (_, val) -> "[" <> new <> " only]: " <> prettyPair name val
        Same {} -> ""
        Delta (_, oldVal) (_, newVal) -> "[" <> old <> " || " <> new <> "]: " <> prettyPair name oldVal <> " || " <> prettyPair name newVal
  let myPretty = pretty "old" "new"

  void $ flip Map.traverseWithKey diff
       $ \name delta -> do
           case delta of
             Old {} -> TIO.putStrLn (myPretty name delta)
             New {} -> TIO.putStrLn (myPretty name delta)
             Delta {} -> TIO.putStrLn (myPretty name delta)
             _ -> pure ()

data Options = Options
  { optionsOld :: FilePath
  , optionsNew :: FilePath
  }

opts :: O.Parser Options
opts = Options
  <$> strOption (long "old" <> metavar "HTML" <> help "filepath of old configuration")
  <*> strOption (long "new" <> metavar "HTML" <> help "filepath of new configuration")

getStrictOld :: Delta a -> Maybe a
getStrictOld = \case
  Old a -> Just a
  _ -> Nothing

getOlds :: Ord k => Map k (Delta a) -> Map k a
getOlds = Map.mapMaybe getStrictOld

htmlToHydraInputs :: Text -> HydraInputs
htmlToHydraInputs = id
  . Map.fromList
  . map toInput
  . Extra.chunksOf 3
  . onlyTds
  . transformEmptyText
  . getInputsTable
  . filters
  . Html.parseTokens

filters :: [Token] -> [Token]
filters = filter allFilters
  where
    allFilters :: Token -> Bool
    allFilters = getPredicate $ foldMap Predicate
      [ (not . newlineThenSpaces)
      , (not . isEmptyContent)
      , (not . isTT)
      ]
    newlineThenSpaces :: Token -> Bool
    newlineThenSpaces = \case
      ContentText txt -> case Text.uncons txt of
        Nothing -> False
        Just (h, rem) -> h == '\n' && Text.all Char.isSpace rem
      _ -> False
    isEmptyContent :: Token -> Bool
    isEmptyContent = \case
      ContentText txt -> Text.all Char.isSpace txt
      _ -> False
    isTT :: Token -> Bool
    isTT = \case
      TagOpen "tt" _ -> True
      TagClose "tt" -> True
      _ -> False

getInputsTable :: [Token] -> [Token]
getInputsTable = id
  . fromMaybe (error "no inputs table")
  . betweenTag "table"
  . snd
  . List.break isTable
  . stripInputs
  . snd
  . List.break isInputs
  where
    isInputs :: Token -> Bool
    isInputs = \case
      ContentText "Inputs" -> True
      _ -> False

    isTable :: Token -> Bool
    isTable = \case
      TagOpen "table" _ -> True
      _ -> False

    stripInputs :: [Token] -> [Token]
    stripInputs = \case
      (ContentText "Inputs" : TagClose _ : tokens) -> tokens
      tokens -> tokens

betweenTag :: Text -> [Token] -> Maybe [Token]
betweenTag tag = \case
  (TagOpen openTag _ : tokens)
    | tag == openTag -> List.reverse <$> go [] tokens
    | otherwise -> Nothing
  token -> Nothing
  where
    go !acc = \case
      -- we should never reach the empty list,
      -- we should always hit a corresponding
      -- TagClose on well-formed html
      [] -> Nothing
      (TagClose closeTag : ts)
        | closeTag == tag -> Just acc
        | otherwise -> go (TagClose closeTag : acc) ts
      (t : ts) -> go (t : acc) ts

onlyTds :: [Token] -> [Token]
onlyTds = \case
  [] -> []
  (TagOpen "td" _ : token : TagClose "td" : tokens) -> token : onlyTds tokens
  (notTd : tokens) -> onlyTds tokens

type InputName = Text
data InputType
  = StringValue
  | Boolean
  | PreviousHydraBuild
  | GitCheckout
  deriving stock (Show, Eq)
type InputValue = Text

type Input = (InputName, (InputType, InputValue))
type HydraInputs = Map InputName (InputType, InputValue)

transformEmptyText :: [Token] -> [Token]
transformEmptyText = List.concat . List.reverse . go []
  where
    go :: [[Token]] -> [Token] -> [[Token]]
    go !acc = \case
      [] -> acc
      -- this is very readable and i am a good developer
      (TagOpen "td" tdOpenTags : ContentText "            \"" : TagOpen "span" _ : TagClose "span" : ContentText "\"        " : TagClose "td" : rest) -> go ([TagOpen "td" tdOpenTags, ContentText "", TagClose "td"] : acc) rest
      (TagOpen "td" tdOpenTags : ContentText "            \"" : TagOpen "span" _ : ContentText txt : TagClose "span" : ContentText "\"        " : TagClose "td" : rest) -> go ([TagOpen "td" tdOpenTags, ContentText txt, TagClose "td"] : acc) rest
      (token : tokens) -> go ([token] : acc) tokens

toInput :: [Token] -> Input
toInput = \case
  [ContentText (Text.strip -> inputName), ContentText (parseInputType . Text.toLower . Text.strip -> inputType), ContentText (Text.strip -> inputValue)] -> (inputName, (inputType, inputValue))
  _ -> error "toInput failure"
  where
    parseInputType :: Text -> InputType
    parseInputType = \case
      "string value" -> StringValue
      "boolean" -> Boolean
      "previous hydra build" -> PreviousHydraBuild
      "git checkout" -> GitCheckout
      i -> error $ "Unsupported Input Type: " ++ Text.unpack i

