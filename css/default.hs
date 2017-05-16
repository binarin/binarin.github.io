{-# LANGUAGE OverloadedStrings #-}
import Clay
import qualified Clay.Media as Media
import Data.Monoid
import Prelude hiding ((**))

headerBackground = rgb 170 170 255
bigBreakpointSize = px 960

fullWidthContainer = do
  width bigBreakpointSize
  sameMargin auto

bigBreakpoint = do
    query Media.screen [Media.minWidth bigBreakpointSize] $
          ".main-header" <> ".content" <> ".main-footer" ? fullWidthContainer

mainHeader = do
    ".main-header" ? do
        background headerBackground
        display flex
        samePadding $ px 20
    ".logo" ? do
        width $ pct 30
    ".top-nav" ? do
        width $ pct 70
    ".top-nav__items" ? do
       display inline
    ".top-nav__item" ? do
       display inline
       listStyleType none

samePadding x = padding x x x x
sameMargin x = margin x x x x

mainFooter = do
    ".main-footer" ? do
        samePadding $ px 20
        background headerBackground

postList = do
    ".posts" ? do
      listStyleType none
    ".posts__item-title" ? do
      fontSize $ px 18
      marginBottom $ px 0
      color black
    ".posts__item-date" ? do
      fontSize $ px 14
      fontColor $ rgb 100 100 100
      fontStyle italic
      marginTop $ px 0
      marginBottom $ px 0
    ".posts__item-metadata" ? do
      textAlign $ alignSide sideRight
    ".posts__item-read-more" ? do
      fontSize $ px 14
      color orange
      fontWeight bold
      textAlign $ alignSide sideRight

post = do
    ".post-image" ? do
        width $ pct 100
        maxWidth $ px 480

main :: IO ()
main = putCss $ do
     html ? do
       overflowY scroll
     body ? do
       fontFamily [] [sansSerif]
       fontSize $ px 18
     a ? do
       color blue
       textDecoration none
     mainHeader
     mainFooter
     post
     postList
     bigBreakpoint
