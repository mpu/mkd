module Main where

import System.Environment(getArgs)
import System.IO
import Control.Applicative
import Control.Monad
import Data.Char(isDigit)

data MdBlock =
    Code String
  | Par String
  | Raw String -- Only inside lists
  | UList [[MdBlock]]
  | OList [[MdBlock]]
  | Title Int String
    deriving (Show)

main = do
    fs <- getArgs
    forM_ fs genHTML
  where
    genHTML inf | outf <- stripExtension inf ++ ".htm" =
        withFile inf ReadMode $ \inh ->
        withFile outf WriteMode $ \outh -> do
            blks <- parseMd <$> hGetContents inh
            hPutStr outh (dumpHTML blks)

stripExtension f = go [] [] f
    where go base ext ('.':cs) = go (ext ++ base) "." cs
          go base ext (c:cs) = go base (c:ext) cs
          go base _ [] = reverse base

-- Indentation functions

spaces l = go 0 l
  where
    go n cs | n >= 4 = (n, cs)
    go n (' ':cs) = go (n+1) cs
    go n ('\t':cs) = go (n+4) cs
    go n cs = (n ,cs)

stripSpaces n cs | n <= 0 = Just cs
stripSpaces n (' ':cs) = stripSpaces (n-1) cs
stripSpaces n ('\t':cs) = stripSpaces (n-4) cs
stripSpaces _ _ = Nothing

skipBlankLines l = (not (null pre), post)
  where (pre, post) = span (== "") l

-- Markdown parsing

data BlockTy = BCode | BTitle Int | BQuote
             | BUList | BOList | BPar
    deriving (Eq)

stripIndent idnt l = go (reverse idnt) l
  where
    go _ "" = Nothing
    go [] l = Just l
    go (BPar : _) l = Just l
    go (BCode : _) l = stripSpaces 4 l
    go (BQuote : bs) l
        | (n, '>':' ':r) <- spaces l, n < 4 = go bs r
        | otherwise = go bs l
    go (BUList : bs) l
        | (n, r) <- spaces l
        = guard (n >= 4) *> go bs r
    go (BOList : bs) l
        | (n, r) <- spaces l
        = guard (n >= 4) *> go bs r

isULBeg (c:' ':cs) | elem c "+*-" = Just cs
isULBeg _ = Nothing

isOLBeg (c:'.':' ':cs) | isDigit c = Just cs
isOLBeg _ = Nothing

getBlockTy l =
    case spaces l of
        (i, cs) | i >= 4 -> (BCode, cs)
        (_, '#':cs) | (n, r) <- titleDepth cs -> (BTitle n, r)
        (_, cs) | Just r <- isULBeg cs -> (BUList, r)
        (_, cs) | Just r <- isOLBeg cs -> (BOList, r)
        (_, cs) -> (BPar, cs)
  where
    titleDepth ('#':cs) | (i, r) <- titleDepth cs = (1+i, r)
    titleDepth cs = (1, cs)

parseBlock idnt (l:ls) | (bty, s) <- getBlockTy l =
    case bty of
        BPar -> getBlk Par (bty:idnt) s ls
        BCode -> getBlk Code (bty:idnt) s ls
        BTitle n -> (Title n s, ls)
        BUList -> getLst UList BUList [] (s:ls)
        BOList -> getLst OList BOList [] (s:ls)
  where
    getBlk k idnt c (l:ls)
        | Just s <- stripIndent idnt l
        = getBlk k idnt (c ++ "\n" ++ s) ls
    getBlk k _ c ls = (k c, ls)

    getLst k lty bss ls =
        let (blocks, rem) = parseBlocks (lty:idnt) [] ls
            (newPar, nextBlk) = skipBlankLines rem
            isItemBlock l = do
                r <- stripIndent idnt l
                let (ty, rem) = getBlockTy r
                rem <$ guard (ty == lty)
        in case nextBlk of
            l:ls | Just r <- isItemBlock l ->
                let bs = if newPar then blocks else rawify blocks
                in getLst k lty (bs:bss) (r:ls)
            _ -> (k (reverse (rawify blocks:bss)), nextBlk)

    rawify [Par p] = [Raw p]
    rawify bs = bs

parseBlocks _ bs [] = (reverse bs, [])
parseBlocks idnt bs ls =
    let (b, rem) = parseBlock idnt ls
        (_, nextBlock) = skipBlankLines rem
    in case nextBlock of
        l:ls | Just r <- stripIndent idnt l ->
            parseBlocks idnt (b:bs) (r:ls)
        _ -> (reverse (b:bs), rem)

parseMd = fst . parseBlocks [] [] . lines

-- HTML output

dumpHTML bs =
    "<html><body>\n"
    ++ concatMap dumpBlock bs
    ++ "</body></html>\n"

dumpBlocks = concatMap dumpBlock

dumpBlock b =
    case b of
        Par s -> tag "p" s
        Code s -> tag "pre" s
        Raw s -> s ++ "\n"
        UList bss -> tag "ul" $ concatMap dumpLi bss
        OList bss -> tag "ol" $ concatMap dumpLi bss
        Title n s -> tag ('h' : show n) s
  where
    tag t x = "<" ++ t ++ ">" ++ x ++ "</" ++ t ++ ">\n"
    dumpLi = tag "li" . dumpBlocks
