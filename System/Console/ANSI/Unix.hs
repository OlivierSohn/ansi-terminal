{-# OPTIONS_HADDOCK hide #-}

module System.Console.ANSI.Unix (
-- This file contains code that is common to modules
-- System.Console.ANSI.Unix and System.Console.ANSI.Windows, namely the module
-- exports and the associated Haddock documentation.
#include "Exports-Include.hs"
    ) where

import System.Console.ANSI.Codes
import System.Console.ANSI.Types
import System.IO (BufferMode (..), Handle, hFlush, hGetBuffering, hGetEcho,
    hIsTerminalDevice, hPutStr, hSetBuffering, hSetEcho, stdin, stdout)
import Text.ParserCombinators.ReadP (readP_to_S)

-- This file contains code that is common to modules System.Console.ANSI.Unix,
-- System.Console.ANSI.Windows and System.Console.ANSI.Windows.Emulator, such as
-- type signatures and the definition of functions specific to stdout in terms
-- of the corresponding more general functions, inclduding the related Haddock
-- documentation.
#include "Common-Include.hs"
-- This file contains code that is common save that different code is required
-- in the case of the module System.Console.ANSI.Windows.Emulator (see the file
-- Common-Include-Emulator.hs in respect of the latter).
#include "Common-Include-Enabled.hs"

hCursorUp h n = hPutStr h $ cursorUpCode n
hCursorDown h n = hPutStr h $ cursorDownCode n
hCursorForward h n = hPutStr h $ cursorForwardCode n
hCursorBackward h n = hPutStr h $ cursorBackwardCode n

hCursorDownLine h n = hPutStr h $ cursorDownLineCode n
hCursorUpLine h n = hPutStr h $ cursorUpLineCode n

hSetCursorColumn h n = hPutStr h $ setCursorColumnCode n
hSetCursorPosition h n m = hPutStr h $ setCursorPositionCode n m

hSaveCursor h = hPutStr h saveCursorCode
hRestoreCursor h = hPutStr h restoreCursorCode
hReportCursorPosition h = hPutStr h reportCursorPositionCode

hClearFromCursorToScreenEnd h = hPutStr h clearFromCursorToScreenEndCode
hClearFromCursorToScreenBeginning h = hPutStr h clearFromCursorToScreenBeginningCode
hClearScreen h = hPutStr h clearScreenCode

hClearFromCursorToLineEnd h = hPutStr h clearFromCursorToLineEndCode
hClearFromCursorToLineBeginning h = hPutStr h clearFromCursorToLineBeginningCode
hClearLine h = hPutStr h clearLineCode

hScrollPageUp h n = hPutStr h $ scrollPageUpCode n
hScrollPageDown h n = hPutStr h $ scrollPageDownCode n

hSetSGR h sgrs = hPutStr h $ setSGRCode sgrs

hHideCursor h = hPutStr h hideCursorCode
hShowCursor h = hPutStr h showCursorCode

hSetTitle h title = hPutStr h $ setTitleCode title

-- getReportedCursorPosition :: IO String
-- (See Common-Include.hs for Haddock documentation)
getReportedCursorPosition = do
    echo <- hGetEcho stdin -- Preserve existing echo
    hSetEcho stdin False   -- Turn echo off
    cp <- get
    hSetEcho stdin echo    -- Restore echo
    return cp
  where
    get = do
        c <- getChar
        if c == '\ESC'
            then get' [c]
            else return [c] -- If the first character is not the expected
                            -- \ESC then give up. This provides a modicom of
                            -- protection against unexpected data in the
                            -- input stream.
    get' s = do
        c <- getChar
        if c == 'R'
            then return (s ++ [c])
            else get' (s ++ [c]) -- Continue until the expected 'R' character is
                                 -- obtained.

-- getCursorPosition :: IO (Maybe (Int, Int))
-- (See Common-Include.hs for Haddock documentation)
getCursorPosition = do
    buffering <- hGetBuffering stdin -- preserve current buffering
    hSetBuffering stdin NoBuffering -- set no buffering (the contents of the
                                    -- buffer will be discarded, so this needs
                                    -- to be done before the cursor positon is
                                    -- emitted)
    reportCursorPosition
    hFlush stdout -- ensure the report cursor position code is sent to the
                  -- operating system
    input <- getReportedCursorPosition
    hSetBuffering stdin buffering -- restore buffering
    case readP_to_S cursorPosition input of
        [] -> return Nothing
        [((row, col),_)] -> return $ Just (row, col)
        (_:_) -> return Nothing
