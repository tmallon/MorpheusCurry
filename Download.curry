-- This module contains functions to download and optionally store Morpheus XML data.

-- Compiled by: KICS2 Version 0.4.0 of 2015-03-06
-- http://www-ps.informatik.uni-kiel.de/currywiki/

module Download where

import URL(getContentsOfUrl)
import IO(IOMode(..), openFile, hGetContents, hPutStrLn, hClose, hPutStr)
import Directory(doesDirectoryExist, createDirectory, doesFileExist)

-- Synonyms

type Language = String
type Word = String
type XmlText = String

-- URL construction
morpheusBase :: String
morpheusBase = "http://www.perseus.tufts.edu/hopper/xmlmorph?lang="

morpheusUrl :: Language -> Word -> String
morpheusUrl lang word = morpheusBase ++ lang ++ "&lookup=" ++ word

-- Note: if the word is Greek, it must be in the form that is accepted by Morpheus,
-- which is Beta Code without diacritics.
-- Example: instead of a)nqrw/pw|, anqrwpw.

-- Get and return the XML document as a String.
fetch :: Language -> Word -> IO XmlText
fetch lang word = getContentsOfUrl $ morpheusUrl lang word

store :: Language -> Word -> XmlText -> IO()
store lang word text = do h <- openFile ("library/" ++ lang ++ "/" ++ word ++ ".xml") WriteMode
                          hPutStr h text
                          hClose h

-- Ensure that the directory structure needed for storing Morpheus data locally exists.
-- store and download will fail unless the structure is present in your working directory.

ensureLibrary :: IO ()
ensureLibrary = do b <-doesDirectoryExist "library"
                   if b then return () 
                        else do createDirectory "library"
                                createDirectory "library/la"
                                createDirectory "library/greek"

-- Fetch and store and return the XML text.
download :: Language -> Word -> IO XmlText
download lang word = do t <- getContentsOfUrl $ morpheusUrl lang word
                        store lang word t
                        return t

-- Try to read the XML data from the library. If it is not found download it.                        

readOrDownload :: Language -> Word -> IO XmlText
readOrDownload lang word =  do b <- doesFileExist ("library/" ++ lang ++ "/" ++ word ++ ".xml")
                               if b then readFile ("library/" ++ lang ++ "/" ++ word ++ ".xml") 
                                    else download lang word



