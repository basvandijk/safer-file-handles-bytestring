{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Data.ByteString.SaferFileHandles
-- Copyright   :  (c) 2010 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This module lifts the bytestring IO operations into the region monad.
--
-------------------------------------------------------------------------------

module Data.ByteString.SaferFileHandles
    ( hGetLine
    , hGetContents
    , hGet
    , hGetNonBlocking

    , hPut
    , hPutStr
    , hPutStrLn
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Data.Int ( Int )

-- from transformers:
import Control.Monad.IO.Class ( MonadIO )

-- from bytestring:
import qualified Data.ByteString as B

-- from regions:
import Control.Monad.Trans.Region ( AncestorRegion )

-- from explicit-iomodes-bytestring:
import qualified Data.ByteString.ExplicitIOModes as E ( hGetLine
                                                      , hGetContents
                                                      , hGet
                                                      , hGetNonBlocking

                                                      , hPut
                                                      , hPutStr
                                                      , hPutStrLn
                                                      )

-- from safer-file-handles:
import System.IO.SaferFileHandles        ( FileHandle, ReadModes, WriteModes )
import System.IO.SaferFileHandles.Unsafe ( wrap, wrap2 )


-------------------------------------------------------------------------------
-- ByteString I/O with regional file handles
-------------------------------------------------------------------------------

-- | Wraps: @Data.ByteString.'B.hGetLine'@.
hGetLine ∷ ( FileHandle handle, ReadModes ioMode
           , pr `AncestorRegion` cr, MonadIO cr
           )
         ⇒ handle ioMode pr → cr B.ByteString
hGetLine = wrap E.hGetLine

-- | Wraps: @Data.ByteString.'B.hGetContents'@.
hGetContents ∷ ( FileHandle handle, ReadModes ioMode
               , pr `AncestorRegion` cr, MonadIO cr
               )
             ⇒ handle ioMode pr → cr B.ByteString
hGetContents = wrap E.hGetContents

-- | Wraps: @Data.ByteString.'B.hGet'@.
hGet ∷ ( FileHandle handle, ReadModes ioMode
       , pr `AncestorRegion` cr, MonadIO cr
       )
     ⇒ handle ioMode pr → Int → cr B.ByteString
hGet = wrap2 E.hGet

-- | Wraps: @Data.ByteString.'B.hGetNonBlocking'@.
hGetNonBlocking ∷ ( FileHandle handle, ReadModes ioMode
                  , pr `AncestorRegion` cr, MonadIO cr
                  )
                ⇒ handle ioMode pr → Int → cr B.ByteString
hGetNonBlocking = wrap2 E.hGetNonBlocking

-- | Wraps: @Data.ByteString.'B.hPut'@.
hPut ∷ ( FileHandle handle, WriteModes ioMode
       , pr `AncestorRegion` cr, MonadIO cr
       )
     ⇒ handle ioMode pr → B.ByteString → cr ()
hPut = wrap2 E.hPut

-- | Wraps: @Data.ByteString.'B.hPutStr'@.
hPutStr ∷ ( FileHandle handle, WriteModes ioMode
          , pr `AncestorRegion` cr, MonadIO cr
          )
        ⇒ handle ioMode pr → B.ByteString → cr ()
hPutStr = wrap2 E.hPutStr

-- | Wraps: @Data.ByteString.'B.hPutStrLn'@.
hPutStrLn ∷ ( FileHandle handle, WriteModes ioMode
            , pr `AncestorRegion` cr, MonadIO cr
            )
          ⇒ handle ioMode pr → B.ByteString → cr ()
hPutStrLn = wrap2 E.hPutStrLn


-- The End ---------------------------------------------------------------------
