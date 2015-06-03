{-

identify-public
By Gregory W. Schwartz

Takes a list of fasta files and returns the sequences that are shared
between files (public) and unique between files (private).

-}

-- Built-in
import qualified System.IO as IO

-- Cabal
import Options.Applicative
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Fasta.ByteString.Lazy
import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PB

-- Local
import Overlap

-- Command line arguments
data Options = Options { input               :: String
                       , output              :: String
                       }

-- Command line options
options :: Parser Options
options = Options
      <$> strOption
          ( long "input"
         <> short 'i'
         <> metavar "FILE FILE FILE ..."
         <> value ""
         <> help "The list of fasta file paths separated by spaces (with\
                 \ no spaces in the names)" )
      <*> strOption
          ( long "output"
         <> short 'o'
         <> metavar "FILE"
         <> value ""
         <> help "The output fasta file with an additional field appended\
                 \ which includes the number of files the sequence appears in" )

getSequences :: String -> IO [FastaSequence]
getSequences file = do
    h <- IO.openFile file IO.ReadMode
    runEffect . P.toListM . pipesFasta . PB.fromHandle $ h

identifyPublic :: Options -> IO ()
identifyPublic opts = do
    let inputList = words . input $ opts

    sequenceFileList <- mapM getSequences inputList

    let overlapMap = getOverlapMapFromFastaSequences sequenceFileList
        fastaList  = map (assignOverlap overlapMap) . concat $ sequenceFileList
        result     = C.unlines . map showFasta $ fastaList

    -- Save results
    if null . output $ opts
        then C.putStrLn result
        else C.writeFile (output opts) result

main :: IO ()
main = execParser opts >>= identifyPublic
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Takes a list of fasta files and returns the sequences\
                 \ that are shared between files (public) and unique between\
                 \ files (private)."
     <> header "identify-public, Gregory W. Schwartz" )
