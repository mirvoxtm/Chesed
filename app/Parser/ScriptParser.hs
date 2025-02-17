module Parser.ScriptParser where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Parser.ScriptAst
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = void $ many (char ' ' <|> char '\t')

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

quotedString :: Parser String
quotedString = char '"' >> manyTill L.charLiteral (char '"')

parseBackground :: Parser VNCommand
parseBackground = do
    _ <- string "(bg | "
    sc
    bg <- manyTill anySingle (try (sc *> string ")"))
    return (VNSetBackground bg)

parseHide :: Parser VNCommand
parseHide = do
    _ <- string "(hide)"
    return VNHide

parseMusic :: Parser VNCommand
parseMusic = do
    _ <- string "(music | "
    sc
    mus <- manyTill anySingle (try (sc *> string ")"))
    return (VNMusic mus)

parseStopMusic :: Parser VNCommand
parseStopMusic = do
    _ <- string "(stopmusic)"
    return VNStopMusic


parseShow :: Parser VNCommand
parseShow = do
    _ <- string "(show | "
    sc
    name <- manyTill anySingle (try (sc *> string "|"))
    sc
    posx <- manyTill anySingle (try (sc *> string "|"))
    sc
    posy <- manyTill anySingle (try (sc *> string "|"))
    sc
    scalex <- manyTill anySingle (try (sc *> string "|"))
    sc
    scaley <- manyTill anySingle (try (sc *> string ")"))
    sc
    path <- manyTill anySingle (try (sc *> string "(/show)"))
    return (VNShowCharacter name posx posy scalex scaley path)

parseSay :: Parser VNCommand
parseSay = do
  speaker <- lexeme (some (letterChar <|> spaceChar))
  dialogue <- lexeme quotedString
  return (VNSay speaker dialogue)

parseCommand :: Parser VNCommand
parseCommand = try parseBackground <|> try parseShow <|> parseHide <|> parseStopMusic <|> parseMusic <|> parseSay 

parseScript :: Parser [VNCommand]
parseScript = sepBy parseCommand (char '\n')