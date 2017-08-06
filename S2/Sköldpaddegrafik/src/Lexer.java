import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

//TODO Try to use array instead of arraylist for better performance

/**
 * Lexer that reads input through a DataInputStream and
 * creates list of tokens from the input
 */
public class Lexer {
    private List<Token> tokenList;
    InputStream in;

    public Lexer(InputStream in) throws SyntaxException, IOException {
        tokenList = new ArrayList<>();
        this.in = in;
        tokenize(readIntoBuffer());
        tokenList.add(new Token(TokenType.END_OF_INPUT, null, -1));
    }

    private String readIntoBuffer() throws IOException {
        char[] buff = new char[1048576];
        int numRead = 0;
        Reader reader = new InputStreamReader(in);
        StringBuilder sb = new StringBuilder();

        while (true) { // Keep read until EOF
            numRead = reader.read(buff);

            if (numRead == -1) break;
            else sb.append(buff, 0, numRead);
        }
        return sb.toString(); // Return the input in a string
    }

    private void tokenize(String input) {
        int index = 0;
        int line = 1;
        Pattern regex = Pattern.compile("" +
                "%.*" +
                "|up" +
                "|down" +
                "|(?<=^|\\.|\\\"|\\s)forw(?=(\\s+|(\\s*%.*\\n\\s*)+).+)" +
                "|(?<=^|\\.|\\\"|\\s)back(?=(\\s+|(\\s*%.*\\n\\s*)+).+)" +
                "|(?<=^|\\.|\\\"|\\s)left(?=(\\s+|(\\s*%.*\\n\\s*)+).+)" +
                "|(?<=^|\\.|\\\"|\\s)right(?=(\\s+|(\\s*%.*\\n\\s*)+).+)" +
                "|(?<=^|\\.|\\\"|\\s)color(?=(\\s+|(\\s*%.*\\n\\s*)+).+)" +
                "|(?<=^|\\.|\\\"|\\s)rep(?=(\\s+|(\\s*%.*\\n\\s*)+).+)" +
                "|#([0-9a-fA-F]{3}){1,2}" +
                "|(?<=\\s|\\.|\\\")\\\"" +
                "|\\." +
                "|[0-9]+" +
                "|\\n" +
                "|[ \\t\\r]+", Pattern.CASE_INSENSITIVE);

        Matcher m = regex.matcher(input);

        while (m.find()) {
            if (m.start() != index) {
                tokenList.add(new Token(TokenType.INVALID_CHARACTER, null, line));
            }

            switch (m.group().toUpperCase()) {
                case "UP":
                    tokenList.add(new Token(TokenType.UP, null, line));
                    break;
                case "DOWN":
                    tokenList.add(new Token(TokenType.DOWN, null, line));
                    break;
                case "FORW":
                    tokenList.add(new Token(TokenType.FORW, null, line));
                    break;
                case "BACK":
                    tokenList.add(new Token(TokenType.BACK, null, line));
                    break;
                case "LEFT":
                    tokenList.add(new Token(TokenType.LEFT, null, line));
                    break;
                case "RIGHT":
                    tokenList.add(new Token(TokenType.RIGHT, null, line));
                    break;
                case "COLOR":
                    tokenList.add(new Token(TokenType.COLOR, null, line));
                    break;
                case "REP":
                    tokenList.add(new Token(TokenType.REP, null, line));
                    break;
                case "\"":
                    tokenList.add(new Token(TokenType.QUOTE, null, line));
                    break;
                case ".":
                    tokenList.add(new Token(TokenType.END_OF_EXPRESSION, null, line));
                    break;
                case "\n":
                    line++;
                    break;
                default:
                    if (m.group().startsWith("#")) {
                        tokenList.add(new Token(TokenType.HEX, m.group(), line));
                    } else if (m.group().startsWith("%"));
                    else if (m.group().startsWith(" "));
                    else {
                        try {
                            tokenList.add(new Token(TokenType.NUM, Double.parseDouble(m.group()), line));
                        } catch (NumberFormatException e) {
                            tokenList.add(new Token(TokenType.INVALID_CHARACTER, null, line));
                        }
                    }
            }

            index = m.end();
        }

        if (index != input.length()) tokenList.add(new Token(TokenType.INVALID_CHARACTER, null, line));
    }

    public List<Token> getTokens() {
        return tokenList;
    }
}