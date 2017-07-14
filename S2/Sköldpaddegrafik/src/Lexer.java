import java.io.DataInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

//TODO Try to use array instead of arraylist for better performance
//TODO Figure out why it wont print tokens

/**
 * Lexer that reads input through a DataInputStream and
 * creates list of tokens from the input
 */
public class Lexer {

    private DataInputStream in;
    private byte[] buff;
    private int numBytesRead;
    private List<Token> tokenList;

    public Lexer() throws SyntaxException, IOException {
        in = new DataInputStream(System.in);
        tokenList = new ArrayList<>();
        buff = new byte[1048576];
        numBytesRead = 0;

        tokenize(readIntoBuffer());
    }

    private String readIntoBuffer() throws IOException {
        int offset = 0;

        while (true) { // Keep read until EOF
            numBytesRead = in.read(buff);

            if (numBytesRead == -1) break;
            else offset += numBytesRead;
        }

        return new String(buff, 0, offset); // Return the input in a string
    }

    private void tokenize(String input) throws SyntaxException {
        int index = 0;
        int line = 1;
        Pattern regex = Pattern.compile("" +
                "%.*" +
                "|up" +
                "|down" +
                "|(?<=^|.|\\\"|\\s)forw(?=\\s+[0-9]+|(\\s*%.*\\n\\s*)+[0-9]+)" +
                "|(?<=^|.|\\\"|\\s)back(?=\\s+[0-9]+|(\\s*%.*\\n\\s*)+[0-9]+)" +
                "|(?<=^|.|\\\"|\\s)left(?=\\s+[0-9]+|(\\s*%.*\\n\\s*)+[0-9]+)" +
                "|(?<=^|.|\\\"|\\s)right(?=\\s+[0-9]+|(\\s*%.*\\n\\s*)+[0-9]+)" +
                "|(?<=^|.|\\\"|\\s)color(?=\\s+#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})" +
                "|(\\s*%.*\\n\\s*)+#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3}))" +

                "|(?<=^|.|\\\"|\\s)rep(?=\\s+[0-9]+\\s+|(\\s*%.*\\n\\s*)+[0-9]+(\\s*%.*\\n)*\\s*)" +
                "|#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})" +
                "|(?<=\\s|\\.|\\\")\\\"" +
                "|\\." +
                "|[0-9]+" +
                "|\\n" +
                "|\\s+", Pattern.CASE_INSENSITIVE);

        Matcher m = regex.matcher(input);

        while (m.find()) {
            if (m.start() != index) {
                throw new SyntaxException("Syntaxfel på rad " + line);
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
                    tokenList.add(new Token(TokenType.ENDOFEXPRESSION, null, line));
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
                        tokenList.add(new Token(TokenType.NUM, Double.parseDouble(m.group()), line));
                    }
            }

            index = m.end();
        }
    }

    public List<Token> getTokens() {
        return tokenList;
    }
}
