
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import static java.lang.Character.isLetter;

/**
 * @author Nehliin
 * @version 2017-04-05
 *
 * TO-DO:
 * REMOVE MAX FILE SIZE IN HANDLE IMPORTS
 * REWEITE TESTS SO THEY TEST COLUMNS
 *
 */
public class Lexer {


    private static final Map<String, TokenType> keywords;


    static {
        keywords = new HashMap<>();
        keywords.put("FROW",  TokenType.FORW);
        keywords.put("BACK",  TokenType.BACK);
        keywords.put("LEFT",   TokenType.LEFT);
        keywords.put("RIGHT",  TokenType.RIGHT);
        keywords.put("UP",    TokenType.UP);
        keywords.put("DOWN",   TokenType.DOWN);
        keywords.put("COLOR",   TokenType.COLOR);
        keywords.put("REP",  TokenType.REP);
    }


    private final ArrayList<Token> tokens = new ArrayList();
    private String sourceCode;
    private int startChar = 0;
    private int currentChar = 0;
    private int line = 1;

    public Lexer(String sourceCode) throws Exception{
        this.sourceCode = sourceCode;
    }

    /**
     * Generates an ArrayList of Tokens from the given source file with an End Of File token in the end.
     * @return ArrayList<Token>
     * */
    public ArrayList<Token> scanTokens() throws Exception{
        startChar = 0; //must be reset after handling imports
        currentChar = 0; //must be reset after handling imports

        while (!isAtEnd()) {
            startChar = currentChar;
            scanToken();
        }

        tokens.add(new Token(TokenType.END_OF_INPUT,null, line));

        return tokens;
    }


    /**
     * Scans the source code for individual tokens and adds them to the tokens ArrayList.
     *  @return the token added
     * */
    private Token scanToken() throws Exception{
        char c = advance();
        switch (c) {
            case '.': return addToken(TokenType.END_OF_EXPRESSION);
            case '%':
                // A comment goes until the end of the line.
                while (peek() != '\n' && !isAtEnd()) advance();
                break;
            case ' ':
            case '\r':
            case '\t':
                // Ignore whitespace.
                break;

            case '\n':
                line++;//taken care of in the addToken method
                break;

            case '"': return addToken(TokenType.QUOTE);

            case 'FORW' =

            default:
                if (isDigit(c)) {
                    return number();
                } else if (isWord(c)) {
                    return identifier();
                } else {
                    if(c != '\u0000'){
                        throw new SyntaxException("Syntaxfel på rad" + line);
                    }
                }
                return null;
        }
        return null;
    }

    /**
     *  This method searches for keywords and identifiers and add them to the "tokens" ArrayList.
     *  Identifiers are things like variable names for example and is not to be confused with other literals i.e the number 12 or the string "hello".
     *  This method ignores IMPORT statements that isn't in the beginning of the file.
     *  @return the token added
     * */
    private Token identifier() {
        while (isWordNumeric(peek())) advance();
        String text = sourceCode.substring(startChar, currentChar);
        TokenType type = keywords.get(text);
        return addToken(type);

    }

    /**
     * This method searches for number literals and add them to the "tokens" ArrayList as doubles
     * @return the token added
     * */
    private Token number() {
        while (isDigit(peek())) advance();
        return addToken(TokenType.NUM, Double.parseDouble(sourceCode.substring(startChar, currentChar)));
    }

    private Token string() throws Exception {
        while (peek() != '"' && !isAtEnd()) {
            if (peek() == '\n') line++;
            advance();
        }

        // Unterminated string.
        if (isAtEnd()) {
            //SyntaxError.sendError(line,column,ErrorType.MISSING_CLOSING_QUOTATION_MARK);
            return null;
        }

        // The closing ".
        advance();

        // Trim the surrounding quotes.
        String value = sourceCode.substring(startChar + 1, currentChar - 1);
        return addToken(TokenType.HEX, value);
    }

    /**
     * This peeks the char without advancing.
     * @return the char if there exists one else '\0'
     * */
    private char peek() {
        if (currentChar >= sourceCode.length()) return '\0';
        return sourceCode.charAt(currentChar);
    }


    /**
     * controls if the char is part of the alphabet or '_'
     * @return if it is or not
     * */
    private boolean isWord(char c) {
        return isLetter(c) || c == '_';
    }

    /**
     * controls if the char is part of the alphabet or '_' OR a number
     * @return if it is or not
     * */
    private boolean isWordNumeric(char c) {
        return isWord(c) || isDigit(c);
    }

    /**
     * controls if the char is a number
     * @return if it is or not
     * */
    private boolean isDigit(char c) {
        return c >= '0' && c <= '9';
    }


    /**
     * controls if the currentChar is at the end of the file or past the end
     * @return if it is or not
     * */
    private boolean isAtEnd() {
        return currentChar >= sourceCode.length();
    }


    /**
     * increments the currentChar and returns the next char in the source code
     * @return the next char in the source code
     * */
    private char advance() {
        currentChar++;
        column++;
        return sourceCode.charAt(currentChar - 1);
    }

    /**
     * Adds a token to the "tokens" ArrayList with null as literal (meaning the token isn't a literal)
     * @param type The token type to be added
     * @return the token added
     * */
    private Token addToken(TokenType type) {
        return addToken(type, null);
    }

    /**
     * Adds a token to the "tokens" ArrayList with null as literal
     * @param type The token type to be added
     * @param literal the literal of the token (NUMBER, STRING or IDENTIFIER)
     * @return the token added
     * */
    private Token addToken(TokenType type, Object literal) {

        String text = sourceCode.substring(startChar, currentChar);
        Token token;

            token = new Token(type, literal, line);

        tokens.add(token);
        return token;
    }

}