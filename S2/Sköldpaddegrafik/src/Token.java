public class Token {

    private final TokenType type;
    private final Object literal;
    private final int line;

    public Token(TokenType type, Object literal, int line) {
        this.type = type;
        this.literal = literal;
        this.line = line;
    }

    // Getter for token attributes
    public TokenType getType() {
        return type;
    }

    public Object getLiteral() {
        return literal;
    }

    public int getLine() {
        return line;
    }

}
