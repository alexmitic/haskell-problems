public enum TokenType {
    // Keywords
    UP, DOWN, FORW, BACK, LEFT, RIGHT, COLOR, REP,

    // Single - character
    QUOTE, HEX, NUM, END_OF_EXPRESSION,

    // Rest
    INVALID_CHARACTER, END_OF_INPUT, ENDLINE
}
