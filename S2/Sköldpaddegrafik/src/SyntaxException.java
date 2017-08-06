/**
 * Created by Alex on 2017-07-14.
 */
public class SyntaxException extends Exception {
    private int col;

    public SyntaxException(String message) {
        super(message);
    }
}
