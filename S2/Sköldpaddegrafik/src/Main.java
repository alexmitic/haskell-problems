import java.io.DataInputStream;
import java.io.IOException;
import java.util.List;

public class Main {
    public static void main(String[] args) throws IOException, SyntaxException {
        Lexer lexer = new Lexer(new DataInputStream(System.in));

        List<Token> l = lexer.getTokens();

        for (int i = 0; i < l.size(); i++) {
            System.out.println(l.get(i).getType().toString() + " " + l.get(i).getLine());
        }
    }
}
