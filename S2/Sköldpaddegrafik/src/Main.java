import java.io.IOException;
import java.util.List;

public class Main {
    public static void main(String[] args) throws IOException, SyntaxException {
        try {
            Lexer lexer = new Lexer(System.in);
            Parser parser = new Parser(lexer.getTokens());
            List<ParseTree> l = parser.parse();
            Execute e = new Execute(l);
        } catch (SyntaxException e) {
                System.out.println(e.toString().substring(17));
        }
    }
}


