import java.util.ArrayList;
import java.util.List;

/**
 * Created by Alex on 2017-07-14.
 */
public class Parser {
    List<Token> tokens;
    List<ParseTree> nodes;
    int index;

    public Parser(List<Token> tokens) {
        this.tokens = tokens;
        this.nodes = new ArrayList<>();
        index = 0;
    }

    public List<ParseTree> parse() throws SyntaxException {

        while (!emptyTokenList()) {
            Token currToken = nextToken();
            parseStatement(currToken);
        }

        return nodes;
    }

    private void parseStatement(Token token) throws SyntaxException {
        switch (token.getType()) {
            case UP:
            case DOWN:
                upOrDownNode(token);
                break;
            case FORW:
            case BACK:
                forwOrBackNode(token);
                break;
            case LEFT:
            case RIGHT:
                leftOrRightNode(token);
                break;
            case COLOR:
                colorNode(token);
                break;
            case REP:
                rep(token);
                break;
            case END_OF_INPUT:
                break;
            default:
                throw new SyntaxException("Syntaxfel på rad " +  token.getLine());
        }
    }

    private Token nextToken() {
        return tokens.get(index++);
    }

    private boolean emptyTokenList() {
        return (index == tokens.size());
    }

    private Token peek() {
        return tokens.get(index);
    }

    private Token peek(int forward) {
            return tokens.get(index + forward);
        }

    private void findLastValidToken(Token token) throws SyntaxException {
        int lastValid = token.getLine();
        for (int i = index; i < tokens.size(); i++) {
            Token currToken = tokens.get(i);
            if (currToken.getType() == TokenType.END_OF_INPUT ||
                currToken.getType() == TokenType.QUOTE ||
                currToken.getType() == TokenType.INVALID_CHARACTER ||
                currToken.getType() == TokenType.HEX ||
                currToken.getType() == TokenType.END_OF_EXPRESSION ||
                currToken.getType() == TokenType.NUM);
            else {
                lastValid = currToken.getLine();
            }
        }

        throw new SyntaxException("Syntaxfel på rad " +  lastValid);
    }

    private void findPreviousValidToken() throws SyntaxException {
        for (int i = tokens.size() - 1; i > -1; i--) {
            Token currToken = tokens.get(i);
            if (currToken.getType() == TokenType.END_OF_INPUT);
            else {
                throw new SyntaxException("Syntaxfel på rad " +  currToken.getLine());
            }
        }
    }

    private void upOrDownNode(Token token) throws SyntaxException {
        if (peek().getType() != TokenType.END_OF_EXPRESSION) {
            if (peek().getType() == TokenType.END_OF_INPUT) {
                throw new SyntaxException("Syntaxfel på rad " +  token.getLine());
            } else {
                throw new SyntaxException("Syntaxfel på rad " +  peek().getLine());
            }
        } else {
            if (token.getType() == TokenType.UP) {
                nodes.add(new UpNode());
            } else {
                nodes.add(new DownNode());
            }
            index++;
        }
    }

    private void forwOrBackNode (Token token) throws SyntaxException {
        if (peek().getType() == TokenType.NUM) {
            if ((double) peek().getLiteral() < 1) {
                throw new SyntaxException("Syntaxfel på rad " +  token.getLine());
            }

            if (peek(1).getType() == TokenType.END_OF_EXPRESSION) {
                if (token.getType() == TokenType.FORW) {
                    nodes.add(new ForwNode((double) peek().getLiteral()));
                } else {
                    nodes.add(new BackNode((double) peek().getLiteral()));
                }
                index = index + 2;
            } else {
                if (peek(1).getType() == TokenType.END_OF_INPUT) {
                    throw new SyntaxException("Syntaxfel på rad " +  peek(-1).getLine());
                } else {
                    throw new SyntaxException("Syntaxfel på rad " +  peek(1).getLine());
                }
            }
        } else {
            if (peek().getType() == TokenType.END_OF_INPUT) {
                throw new SyntaxException("Syntaxfel på rad " +  token.getLine());
            } else {
                throw new SyntaxException("Syntaxfel på rad " +  peek().getLine());
            }
        }
    }

    private void leftOrRightNode (Token token) throws SyntaxException {
       if (peek().getType() == TokenType.NUM) {
           if ((double) peek().getLiteral() < 1) {
               throw new SyntaxException("Syntaxfel på rad " +  peek(-1).getLine());
           }

           if (peek(1).getType() == TokenType.END_OF_EXPRESSION) {

               if (token.getType() == TokenType.LEFT) {
                   nodes.add(new LeftNode((double) peek().getLiteral()));
               } else {
                   nodes.add(new RightNode((double) peek().getLiteral()));
               }
               index = index + 2;
           } else {
               if (peek(1).getType() == TokenType.END_OF_INPUT) {
                   throw new SyntaxException("Syntaxfel på rad " +  peek(-1).getLine());
               } else {
                   throw new SyntaxException("Syntaxfel på rad " +  peek(1).getLine());
               }
           }
       } else {
           if (peek().getType() == TokenType.END_OF_INPUT) {
               throw new SyntaxException("Syntaxfel på rad " +  token.getLine());
           } else {
               throw new SyntaxException("Syntaxfel på rad " +  peek().getLine());
           }
       }
    }

    private void colorNode(Token token) throws SyntaxException {
       if (peek().getType() == TokenType.HEX) {
            if (peek(1).getType() == TokenType.END_OF_EXPRESSION) {
                nodes.add(new ColorNode((String) nextToken().getLiteral()));
                index = index + 1;
            } else {
                if (peek(1).getType() == TokenType.END_OF_INPUT) {
                    throw new SyntaxException("Syntaxfel på rad " +  peek(-1).getLine());
                } else {
                    throw new SyntaxException("Syntaxfel på rad " +  peek(1).getLine());
                }
            }

       } else {
           if (peek().getType() == TokenType.END_OF_INPUT) {
               throw new SyntaxException("Syntaxfel på rad " +  token.getLine());
           } else {
               throw new SyntaxException("Syntaxfel på rad " +  peek().getLine());
           }
       }
    }

    private void rep(Token token) throws SyntaxException {
        if (peek().getType() == TokenType.NUM && peek(1).getType() != TokenType.QUOTE) {
            double numTimes = (double) nextToken().getLiteral();

            if (numTimes < 1) {
                throw new SyntaxException("Syntaxfel på rad " + peek(-1).getLine());
            }

            Token next = nextToken();
            int numNodes = 0;

            if (next.getType() == TokenType.REP) {
                numNodes += nestedRep(next);
            } else {
                if (next.getType() == TokenType.END_OF_INPUT) {
                    throw new SyntaxException("Syntaxfel på rad " + peek(-2).getLine());
                }
                numNodes++;
                parseStatement(next);
            }

            for (int i = 0; i < numTimes - 1; i++) {
                for (int j = 0; j < numNodes; j++) {
                    nodes.add(nodes.get(nodes.size() - numNodes));
                }
            }
        } else if (peek().getType() == TokenType.NUM && peek(1).getType() == TokenType.QUOTE) {
            double numTimes = (double) nextToken().getLiteral();
            if (numTimes < 1) {
                throw new SyntaxException("Syntaxfel på rad " + peek(-1).getLine());
            }

            nextToken();

            int numNodes = 0;
            while ((token = nextToken()).getType() != TokenType.QUOTE) {
                if (token.getType() == TokenType.REP) {
                    numNodes += nestedRep(token);
                } else {
                    if (token.getType() == TokenType.END_OF_INPUT) {
                        findPreviousValidToken();
                    }
                    parseStatement(token);
                    numNodes++;
                }
            }

            if (token.getType() == TokenType.QUOTE && numNodes == 0) {
                throw new SyntaxException("Syntaxfel på rad " + token.getLine());
            }

            for (int i = 0; i < numTimes - 1; i++) {
                for (int j = 0; j < numNodes; j++) {
                    nodes.add(nodes.get(nodes.size() - numNodes));
                }
            }
        } else {
            if (peek().getType() == TokenType.END_OF_INPUT) {
                throw new SyntaxException("Syntaxfel på rad " + token.getLine());
            } else {
                throw new SyntaxException("Syntaxfel på rad " + peek().getLine());
            }
        }
    }

    private int nestedRep(Token token) throws SyntaxException {
        int numNodes = 0;
        double numTimes = 0;
        if (peek().getType() == TokenType.NUM && peek(1).getType() != TokenType.QUOTE) {
            numTimes = (double) nextToken().getLiteral();

            if (numTimes < 1) {
                throw new SyntaxException("Syntaxfel på rad " + peek(-1).getLine());
            }

            if (peek().getType() != TokenType.REP) {
                if (peek().getType() == TokenType.END_OF_INPUT) {
                    throw new SyntaxException("Syntaxfel på rad " + peek(-1).getLine());
                }
                numNodes++;
                parseStatement(nextToken());
            } else if (peek().getType() == TokenType.REP) {
                numNodes += nestedRep(nextToken());
            }

            for (int i = 0; i < numTimes - 1; i++) {
                for (int j = 0; j < numNodes; j++) {
                    nodes.add(nodes.get(nodes.size() - numNodes));
                }
            }
        } else if (peek().getType() == TokenType.NUM && peek(1).getType() == TokenType.QUOTE) {
            numTimes = (double) nextToken().getLiteral();

            if (numTimes < 1) {
                throw new SyntaxException("Syntaxfel på rad " + peek(-1).getLine());
            }

            nextToken();

            while ((token = nextToken()).getType() != TokenType.QUOTE) {
                if (token.getType() == TokenType.END_OF_INPUT) {
                    throw new SyntaxException("Syntaxfel på rad " + token.getLine());
                }

                if (token.getType() == TokenType.REP) {
                    numNodes += nestedRep(token);
                } else {
                    if (token.getType() == TokenType.END_OF_INPUT) {
                        findPreviousValidToken();
                    }
                    parseStatement(token);
                    numNodes++;
                }
            }

            if (token.getType() == TokenType.QUOTE && numNodes == 0) {
                throw new SyntaxException("Syntaxfel på rad " + token.getLine());
            }

            for (int i = 0; i < numTimes - 1; i++) {
                for (int j = 0; j < numNodes; j++) {
                    nodes.add(nodes.get(nodes.size() - numNodes));
                }
            }
        } else {
            if (peek().getType() == TokenType.END_OF_INPUT) {
                throw new SyntaxException("Syntaxfel på rad " + token.getLine());
            } else {
                throw new SyntaxException("Syntaxfel på rad " + peek().getLine());
            }
        }

        return numNodes * (int) numTimes;
    }
}