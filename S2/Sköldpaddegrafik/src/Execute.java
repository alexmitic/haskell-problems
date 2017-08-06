import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.List;

/**
 * Created by Alex on 2017-07-24.
 */
public class Execute {
    List<ParseTree> nodes;
    private String color = "#0000FF";
    private boolean isDown = false;
    private double angle = 0;
    double x = 0;
    double y = 0;
    int index = 0;
    DecimalFormat df;
    DecimalFormatSymbols dot;

    public Execute(List<ParseTree> nodes) {
        dot = new DecimalFormatSymbols();
        dot.setDecimalSeparator('.');
        df = new DecimalFormat("0.0000", dot);

        this.nodes = nodes;

        while (emptyList()) {
            ParseTree node = nextNode();
            switch (node.show()) {
                case "UP":
                    upNode();
                    break;
                case "DOWN":
                    downNode();
                    break;
                case "FORW":
                    forwNode((ForwNode) node);
                    break;
                case "BACK":
                    backNode((BackNode) node);
                    break;
                case "LEFT":
                    leftNode((LeftNode) node);
                    break;
                case "RIGHT":
                    rightNode((RightNode) node);
                    break;
                case "COLOR":
                    colorNode((ColorNode) node);
                    break;
            }
            index++;
        }
    }

    private boolean emptyList() {
        return (nodes.size() != index);
    }

    private ParseTree nextNode() {
        return nodes.get(index);
    }

    private void print(double x, double y) {
        System.out.println(color + " " + df.format(this.x) + " " + df.format(this.y)
                    + " " + df.format(x) + " " + df.format(y));
        this.x = x;
        this.y = y;
    }

    private void upNode() {
        isDown = false;
    }

    private void downNode() {
        isDown = true;
    }

    private void forwNode(ForwNode node) {
        if (isDown) {
            double tX = (x + node.data * Math.cos(Math.PI * angle / 180));
            double tY = (y + node.data * Math.sin(Math.PI * angle / 180));

            print(tX, tY);
        } else {
            x = (x + node.data * Math.cos(Math.PI * angle / 180));
            y = (y + node.data * Math.sin(Math.PI * angle / 180));
        }

    }

    private void backNode(BackNode node) {
        if (isDown) {
            double tX = (x - node.data * Math.cos(Math.PI * angle / 180));
            double tY = (y - node.data * Math.sin(Math.PI * angle / 180));

            print(tX, tY);
        } else {
            x = (x - node.data * Math.cos(Math.PI * angle / 180));
            y = (y - node.data * Math.sin(Math.PI * angle / 180));
        }

    }

    private void leftNode(LeftNode node) {
        angle += node.data;
    }

    private void rightNode(RightNode node) {
        angle -= node.data;
    }

    private void colorNode(ColorNode node) {
        color = node.data;
    }
}
