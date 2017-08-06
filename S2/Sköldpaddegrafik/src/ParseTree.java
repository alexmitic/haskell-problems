abstract class ParseTree {
    abstract String show();
}

class UpNode extends ParseTree {
    public String show() {
        return "UP";
    }
}

class DownNode extends ParseTree {
    public String show() {
        return "DOWN";
    }
}

class ForwNode extends ParseTree {
    double data;
    public ForwNode(double data) {
        this.data = data;
    }

    public String show() {
        return "FORW";
    }
}

class BackNode extends ParseTree {
    double data;
    public BackNode(double data) {
        this.data = data;
    }

    public String show() {
        return "BACK";
    }
}

class LeftNode extends ParseTree {
    double data;
    public LeftNode(double data) {
        this.data = data;
    }

    public String show() {
        return "LEFT";
    }
}

class RightNode extends ParseTree {
    double data;
    public RightNode(double data) {
        this.data = data;
    }

    public String show() {
        return "RIGHT";
    }
}

class ColorNode extends ParseTree {
    String data;
    public ColorNode(String data) {
        this.data = data;
    }

    public String show() {
        return "COLOR";
    }
}
