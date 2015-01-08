public class Not extends Expression {
    public Not(Expression left) {
        super(left);
    }

    @Override
    protected String getSymbol() {
        return null;
    }

    public String toString() {
        if (left instanceof Variable)
            return "!" + left.toString();
        return "!(" + left.toString() + ")";
    }
}
