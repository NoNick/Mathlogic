public class Consequence extends Expression{
    private Expression left, right = null;

    public Consequence(Expression left, Expression right) {
        super(left, right);
    }

    @Override
    protected String getSymbol() {
        return "->";
    }
}
