public class Conjunction extends Expression{
    public Conjunction(Expression left, Expression right) {
        super(left, right);
    }

    @Override
    protected String getSymbol() {
        return "&";
    }
}
