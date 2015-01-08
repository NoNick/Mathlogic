public class Disjunction extends Expression{

    public Disjunction(Expression left, Expression right) {
        super(left, right);
    }

    @Override
    protected String getSymbol() {
        return "|";
    }
}
