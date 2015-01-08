public class Line {
    Type type;
    Expression expression;
    // number(s) of an axiom or a lines
    int[] reference;

    public Line(Type type, Expression expression, int[] ref) {
        this.type = type;
        this.expression = expression;
        reference = ref;
    }
}