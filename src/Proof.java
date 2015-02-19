import java.util.ArrayList;

public class Proof {
    protected static String axiomsText[] = new String[]{
            "A -> (B -> A)",
            "(A->B)->(A->B->C)->(A->C)",
            "(A & B) -> A",
            "(A & B) -> B",
            "A -> (B -> (A & B))",
            "A -> (A | B)",
            "B -> (A | B)",
            "(A -> C) -> ((B -> C) -> (A | B -> C))",
            "(A -> B) -> ((A -> !B) -> !A)",
            "!!A->A"
    };
    private ArrayList<Line> proven = new ArrayList<Line>();
    protected ArrayList<Expression> axioms = new ArrayList<Expression>();

    public Proof() {
        for (String s: axiomsText) {
            axioms.add(ExpressionFactory.parse(s));
        }
    }

    // returns number of an axiom in [1 .. axioms.size()] if X is one of them or -1 otherwise
    protected int isAxiom(Expression X) {
        for (Expression a: axioms) {
            if (X.matches(a)) {
                return axioms.indexOf(a) + 1;
            }
        }
        return -1;
    }

    // returns two numbers of expressions in [0 .. proven.size() - 1] if X is result of MP or null otherwise
    // if at least one of expressions used by MP is unproven, MP wouldn't be found, returns next MP or null.
    protected int[] isMP(Expression X) {
        for (Line p: proven) {
            Expression pe = p.expression;
            if (pe instanceof Consequence && pe.right.equals(X) && p.type != Type.NOT_PROVEN) {
                Expression left = pe.left;
                for (Line p1: proven) {
                    Expression pe1 = p1.expression;
                    if (left.equals(pe1)) {
                        if (p1.type != Type.NOT_PROVEN) {
                            return new int[]{proven.indexOf(p1), proven.indexOf(p)};
                        }
                    }
                }

            }
        }

        return null;
    }

    // comment next line
    public Line addLine(Expression line) {
        int axiom = isAxiom(line);
        if (axiom != -1) {
            proven.add(new Line(Type.AXIOM, line, new int[]{axiom}));
            return proven.get(proven.size() - 1);
        }

        int mp[] = isMP(line);
        if (mp != null) {
            proven.add(new Line(Type.MODUS_PONENS, line, mp));
            return proven.get(proven.size() - 1);
        }

        proven.add(new Line(Type.NOT_PROVEN, line, null));
        return proven.get(proven.size() - 1);
    }
}
