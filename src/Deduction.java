import com.sun.istack.internal.NotNull;

import java.util.Arrays;

public class Deduction extends Proof{
    // premise, A |- B => premise |- A -> B
    Expression A, B;
    Expression[] premise;

    // premise |- result
    public Deduction(Expression[] premise, @NotNull Expression result) {
        super();

        this.premise = premise;
        if (premise == null) {
            A = null;
        } else {
            axioms.addAll(Arrays.asList(premise).subList(0, premise.length - 1));
            A = premise[premise.length - 1];
        }
        B = result;
    }

    // premise |- A -> B
    public String getHeader() {
        String result = "";
        for (Expression p: Arrays.asList(premise).subList(0, premise.length - 1)) {
            if (!result.equals(""))
                result += ",";
            result += p.toString();
        }
        result += "|-" + (new Consequence(A, B)).toString();
        return result;
    }

    // proof for X->X
    public Line[] XEnsueX(Expression X, int offset) {
        return new Line[] {
                // 2nd axiom
                // (X->(X->X)) -> (X->(X->X)->X) -> (X->X)
                new Line(Type.AXIOM, new Consequence(new Consequence(X, new Consequence(X, X)),
                        new Consequence(new Consequence(X, new Consequence(new Consequence(X, X), X)), new Consequence(X, X))),
                        new int[]{2}),
                // 1st axiom
                // X->X->X
                new Line(Type.AXIOM, new Consequence(X, new Consequence(X, X)), new int[]{1}),
                // M.P. two last
                // (X->(X->X)->X)->(X->X)
                new Line(Type.MODUS_PONENS, new Consequence(new Consequence(X, new Consequence(new Consequence(X, X), X)),
                        new Consequence(X, X)), new int[]{offset, offset + 1}),
                // 1st axiom
                // X->(X->X)->X
                new Line(Type.AXIOM, new Consequence(X, new Consequence(new Consequence(X, X), X)), new int[]{1}),
                // M.P. two last
                // X->X
                new Line(Type.MODUS_PONENS, new Consequence(X, X), new int[]{offset + 2, offset + 3})
        };
    }

    // proof for A->Ci, proven.get(mp[0]) is A->Cj, proven.get(mp[1]) is A->(Cj->Ci)
    private Line[] deductiveMP(Expression A, @NotNull int[] mp, int offset) {
        Expression Cj = proven.get(mp[1]).expression.right, CjCi = proven.get(mp[0]).expression.right, Ci = CjCi.right;
        return new Line[] {
                // 2nd axiom
                // (A -> Cj) -> (A -> (Cj -> Ci)) -> (A -> Ci)
                new Line(Type.AXIOM, new Consequence(new Consequence(A, Cj),
                        new Consequence(new Consequence(A, CjCi), new Consequence(A, Ci))), new int[]{2}),
                // M.P. mp[1], last statement
                // (A -> (Cj -> Ci)) -> (A -> Ci)
                new Line(Type.MODUS_PONENS, new Consequence(new Consequence(A, CjCi), new Consequence(A, Ci)),
                                                                                    new int[]{mp[1], offset}),
                // M.P. mp[0], last statement
                // A->Ci
                new Line(Type.MODUS_PONENS, new Consequence(A, Ci), new int[]{mp[0], offset + 1})
        };
    }

    // returns number of lines (A->(Cj->Ci)) and A->Cj or null if there aren't such lines
    private int[] isDeductiveMP(Expression Ci) {
        for (Line p: proven) {
            Expression pe = p.expression;
            if (pe instanceof Consequence && pe.left.equals(A)
                    && pe.right instanceof Consequence && pe.right.right.equals(Ci)) {
                Expression Cj = pe.right.left;
                for (Line p1: proven) {
                    Expression pe1 = p1.expression;
                    if (pe1 instanceof Consequence && pe1.left.equals(A) && pe1.right.equals(Cj))
                        return new int[]{proven.indexOf(p), proven.indexOf(p1)};
                }
            }
        }

        return null;
    }

    // X->A where A is the axiom #n
    public Line[] XEnsueAxiom(Expression X, Expression A, int n, int offset) {
        return new Line[]{
                // nth axiom
                new Line(Type.AXIOM, A, new int[]{n}),
                // 1st axiom
                new Line(Type.AXIOM, new Consequence(A, new Consequence(X, A)), new int[]{1}),
                // MP two last
                new Line(Type.MODUS_PONENS, new Consequence(X, A), new int[]{offset, offset + 1})
        };
    }

    public Line[] nextLine(@NotNull Expression line) throws Exception {
        int axiom, mp[];
        if (line.equals(A)) {
            Line[] result = XEnsueX(line, proven.size());
            proven.addAll(Arrays.asList(result));
            return result;
        } else if ((axiom = isAxiom(line)) != -1) {
            Line[] result = XEnsueAxiom(A, line, axiom, proven.size());
            proven.addAll(Arrays.asList(result));
            return result;
        } else if ((mp = isDeductiveMP(line)) != null) {
            Line[] result = deductiveMP(A, mp, proven.size());
            proven.addAll(Arrays.asList(result));
            return result;
        }/* else {
            throw new Exception("Couldn't proceed expression " + line.toString());
        }*/
        return null;
    }
}