import com.sun.istack.internal.NotNull;

import java.util.Map;

public class Conjunction extends Expression{
    // A,B|-A&B
    final static String[] TT = new String[]{
            "A->B->(A&B)",
            "B->(A&B)",
            "(A&B)"};

    // A,!B|-!(A&B)
    final static String[] TF = new String[]{
            "(A&B)->B",
            "(!B)->(A&B)->(!B)",
            "((A&B)->(!B))",
            "((A&B)->B)->((A&B)->(!B))->(!(A&B))",
            "((A&B)->(!B))->(!(A&B))",
            "!(A&B)",
    };

    // !A,B|-!(A&B)
    final static String[] FT = new String[]{
            "(A&B)->A",
            "(!A)->((A&B)->(!A))",
            "(A&B)->(!A)",
            "((A&B)->A)->((A&B)->(!A))->(!(A&B))",
            "((A&B)->(!A))->(!(A&B))",
            "!(A&B)",
    };

    // !A,!B|-!(A&B)
    final static String FF[] = new String[]{
            "!B->A&B->!B",
            "A&B->!B",
            "A&B->B",
            "(A&B->B)->(A&B->!B)->!(A&B)",
            "(A&B->!B)->!(A&B)",
            "!(A&B)"
    };

    public Conjunction(Expression left, Expression right) {
        super(left, right);
    }

    @Override
    protected String getSymbol() {
        return "&";
    }

    @Override
    public boolean evaluate(@NotNull Map<String, Boolean> variables) {
        return  right.evaluate(variables) & left.evaluate(variables);
    }

    @Override
    protected String[] getProof(boolean a, boolean b) {
        if (a) {
            if (b) {
                return TT;
            } else {
                return TF;
            }
        } else {
            if (b) {
                return FT;
            } else {
                return FF;
            }
        }

    }
}
