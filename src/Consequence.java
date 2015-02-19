import com.sun.istack.internal.NotNull;

import java.util.Map;

public class Consequence extends Expression{
    // A,B|-A->B
    final static String[] TT = {
            "B->A->B",
            "A->B"
    };

    //A,!B|-!(A->B)
    final static String[] TF = {
            "(A->B)->(A->B)->A->B",
            "((A->B)->(A->B)->A->B)->((A->B)->((A->B)->A->B)->A->B)->(A->B)->A->B",
            "((A->B)->((A->B)->A->B)->A->B)->(A->B)->A->B",
            "(A->B)->((A->B)->A->B)->A->B",
            "(A->B)->A->B",
            "A->(A->B)->A",
            "(A->B)->A",
            "((A->B)->A)->((A->B)->A->B)->(A->B)->B",
            "((A->B)->A->B)->(A->B)->B",
            "(A->B)->B",
            "B->!A|B",
            "(B->!A|B)->(A->B)->B->!A|B",
            "(A->B)->B->!A|B",
            "((A->B)->B)->((A->B)->B->!A|B)->(A->B)->!A|B",
            "((A->B)->B->!A|B)->(A->B)->!A|B",
            "(A->B)->!A|B",
            "(!A->A)->(!A->!A)->!!A",
            "A->!A->A",
            "!A->A",
            "(!A->!A)->!!A",
            "!A->!A->!A",
            "(!A->!A->!A)->(!A->(!A->!A)->!A)->!A->!A",
            "(!A->(!A->!A)->!A)->!A->!A",
            "!A->(!A->!A)->!A",
            "!A->!A",
            "!!A",
            "!A->!A->!A",
            "(!A->!A->!A)->(!A->(!A->!A)->!A)->!A->!A",
            "(!A->(!A->!A)->!A)->!A->!A",
            "!A->(!A->!A)->!A",
            "!A->!A",
            "(!!A&!B->!A)->(!!A&!B->!!A)->!(!!A&!B)",
            "((!!A&!B->!A)->(!!A&!B->!!A)->!(!!A&!B))->!A->(!!A&!B->!A)->(!!A&!B->!!A)->!(!!A&!B)",
            "!A->(!!A&!B->!A)->(!!A&!B->!!A)->!(!!A&!B)",
            "!A->!!A&!B->!A",
            "(!A->!!A&!B->!A)->!A->!A->!!A&!B->!A",
            "!A->!A->!!A&!B->!A",
            "(!A->!A)->(!A->!A->!!A&!B->!A)->!A->!!A&!B->!A",
            "(!A->!A->!!A&!B->!A)->!A->!!A&!B->!A",
            "!A->!!A&!B->!A",
            "(!A->!!A&!B->!A)->(!A->(!!A&!B->!A)->(!!A&!B->!!A)->!(!!A&!B))->!A->(!!A&!B->!!A)->!(!!A&!B)",
            "(!A->(!!A&!B->!A)->(!!A&!B->!!A)->!(!!A&!B))->!A->(!!A&!B->!!A)->!(!!A&!B)",
            "!A->(!!A&!B->!!A)->!(!!A&!B)",
            "!!A&!B->!!A",
            "(!!A&!B->!!A)->!A->!!A&!B->!!A",
            "!A->!!A&!B->!!A",
            "(!A->!!A&!B->!!A)->(!A->(!!A&!B->!!A)->!(!!A&!B))->!A->!(!!A&!B)",
            "(!A->(!!A&!B->!!A)->!(!!A&!B))->!A->!(!!A&!B)",
            "!A->!(!!A&!B)",
            "B->B->B",
            "(B->B->B)->(B->(B->B)->B)->B->B",
            "(B->(B->B)->B)->B->B",
            "B->(B->B)->B",
            "B->B",
            "(!!A&!B->B)->(!!A&!B->!B)->!(!!A&!B)",
            "((!!A&!B->B)->(!!A&!B->!B)->!(!!A&!B))->B->(!!A&!B->B)->(!!A&!B->!B)->!(!!A&!B)",
            "B->(!!A&!B->B)->(!!A&!B->!B)->!(!!A&!B)",
            "B->!!A&!B->B",
            "(B->!!A&!B->B)->B->B->!!A&!B->B",
            "B->B->!!A&!B->B",
            "(B->B)->(B->B->!!A&!B->B)->B->!!A&!B->B",
            "(B->B->!!A&!B->B)->B->!!A&!B->B",
            "B->!!A&!B->B",
            "(B->!!A&!B->B)->(B->(!!A&!B->B)->(!!A&!B->!B)->!(!!A&!B))->B->(!!A&!B->!B)->!(!!A&!B)",
            "(B->(!!A&!B->B)->(!!A&!B->!B)->!(!!A&!B))->B->(!!A&!B->!B)->!(!!A&!B)",
            "B->(!!A&!B->!B)->!(!!A&!B)",
            "!!A&!B->!B",
            "(!!A&!B->!B)->B->!!A&!B->!B",
            "B->!!A&!B->!B",
            "(B->!!A&!B->!B)->(B->(!!A&!B->!B)->!(!!A&!B))->B->!(!!A&!B)",
            "(B->(!!A&!B->!B)->!(!!A&!B))->B->!(!!A&!B)",
            "B->!(!!A&!B)",
            "(!A->!(!!A&!B))->(B->!(!!A&!B))->!A|B->!(!!A&!B)",
            "(B->!(!!A&!B))->!A|B->!(!!A&!B)",
            "!A|B->!(!!A&!B)",
            "!!A->!B->!!A&!B",
            "!B->!!A&!B",
            "!!A&!B",
            "!!A&!B->!A|B->!!A&!B",
            "!A|B->!!A&!B",
            "(!A|B->!!A&!B)->(!A|B->!(!!A&!B))->!(!A|B)",
            "(!A|B->!(!!A&!B))->!(!A|B)",
            "!(!A|B)",
            "!(!A|B)->(A->B)->!(!A|B)",
            "(A->B)->!(!A|B)",
            "((A->B)->!A|B)->((A->B)->!(!A|B))->!(A->B)",
            "((A->B)->!(!A|B))->!(A->B)",
            "!(A->B)"
    };

    // !A,B|-A->B
    final static String[] FT = {
            "B->A->B",
            "A->B"
    };

    // !A,!B|-A->B
    final static String[] FF = {
            "(!B->A)->(!B->!A)->!!B",
            "((!B->A)->(!B->!A)->!!B)->A->(!B->A)->(!B->!A)->!!B",
            "A->(!B->A)->(!B->!A)->!!B",
            "!A->!B->!A",
            "(!A->!B->!A)->A->!A->!B->!A",
            "A->!A->!B->!A",
            "A->!B->A",
            "(A->!B->A)->A->A->!B->A",
            "A->A->!B->A",
            "A->A->A",
            "(A->A->A)->(A->(A->A)->A)->A->A",
            "(A->(A->A)->A)->A->A",
            "A->(A->A)->A",
            "A->A",
            "!A->A->!A",
            "A->!A",
            "(A->A)->(A->A->!B->A)->A->!B->A",
            "(A->A->!B->A)->A->!B->A",
            "A->!B->A",
            "(A->!A)->(A->!A->!B->!A)->A->!B->!A",
            "(A->!A->!B->!A)->A->!B->!A",
            "A->!B->!A",
            "(A->!B->A)->(A->(!B->A)->(!B->!A)->!!B)->A->(!B->!A)->!!B",
            "(A->(!B->A)->(!B->!A)->!!B)->A->(!B->!A)->!!B",
            "A->(!B->!A)->!!B",
            "(A->!B->!A)->(A->(!B->!A)->!!B)->A->!!B",
            "(A->(!B->!A)->!!B)->A->!!B",
            "A->!!B",
            "!!B->B",
            "(!!B->B)->A->!!B->B",
            "A->!!B->B",
            "(A->!!B)->(A->!!B->B)->A->B",
            "(A->!!B->B)->A->B",
            "A->B"
    };

    public Consequence(Expression left, Expression right) {
        super(left, right);
    }

    @Override
    protected String getSymbol() {
        return "->";
    }

    @Override
    public boolean evaluate(@NotNull Map<String, Boolean> variables) {
        return !left.evaluate(variables) ||
                right.evaluate(variables);
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
