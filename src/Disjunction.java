import java.util.Map;

public class Disjunction extends Expression{
    //A,B|-A|B
    final static String[] TT = {
            "A->A|B",
            "A|B"
    };

    //A,!B|-A|B
    final static String[] TF = {
            "A->A|B",
            "A|B"
    };

    //!A,B|-A|B
    final static String[] FT = {
            "B->A|B",
            "A|B"
    };

    //!A,!B|-!(A|B)
    final static String[] FF = {
            "A->A->A",
            "(A->A->A)->(A->(A->A)->A)->A->A",
            "(A->(A->A)->A)->A->A",
            "A->(A->A)->A",
            "A->A",
            "(!A&!B->A)->(!A&!B->!A)->!(!A&!B)",
            "((!A&!B->A)->(!A&!B->!A)->!(!A&!B))->A->(!A&!B->A)->(!A&!B->!A)->!(!A&!B)",
            "A->(!A&!B->A)->(!A&!B->!A)->!(!A&!B)",
            "A->!A&!B->A",
            "(A->!A&!B->A)->A->A->!A&!B->A",
            "A->A->!A&!B->A",
            "(A->A)->(A->A->!A&!B->A)->A->!A&!B->A",
            "(A->A->!A&!B->A)->A->!A&!B->A",
            "A->!A&!B->A",
            "(A->!A&!B->A)->(A->(!A&!B->A)->(!A&!B->!A)->!(!A&!B))->A->(!A&!B->!A)->!(!A&!B)",
            "(A->(!A&!B->A)->(!A&!B->!A)->!(!A&!B))->A->(!A&!B->!A)->!(!A&!B)",
            "A->(!A&!B->!A)->!(!A&!B)",
            "!A&!B->!A",
            "(!A&!B->!A)->A->!A&!B->!A",
            "A->!A&!B->!A",
            "(A->!A&!B->!A)->(A->(!A&!B->!A)->!(!A&!B))->A->!(!A&!B)",
            "(A->(!A&!B->!A)->!(!A&!B))->A->!(!A&!B)",
            "A->!(!A&!B)",
            "B->B->B",
            "(B->B->B)->(B->(B->B)->B)->B->B",
            "(B->(B->B)->B)->B->B",
            "B->(B->B)->B",
            "B->B",
            "(!A&!B->B)->(!A&!B->!B)->!(!A&!B)",
            "((!A&!B->B)->(!A&!B->!B)->!(!A&!B))->B->(!A&!B->B)->(!A&!B->!B)->!(!A&!B)",
            "B->(!A&!B->B)->(!A&!B->!B)->!(!A&!B)",
            "B->!A&!B->B",
            "(B->!A&!B->B)->B->B->!A&!B->B",
            "B->B->!A&!B->B",
            "(B->B)->(B->B->!A&!B->B)->B->!A&!B->B",
            "(B->B->!A&!B->B)->B->!A&!B->B",
            "B->!A&!B->B",
            "(B->!A&!B->B)->(B->(!A&!B->B)->(!A&!B->!B)->!(!A&!B))->B->(!A&!B->!B)->!(!A&!B)",
            "(B->(!A&!B->B)->(!A&!B->!B)->!(!A&!B))->B->(!A&!B->!B)->!(!A&!B)",
            "B->(!A&!B->!B)->!(!A&!B)",
            "!A&!B->!B",
            "(!A&!B->!B)->B->!A&!B->!B",
            "B->!A&!B->!B",
            "(B->!A&!B->!B)->(B->(!A&!B->!B)->!(!A&!B))->B->!(!A&!B)",
            "(B->(!A&!B->!B)->!(!A&!B))->B->!(!A&!B)",
            "B->!(!A&!B)",
            "(A->!(!A&!B))->(B->!(!A&!B))->A|B->!(!A&!B)",
            "(B->!(!A&!B))->A|B->!(!A&!B)",
            "A|B->!(!A&!B)",
            "!A->!B->!A&!B",
            "!B->!A&!B",
            "!A&!B",
            "!A&!B->A|B->!A&!B",
            "A|B->!A&!B",
            "(A|B->!A&!B)->(A|B->!(!A&!B))->!(A|B)",
            "(A|B->!(!A&!B))->!(A|B)",
            "!(A|B)"
    };

    public Disjunction(Expression left, Expression right) {
        super(left, right);
    }

    @Override
    protected String getSymbol() {
        return "|";
    }

    @Override
    public boolean evaluate( Map<String, Boolean> variables) {
        return  right.evaluate(variables) | left.evaluate(variables);
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
