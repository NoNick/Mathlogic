import java.util.ArrayList;
import java.util.HashMap;

public class ExcludeAssumption {
    public static ArrayList<Expression> getContrposition(Expression a, Expression b) throws Exception {
        ArrayList<Expression> result = new ArrayList<Expression>();
        HashMap<String, Expression> hm = new HashMap<String, Expression>();
        hm.put("A", a);
        hm.put("B", b);
        result.add(ExpressionFactory.parse("(A->B)->(A->!B)->!A").replace(hm));
        result.add(ExpressionFactory.parse("A->B").replace(hm));
        result.add(ExpressionFactory.parse("(A->!B)->!A").replace(hm));
        result.add(ExpressionFactory.parse("!B->A->!B").replace(hm));
        result.add(ExpressionFactory.parse("!B").replace(hm));
        result.add(ExpressionFactory.parse("A->!B").replace(hm));
        result.add(ExpressionFactory.parse("!A").replace(hm));

        ArrayList<Expression> assumption = new ArrayList<Expression>();
        assumption.add(ExpressionFactory.parse("A->B").replace(hm));
        result = Deduction.deduct(assumption, new Not(b), new Not(a), result);
        result = Deduction.deduct(new ArrayList<Expression>(), ExpressionFactory.parse("A->B").replace(hm),
                    ExpressionFactory.parse("!B->!A").replace(hm), result);

        return result;
    }

    public static ArrayList<Expression> getExcludedMiddle(Expression a) throws Exception {
        ArrayList<Expression> result = new ArrayList<Expression>();
        HashMap<String, Expression> hm = new HashMap<String, Expression>();
        hm.put("A", a);
        result.add(ExpressionFactory.parse("A->(A|!A)").replace(hm));
        result.addAll(getContrposition(a, ExpressionFactory.parse("A|!A").replace(hm)));
        result.add(ExpressionFactory.parse("!(A|!A)->!A").replace(hm));
        result.add(ExpressionFactory.parse("!A->(A|!A)").replace(hm));
        result.addAll(getContrposition(new Not(a), ExpressionFactory.parse("A|!A").replace(hm)));
        result.add(ExpressionFactory.parse("!(A|!A)->!(!A)").replace(hm));
        result.add(ExpressionFactory.parse("(!(A|!A)->!A)->(!(A|!A)->!(!A))->!(!(A|!A))").replace(hm));
        result.add(ExpressionFactory.parse("(!(A|!A)->!(!A))->!(!(A|!A))").replace(hm));
        result.add(ExpressionFactory.parse("!(!(A|!A))").replace(hm));
        result.add(ExpressionFactory.parse("!(!(A|!A))->A|!A").replace(hm));
        result.add(ExpressionFactory.parse("A|!A").replace(hm));

        return result;
    }

    public static ArrayList<Expression> get(Prover.ProofWithAssumptions proofTrue, Prover.ProofWithAssumptions proofFalse)
                                                                                                    throws Exception {
        ArrayList<Expression> result = new ArrayList<Expression>();
        HashMap<String, Expression> hm = new HashMap<String, Expression>();
        Expression assumption = proofTrue.getAlpha();
        Expression expression = proofTrue.getResult();
        proofTrue.deduct();
        proofFalse.deduct();
        hm.put("A", assumption);
        hm.put("E", expression);
        result.addAll(proofTrue.proof);
        result.addAll(proofFalse.proof);
        result.addAll(getExcludedMiddle(assumption));
        result.add(ExpressionFactory.parse("(A->E)->(!A->E)->(A|!A)->E").replace(hm));
        result.add(ExpressionFactory.parse("(!A->E)->(A|!A->E)").replace(hm));
        result.add(ExpressionFactory.parse("A|!A->E").replace(hm));
        result.add(ExpressionFactory.parse("E").replace(hm));

        return result;
    }
}
