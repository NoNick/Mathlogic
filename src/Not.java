import com.sun.istack.internal.NotNull;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Not extends Expression {
    final static String[] TRUE = new String[]{
            "A->!A->A",
            "!A->A",
            "!A->!A->!A",
            "(!A->!A->!A)->(!A->(!A->!A)->!A)->!A->!A",
            "(!A->(!A->!A)->!A)->!A->!A",
            "!A->(!A->!A)->!A",
            "!A->!A",
            "(!A->A)->(!A->!A)->!!A",
            "(!A->!A)->!!A",
            "!!A"
    };

    public Not(Expression left) {
        super(left);
    }

    @Override
    protected String getSymbol() {
        return null;
    }

    public String toString() {
        return "!" + left.toString();
    }

    @Override
    public boolean evaluate(@NotNull Map<String, Boolean> variables) {
        return !left.evaluate(variables);
    }

    @Override
    public List<Expression> proofForVars(@NotNull Map<String, Boolean> vars) {
        List<Expression> list = left.proofForVars(vars);
        if (evaluate(vars))
            return list;

        HashMap<String, Expression> hm = new HashMap<String, Expression>();
        hm.put("A", left);
        for (String str: TRUE) {
            list.add(ExpressionFactory.parse(str).replace(hm));
        }
        return list;
    }

    // Only for binary functions
    @Override
    protected String[] getProof(boolean a, boolean b) {
        return new String[0];
    }
}
