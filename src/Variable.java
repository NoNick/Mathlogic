import com.sun.istack.internal.NotNull;

import java.util.*;

public class Variable extends Expression{
    String name;

    public Variable(String name) {
        super(null, null);
        this.name = name;
    }

    @Override
    protected String getSymbol() {
        return null;
    }

    @Override
    public String toString() {
        return name;
    }

    @Override
    public boolean equals(Object other) {
        return (other instanceof Variable) && name.equals(((Variable) other).name);
    }

    @Override
    public Expression replace(@NotNull Map<String, Expression> replacements) {
        Expression expression = replacements.get(name);
        if (expression != null) {
            return expression;
        }
        return this;
    }

    @Override
    public boolean evaluate(@NotNull Map<String, Boolean> variables) {
        Boolean result = variables.get(name);
        if (result == null)
            throw new ArithmeticException("No such variable in map: " + name);
        return result;
    }

    @Override
    protected HashSet<String> getVariables(HashSet<String> curr) {
        curr.add(name);
        return curr;
    }

    @Override
    protected String[] getProof(boolean a, boolean b) {
        return new String[0];
    }

    @Override
    public List<Expression> proofForVars(@NotNull Map<String, Boolean> vars) {
        ArrayList<Expression> list = new ArrayList<Expression>();
        list.add(evaluate(vars) ? this : new Not(this));
        return list;
    }
}
