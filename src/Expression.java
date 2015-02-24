import java.util.*;

public abstract class Expression extends Throwable {
    protected Expression left, right = null;

    public Expression( Expression left,  Expression right) {
        this.left = left;
        this.right = right;
    }

    public Expression( Expression left) {
        this.left = left;
    }

    protected abstract String getSymbol();

    public String toString() {
        if (right != null)
            return "(" + left.toString() + getSymbol() + right.toString() + ")";
        return left.toString();
    }

    @Override
    public boolean equals(Object other) {
        if (!getClass().equals(other.getClass()))
            return false;
        return left.equals(((Expression)other).left) && ((right == null && ((Expression)other).right == null)
                || (right != null && ((Expression)other).right != null && right.equals(((Expression)other).right)));
    }

    public boolean matches( Expression other) {
        return matches(other, new HashMap<String, Expression>());
    }

    protected boolean matches(Expression other, HashMap<String, Expression> history) {
        if (other instanceof Variable) {
            Expression prev = history.get(other.toString());
            if (prev == null) {
                history.put(other.toString(), this);
                return true;
            } else {
                return equals(prev);
            }
        }

        return getClass().equals(other.getClass()) &&
          ((right == null && other.right == null) || (right != null && other.right != null &&
                                                                     right.matches(other.right, history))) &&
          left.matches(other.left, history);

    }

    // may create a few links pointing to one object inside tree
    // returns copy of itself with replacements committed
    public Expression replace( Map<String, Expression> replacements) {
        Expression rright = null, rleft = null;
        if (right != null) {
            rright = right.replace(replacements);
        }
        if (left != null) {
            rleft = left.replace(replacements);
        }

        if (this instanceof Conjunction)
            return new Conjunction(rleft, rright);
        if (this instanceof Disjunction)
            return new Disjunction(rleft, rright);
        if (this instanceof Consequence)
            return new Consequence(rleft, rright);
        if (this instanceof Not)
            return new Not(rleft);
        return null;
    }

    public abstract boolean evaluate( Map<String, Boolean> variables);

    public ArrayList<String> getVariables() {
        return new ArrayList<String>(getVariables(new HashSet<String>()));
    }

    protected HashSet<String> getVariables(HashSet<String> curr) {
        left.getVariables(curr);
        if (right != null) {
            right.getVariables(curr);
        }
        return curr;
    }

    public List<Expression> proofForVars( Map<String, Boolean> vars) {
        ArrayList<Expression> proof = new ArrayList<Expression>();
        proof.addAll(left.proofForVars(vars));
        if (right != null) {
            proof.addAll(right.proofForVars(vars));
        }

        HashMap<String, Expression> hm = new HashMap<String, Expression>();
        hm.put("A", left);
        hm.put("B", right);

        String[] lemma;
        lemma = getProof(left.evaluate(vars), right.evaluate(vars));
        for (String str: lemma) {
            proof.add(ExpressionFactory.parse(str).replace(hm));
        }

        return proof;
    }

    protected abstract String[] getProof(boolean a, boolean b);
}
