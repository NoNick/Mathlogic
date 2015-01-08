import com.sun.istack.internal.NotNull;

import java.util.HashMap;

public abstract class Expression extends Throwable {
    protected Expression left, right = null;

    public Expression(@NotNull Expression left, @NotNull Expression right) {
        this.left = left;
        this.right = right;
    }

    public Expression(@NotNull Expression left) {
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

    public boolean matches(@NotNull Expression other) {
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
}
