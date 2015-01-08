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
}
