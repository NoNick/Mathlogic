public class ExpressionFactory {
    private static String s;
    private static int pos;

    public static Expression parse(String str) {
        s = str.replaceAll("\\s+", "");
        pos = 0;
        return doImply();
    }

    // ! right associativity
    private static Expression doImply() {
        Expression expression = doDisjunction(), curr;

        if (pos + 1 >= s.length() || (s.charAt(pos) != '-' && s.charAt(pos + 1) != '>'))
            return expression;

        pos += 2;
        return new Consequence(expression, doImply());
    }

    private static Expression doDisjunction() {
        Expression expression = doConjunction(), curr;
        while (pos < s.length()) {
            if (s.charAt(pos) != '|')
                break;

            pos++;
            curr = doConjunction();
            if (expression == null) {
                expression = curr;
            } else {
                expression = new Disjunction(expression, curr);
            }
        }

        return  expression;
    }

    private static Expression doConjunction() {
        Expression expression = doUnary(), curr;
        while (pos < s.length()) {
            if (s.charAt(pos) != '&')
                break;

            pos++;
            curr = doUnary();
            if (expression == null) {
                expression = curr;
            } else {
                expression = new Conjunction(expression, curr);
            }
        }

        return expression;
    }

    private static Expression doUnary() {
        if (s.charAt(pos) == '(') {
            pos++;
            Expression tmp = doImply();
            // closing bracket
            pos++;
            return tmp;
        }
        else if (s.charAt(pos) == '!') {
            pos++;
            return new Not(doUnary());
        } else {
            return doVar();
        }
    }

    private static Expression doVar() {
        boolean not = (s.charAt(pos) == '!');
        if (not)
            pos++;

        StringBuilder name = new StringBuilder();
        do {
            name.append(s.charAt(pos));
            pos++;
        } while (pos < s.length() && Character.isDigit(s.charAt(pos)));

        return new Variable(name.toString());
    }
}
