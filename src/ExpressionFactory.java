public class ExpressionFactory {
    private static String s;
    private static int pos;

    public static Expression parse(String str) {
        s = str;
        pos = 0;
        return doConsequence();
    }

    private static void passWhitespaces() {
        while (pos < s.length() && Character.isWhitespace(s.charAt(pos))) {
            pos++;
        }
    }

    // ! right associativity
    private static Expression doConsequence() {
        Expression expression = doDisjunction(), curr;

        passWhitespaces();
        if (pos + 1 >= s.length() || (s.charAt(pos) != '-' && s.charAt(pos + 1) != '>'))
            return expression;

        pos += 2;
        passWhitespaces();
        return new Consequence(expression, doConsequence());
    }

    private static Expression doDisjunction() {
        Expression expression = doConjunction(), curr;
        passWhitespaces();
        while (pos < s.length()) {
            if (s.charAt(pos) != '|')
                break;

            pos++;
            passWhitespaces();
            curr = doConjunction();
            if (expression == null) {
                expression = curr;
            } else {
                expression = new Disjunction(expression, curr);
            }
            passWhitespaces();
        }

        return  expression;
    }

    private static Expression doConjunction() {
        Expression expression = doBracket(), curr;
        passWhitespaces();
        while (pos < s.length()) {
            if (s.charAt(pos) != '&')
                break;

            pos++;
            passWhitespaces();
            curr = doBracket();
            if (expression == null) {
                expression = curr;
            } else {
                expression = new Conjunction(expression, curr);
            }
            passWhitespaces();
        }

        return expression;
    }

    private static Expression doBracket() {
        passWhitespaces();
        if (s.charAt(pos) == '(') {
            pos++;
            passWhitespaces();
            Expression tmp = doConsequence();
            // closing bracket
            pos++;
            passWhitespaces();
            return tmp;
        }
        else
            return doNot();
    }

    private static Expression doNot() {
        if (s.charAt(pos) == '!') {
            pos++;
            passWhitespaces();
            return new Not(doBracket());
        }
        return doVariable();
    }

    private static Expression doVariable() {
        passWhitespaces();
        boolean not = (s.charAt(pos) == '!');
        if (not)
            pos++;

        StringBuilder name = new StringBuilder();
        do {
            name.append(s.charAt(pos));
            pos++;
        } while (pos < s.length() && Character.isDigit(s.charAt(pos)));
        passWhitespaces();

        return new Variable(name.toString());
    }
}
