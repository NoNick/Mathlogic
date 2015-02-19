import org.junit.Test;

import java.io.PrintWriter;
import java.util.*;

import static org.junit.Assert.*;

public class ExpressionTest {
    @Test
    public void equalsTest() {
        String axiomsText[] = new String[]{
                "A -> (B -> A)",
                "(A -> (B -> C)) -> ((A -> B) -> (A -> C))",
                "(A & B) -> A",
                "(A & B) -> B",
                "A -> (B -> (A & B))",
                "A -> (A | B)",
                "B -> (A | B)",
                "(A -> C) -> ((B -> C) -> (A | B -> C))",
                "!A -> (A -> B)",
                "(A -> B) -> ((A -> !B) -> !A)",
                "A | !A"
        };
        ArrayList <Expression> expressions1 = new ArrayList<Expression>();
        ArrayList <Expression> expressions2 = new ArrayList<Expression>();
        for (String s: axiomsText) {
            expressions1.add(ExpressionFactory.parse(s));
            expressions2.add(ExpressionFactory.parse(s));
        }

        for (int i = 0; i < expressions1.size(); i++) {
            for (int j = 0; j < expressions2.size(); j++) {
                assertEquals(expressions1.get(i).equals(expressions2.get(j)), i == j);
            }
        }
    }

    @Test
    public void matchesTest() {
        // {mask, expression}
        String[][] input = new String[][]{new String[]{"A", "A"}, new String[]{"A", "B"}, new String[]{"A", "A&B&C&!D"},
                     new String[]{"A&B", "A01&C&B01&D"}, new String[]{"A&B", "(A01&C)&(B01&D)"},
                     new String[]{"A&A", "A&B"}, new String[]{"A&A", "A|A"}, new String[]{"A&A", "A"},
                     new String[]{"(A->B)->(A->!B)->!A", "A->B"},
                     new String[]{"(A->B)->(A->B->C)->(A->C)",
            "(((A->B)->(!B->((A->B)->((A->!B)->!A))))->(((A->B)->((A->!B)->!A))->((A->B)->((A->B)->((A->!B)->!A)))))"}};
        boolean ans[] = new boolean[]{true, true, true, true, true, false, false, false, false, false};
        for (int i = 0; i < input.length; i++) {
            assertEquals(ExpressionFactory.parse(input[i][1]).matches(ExpressionFactory.parse(input[i][0])), ans[i]);
        }
    }

    @Test
    public void replaceTest() {
        String axiomsText[] = new String[]{
                "A -> (B -> A)",
                "(A -> (B -> C)) -> ((A -> B) -> (A -> C))",
                "(A & B) -> A",
                "(A & B) -> B",
        };
        String axiomsNewText[] = new String[]{
                "B -> (A -> B)",
                "(A1 -> (B1 -> C)) -> ((A1 -> B1) -> (A1 -> C))",
                "(A11 & B11) -> A11",
                "(A1 & (B|A)) -> (B|A)",
        };
        HashMap<String, Expression> r = new HashMap<String, Expression>();
        r.put("A", new Variable("B"));
        r.put("B", new Variable("A"));
        assertEquals(ExpressionFactory.parse(axiomsText[0]).replace(r).toString(),
                ExpressionFactory.parse(axiomsNewText[0]).toString());
        r.put("A", new Variable("A1"));
        r.put("B", new Variable("B1"));
        assertEquals(ExpressionFactory.parse(axiomsText[1]).replace(r).toString(),
                ExpressionFactory.parse(axiomsNewText[1]).toString());
        r.put("A", new Variable("A11"));
        r.put("B", new Variable("B11"));
        assertEquals(ExpressionFactory.parse(axiomsText[2]).replace(r).toString(),
                ExpressionFactory.parse(axiomsNewText[2]).toString());
        r = new HashMap<String, Expression>();
        r.put("A", new Variable("A1"));
        r.put("B", new Disjunction(new Variable("B"), new Variable("A")));
        assertEquals(ExpressionFactory.parse(axiomsText[3]).replace(r).toString(),
                ExpressionFactory.parse(axiomsNewText[3]).toString());
    }

    @Test
    public void evaluateAxiomsTest() {
        String axiomsText[] = new String[]{
                "A -> (B -> A)",
                "(A -> (B -> C)) -> ((A -> B) -> (A -> C))",
                "(A & B) -> A",
                "(A & B) -> B",
                "A -> (B -> (A & B))",
                "A -> (A | B)",
                "B -> (A | B)",
                "(A -> C) -> ((B -> C) -> (A | B -> C))",
                "!A -> (A -> B)",
                "(A -> B) -> ((A -> !B) -> !A)",
                "A | !A"
        };

        HashMap<String, Boolean> variables = new HashMap<String, Boolean>();
        boolean carry;
        String vars[] = new String[]{"A", "B", "C"};
        for (String a: axiomsText) {
            Expression e = ExpressionFactory.parse(a);
            boolean values[] = new boolean[]{false, false, false};
            for (int i = 0; i < 8; i++) {
                for (int j = 0; j < vars.length; j++) {
                    variables.put(vars[j], values[j]);
                }
                assertTrue(e.evaluate(variables));

                carry = true;
                for (int j = values.length - 1; j >= 0 && carry; j--) {
                    boolean old = values[j];
                    values[j] = values[j] ^ carry;
                    carry &= old;
                }
            }
        }
    }

    @Test
    public void evaluateFalseTest() {
        HashMap<String, Boolean> e = new HashMap<String, Boolean>();
        e.put("A", true);
        e.put("B", true);
        assertFalse(ExpressionFactory.parse("A&!B").evaluate(e));
    }

    @Test(expected = ArithmeticException.class)
    public void evaluateExceptionTest() {
        HashMap<String, Boolean> e = new HashMap<String, Boolean>();
        e.put("A", true);
        ExpressionFactory.parse("A&!B").evaluate(e);
    }

    @Test
    public void getVariablesTest() {
        HashSet<String> right = new HashSet<String>();
        right.add("A");
        right.add("B");
        assertEquals(ExpressionFactory.parse("A&B").getVariables().size(), right.size());
        for (String s: ExpressionFactory.parse("A&B").getVariables()) {
            assertTrue(right.contains(s));
        }

        right.add("A1");
        right.add("B013");
        assertEquals(ExpressionFactory.parse("A&B->A1|!B013&A->A1").getVariables().size(), right.size());
        for (String s: ExpressionFactory.parse("A&B->A1|!B013&A->A1").getVariables()) {
            assertTrue(right.contains(s));
        }
    }

    @Test
    public void testProofForVars() throws Exception {
        Expression expression = ExpressionFactory.parse("!A|!B");
        HashMap<String, Boolean> hm = new HashMap<String, Boolean>();
        hm.put("A", true);
        hm.put("B", true);
        List<Expression> result = expression.proofForVars(hm);

        PrintWriter out = new PrintWriter("proofForVarsTest.out");
        for (Expression e: result) {
            out.println(e.toString());
        }
        out.close();
    }
}