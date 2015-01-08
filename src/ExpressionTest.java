import org.junit.Test;

import java.util.ArrayList;

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
        for(int i = 0; i < input.length; i++) {
            assertEquals(ExpressionFactory.parse(input[i][1]).matches(ExpressionFactory.parse(input[i][0])), ans[i]);
        }
    }
}