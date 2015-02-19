import org.junit.Test;

import java.io.File;
import java.io.IOException;

import static org.junit.Assert.*;

public class ExpressionFactoryTest {
    @Test
    public void singleVariable() {
        // pairs of input & output
        String test[][] = new String[][]{new String[]{"A", "A"}, new String[]{"!A", "!A"},
                new String[]{"A01234", "A01234"}, new String[]{"!A12391", "!A12391"}};
        for (String[] s: test) {
            assertEquals(s[1], ExpressionFactory.parse(s[0]).toString());
        }
    }

    @Test
    public void plainConjunction() {
        // pairs of input & output
        String test[][] = new String[][]{new String[]{"A&B&C", "((A&B)&C)"}, new String[]{"A & B & C", "((A&B)&C)"},
                                         new String[]{"A & B", "(A&B)"}, new String[]{"A&A ", "(A&A)"}};
        for (String[] s: test) {
            assertEquals(s[1], ExpressionFactory.parse(s[0]).toString());
        }
    }

    @Test
    public void plainDisjunction() {
        // pairs of input & output
        String test[][] = new String[][]{new String[]{"A|B|C", "((A|B)|C)"}, new String[]{"A | B | C", "((A|B)|C)"},
                new String[]{"A | B", "(A|B)"}, new String[]{"A|A ", "(A|A)"}};
        for (String[] s: test) {
            assertEquals(s[1], ExpressionFactory.parse(s[0]).toString());
        }
    }

    @Test
    public void bracketsConjunction() {
        // pairs of input & output
        String test[][] = new String[][]{new String[]{"(A&B)", "(A&B)"}, new String[]{"(A & B) & C", "((A&B)&C)"},
                new String[]{"A&(B&C)", "(A&(B&C))"}, new String[]{"(A&(B&C)) ", "(A&(B&C))"}};
        for (String[] s: test) {
            assertEquals(s[1], ExpressionFactory.parse(s[0]).toString());
        }
    }

    @Test
    public void bracketsDisjunction() {
        // pairs of input & output
        String test[][] = new String[][]{new String[]{"(A|B)", "(A|B)"}, new String[]{"(A | B) | C", "((A|B)|C)"},
                new String[]{"A|(B|C)", "(A|(B|C))"}, new String[]{"(A|(B|C)) ", "(A|(B|C))"}};
        for (String[] s: test) {
            assertEquals(s[1], ExpressionFactory.parse(s[0]).toString());
        }
    }

    @Test
    public void conjunctionDisjunctionBrackets() {
        // pairs of input & output
        String test[][] = new String[][]{new String[]{"A&B | C", "((A&B)|C)"}, new String[]{"A|B & C", "(A|(B&C))"},
                new String[]{"A &  B | D & C & A", "((A&B)|((D&C)&A))"},
                new String[]{"A &  ( B | D )& C", "((A&(B|D))&C)"}};
        for (String[] s: test) {
            assertEquals(s[1], ExpressionFactory.parse(s[0]).toString());
        }
    }

    @Test
    public void consequence() {
        // pairs of input & output
        String test[][] = new String[][]{new String[]{" A ->   B ", "(A->B)"}, new String[]{"A->B->C", "(A->(B->C))"},
                new String[]{"(A->B)->A ", "((A->B)->A)"},
                new String[]{"(A -> B -> C) -> ((A -> B) -> (A -> C))", "((A->(B->C))->((A->B)->(A->C)))"}};
        for (String[] s: test) {
            assertEquals(s[1], ExpressionFactory.parse(s[0]).toString());
        }
    }

    @Test
    public void mixed() {
        // pairs of input & output
        String test[][] = new String[][]{new String[]{"(A00 & B11) -> A00", "((A00&B11)->A00)"},
                          new String[]{"A&B|C->B|A&C->C", "(((A&B)|C)->((B|(A&C))->C))"},
                          new String[]{"!(A&!(!C)&B)", "!(((A&!(!C))&B))"}, new String[]{"!!!A", "!(!(!A))"}};
        for (String[] s: test) {
            assertEquals(s[1], ExpressionFactory.parse(s[0]).toString());
        }
    }

    @Test
    public void maxTest() throws IOException {
        FastScanner scanner = new FastScanner(new File("homework1/maxtest1.in"));
        String curr;
        while ((curr = scanner.nextLine()) != null) {
            if (curr.length() == 0)
                continue;
            ExpressionFactory.parse(curr);
        }
    }
}