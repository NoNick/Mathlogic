import org.junit.Test;

import java.io.File;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;

public class ExcludeAssumptionTest {
    @Test
    public void testGet() throws Exception {
        PrintWriter out = new PrintWriter(new File("contrapositionTest.out"));
        ArrayList<Expression> result = ExcludeAssumption.getContrposition(new Variable("A"),
                new Disjunction(new Variable("A"), new Not(new Variable("A"))));
        for(Expression e: result) {
            out.println(e.toString());
        }
        out.close();
    }

    @Test
    public void ExcludedMiddleTest() throws Exception {
        PrintWriter out = new PrintWriter(new File("excludedMiddleTest.out"));
        ArrayList<Expression> result = ExcludeAssumption.getExcludedMiddle(new Variable("A"));
        for(Expression e: result) {
            out.println(e.toString());
        }
        out.close();
    }

    @Test
    public void ExcludeAssumption() throws Exception {
        PrintWriter out = new PrintWriter(new File("excludeAssumptionTest.out"));
        Prover.ProofWithAssumptions a = new Prover.ProofWithAssumptions(new ArrayList<Expression>(Arrays.asList(
                ExpressionFactory.parse("(A&B)->A"),
                ExpressionFactory.parse("!A"),
                ExpressionFactory.parse("(!A)->((A&B)->(!A))"),
                ExpressionFactory.parse("(A&B)->(!A)"),
                ExpressionFactory.parse("((A&B)->A)->((A&B)->(!A))->(!(A&B))"),
                ExpressionFactory.parse("((A&B)->(!A))->(!(A&B))"),
                ExpressionFactory.parse("(!(A&B))")
        )), new ArrayList<Expression>(Arrays.asList(
                ExpressionFactory.parse("!A"),
                ExpressionFactory.parse("B")
        )));
        Prover.ProofWithAssumptions b = new Prover.ProofWithAssumptions(new ArrayList<Expression>(Arrays.asList(
                ExpressionFactory.parse("(A&B)->A"),
                ExpressionFactory.parse("!A"),
                ExpressionFactory.parse("(!A)->((A&B)->(!A))"),
                ExpressionFactory.parse("(A&B)->(!A)"),
                ExpressionFactory.parse("((A&B)->A)->((A&B)->(!A))->(!(A&B))"),
                ExpressionFactory.parse("((A&B)->(!A))->(!(A&B))"),
                ExpressionFactory.parse("(!(A&B))")
        )), new ArrayList<Expression>(Arrays.asList(
                ExpressionFactory.parse("!A"),
                ExpressionFactory.parse("!B")
        )));
        ArrayList<Expression> result = ExcludeAssumption.get(a, b);
        for(Expression e: result) {
            out.println(e.toString());
        }
        out.close();
    }
}