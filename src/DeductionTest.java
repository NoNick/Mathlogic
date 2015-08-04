import org.junit.Test;

import java.io.File;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class DeductionTest {
    @Test
    public void homework2() throws Exception {
        File dir = new File("homework2");
        if (dir.listFiles() == null)
            return;
        for (File test: dir.listFiles()) {
            String name = test.getName();
            if (!name.substring(name.length() - 3).equals(".in"))
                continue;

            FastScanner in = new FastScanner(test);
            PrintWriter out = new PrintWriter(new File(dir.getName() + "/" +
                                        name.substring(0, name.length() - 3) + ".out"));

            String curr = in.nextLine(), split[] = curr.split("\\0174-"), premiseText[] = split[0].split(",");
            Expression premise[] = new Expression[premiseText.length];
            for (int i = 0; i < premise.length; i++) {
                premise[i] = ExpressionFactory.parse(premiseText[i]);
            }
            Deduction deduction = new Deduction(premise, ExpressionFactory.parse(split[1]));
            out.println(deduction.getHeader());
            while ((curr = in.nextLine()) != null) {
                if (curr.length() == 0)
                    continue;

                Expression[] result = deduction.nextLine(ExpressionFactory.parse(curr));
                if (result == null)
                    continue;
                for (Expression r: result) {
                    out.println(r.toString());
                }
            }

            out.close();
        }
    }

    @Test
    public void deduct() throws Exception {
        List<Expression> list = Arrays.asList(
                ExpressionFactory.parse("((A -> (A | !A))->((A->!(A|!A))->!A))"),
                ExpressionFactory.parse("(A->(A|!A))"),
                ExpressionFactory.parse("((A->!(A|!A))->!A)"),
                ExpressionFactory.parse("(!(A|!A)->(A->!(A|!A)))"),
                ExpressionFactory.parse("!(A|!A)"),
                ExpressionFactory.parse("(A->!(A|!A))"),
                ExpressionFactory.parse("!A"));
        ArrayList<Expression> premise = new ArrayList<Expression>();
        premise.add(ExpressionFactory.parse("(A->(A|!A))"));
        ArrayList<Expression> proof = Deduction.deduct(premise, ExpressionFactory.parse("!(A|!A)"),
                                                ExpressionFactory.parse("!A"), new ArrayList<Expression>(list));
        PrintWriter out = new PrintWriter("deduct.out");
        for (Expression e: proof) {
            out.println(e);
        }
        out.close();
    }
}