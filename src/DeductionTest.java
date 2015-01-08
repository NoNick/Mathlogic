import org.junit.Test;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;

import static org.junit.Assert.*;

public class DeductionTest {
    @Test
    public void XEnsueXTest() {
        Deduction d = new Deduction(null, new Variable("X"));
        Line[] proof = d.XEnsueX(new Variable("X"), 0);
        assertEquals(proof.length, 5);
        assertTrue(proof[0].expression.equals(ExpressionFactory.parse("(X->(X->X))->(X->(X->X)->X)->(X->X)")));
        assertTrue(proof[1].expression.equals(ExpressionFactory.parse("X->X->X")));
        assertTrue(proof[2].expression.equals(ExpressionFactory.parse("(X->(X->X)->X)->(X->X)")));
        assertTrue(proof[3].expression.equals(ExpressionFactory.parse("X->(X->X)->X")));
        assertTrue(proof[4].expression.equals(ExpressionFactory.parse("X->X")));
    }

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
            int i = 0;
            while ((curr = in.nextLine()) != null) {
                if (curr.length() == 0)
                    continue;

                Line[] result = deduction.nextLine(ExpressionFactory.parse(curr));
                if (result == null)
                    continue;
                for (Line r: result) {
//                    out.println(r.expression.toString());
                    out.print("(" + i + ") " + r.expression.toString() + " ");
                    switch (r.type) {
                        case AXIOM:
                            out.println("Сх. акс. " + r.reference[0]);
                            break;
                        case MODUS_PONENS:
                            out.println("M.P. " + r.reference[0] + ", " + r.reference[1]);
                            break;
                        case NOT_PROVEN:
                            out.println("Не доказано");
                            break;
                    }
                    i++;
                }
            }

            out.close();
        }
    }
}