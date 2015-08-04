import org.junit.Test;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;

import static org.junit.Assert.*;

public class ProofTest {
    @Test
    public void homework1() throws IOException {
        File dir = new File("homework1");
        if (dir.listFiles() == null)
            return;
        for (File test: dir.listFiles()) {
            String name = test.getName();
            if (!name.substring(name.length() - 3).equals(".in"))
                continue;

            FastScanner in = new FastScanner(test);
            PrintWriter out = new PrintWriter(new File(dir.getName() + "/" +
                                                 name.substring(0, name.length() - 3) + ".out"));

            Proof proof = new Proof();
            String curr;
            int i = 0;
            while ((curr = in.nextLine()) != null) {
                if (curr.length() == 0)
                    continue;

                Line result = proof.addLine(ExpressionFactory.parse(curr));
                out.print("(" + (i + 1) + ") " + curr + " ");
                switch (result.type) {
                    case AXIOM:
                        out.println("(Сх. акс. " + result.reference[0] + ")");
                        break;
                    case MODUS_PONENS:
                        out.println("(M.P. " + (result.reference[0] + 1) + ", " + (result.reference[1] + 1) + ")");
                        break;
                    case NOT_PROVEN:
                        out.println("(Не доказано)");
                        break;
                }
                i++;
            }

            out.close();
        }
    }
}