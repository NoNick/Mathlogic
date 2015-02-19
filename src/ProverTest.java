import org.junit.Test;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

public class ProverTest {
    @Test
    public void validationInvalidsTest() throws IOException {
        File dir = new File("homework3");
        if (dir.listFiles() == null)
            return;
        for (File test: dir.listFiles()) {
            String name = test.getName();
            if (!name.substring(name.length() - 3).equals(".in"))
                continue;

            FastScanner in = new FastScanner(test);
            PrintWriter out = new PrintWriter(new File(dir.getName() + "/" +
                    name.substring(0, name.length() - 3) + ".out"));

            try {
                ArrayList<Expression> proof = Prover.prove(ExpressionFactory.parse(in.nextLine()));
                for (Expression expression: proof) {
                    out.println(expression);
                }
            } catch (NotValidException e) {
                String msg = "Высказывание ложно при ";
                Iterator it = e.values.entrySet().iterator();
                while (it.hasNext()) {
                    HashMap.Entry entry = (HashMap.Entry)it.next();
                    msg += entry.getKey() + ((Boolean)entry.getValue() ? "И" : "Л") + ", ";
                    it.remove(); // avoids a ConcurrentModificationException
                }
                out.println(msg);
            }
            out.close();
        }
    }

}
