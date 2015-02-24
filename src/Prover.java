import java.io.File;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

public class Prover {
    public static class ProofWithAssumptions {
        ArrayList<Expression> proof;
        ArrayList<Expression> assumptions;

        public ProofWithAssumptions() {}

        public ProofWithAssumptions(ArrayList<Expression> proof, ArrayList<Expression> assumptions) {
            this.proof = proof;
            this.assumptions = assumptions;
        }

        // get last statement of proof
        public Expression getResult() {
            return proof.isEmpty() ? null : proof.get(proof.size() - 1);
        }

        // get last assumption
        public Expression getAlpha() {
            return assumptions.isEmpty() ? null : assumptions.get(assumptions.size() - 1);
        }

        // reduce last assumption
        public void deduct() throws Exception {
            Expression alpha = assumptions.get(assumptions.size() - 1);
            assumptions.remove(assumptions.size() - 1);
            proof = Deduction.deduct((ArrayList<Expression>)assumptions.clone(), alpha, proof.get(proof.size() - 1), proof);
        }
    }

    // do nothing if valid
    // throw an exception with evaluation of variables when expr is false otherwise
    private static void isValid(Expression expr) throws NotValidException {
        ArrayList<String> vars = expr.getVariables();
        HashMap<String, Boolean> hm = new HashMap<String, Boolean>();
        boolean values[] = new boolean[vars.size()], carry;
        for (int i = 0; i < Math.pow(2, values.length); i++) {
            for (int j = 0; j < vars.size(); j++) {
                hm.put(vars.get(j), values[j]);
            }

            if (!expr.evaluate(hm)) {
                throw new NotValidException(hm);
            }

            carry = true;
            for (int j = values.length - 1; j >= 0 && carry; j--) {
                boolean old = values[j];
                values[j] = values[j] ^ carry;
                carry &= old;
            }
        }
    }

    private static ProofWithAssumptions buildProof(int index, Expression toBeProven, ArrayList<String> variables,
                                                   HashMap<String, Boolean> evaluation) throws Exception {
        if (index < variables.size()) {
            evaluation.put(variables.get(index), false);
            ProofWithAssumptions varFalse = buildProof(index + 1, toBeProven, variables, evaluation);
            evaluation.put(variables.get(index), true);
            ProofWithAssumptions varTrue = buildProof(index + 1, toBeProven, variables, evaluation);
            // last assumption in varTrue was removed in ProofWithAssumptions during varTrue.deduct() call
            return new ProofWithAssumptions(ExcludeAssumption.get(varTrue, varFalse), varTrue.assumptions);
        } else {
            ArrayList<Expression> assumptions = new ArrayList<Expression>();
            for (String var: variables) {
                if (evaluation.get(var)) {
                    assumptions.add(new Variable(var));
                } else {
                    assumptions.add(new Not(new Variable(var)));
                }
            }

            ArrayList<Expression> proof = new ArrayList<Expression>(toBeProven.proofForVars(evaluation));
            return new ProofWithAssumptions(proof, assumptions);
        }
    }

    public static ArrayList<Expression> prove(Expression expression) throws NotValidException {
        isValid(expression);

        ArrayList<String> vars = expression.getVariables();
        try {
            return buildProof(0, expression, vars, new HashMap<String, Boolean>()).proof;
        } catch (Exception e) {
            e.printStackTrace();
            System.err.println("Couldn't proceed " + expression.toString());
            return null;
        }
    }

    public static void main(String[] args) throws Exception {
        FastScanner in = new FastScanner(new File("input.txt"));
        PrintWriter out = new PrintWriter("output.txt");

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
                msg += entry.getKey() + ((Boolean)entry.getValue() ? "=И" : "=Л") + ", ";
                it.remove(); // avoids a ConcurrentModificationException
            }
            out.println(msg);
        }
        out.close();
    }
}
