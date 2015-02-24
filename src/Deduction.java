import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

public class Deduction extends Proof{
    // premise, A |- B => premise |- A -> B
    private Expression A, B;
    private ArrayList<Expression> premise = new ArrayList<Expression>();
    private ArrayList<Expression> proven = new ArrayList<Expression>();

    // premise |- result
    public Deduction(Expression[] premise, Expression result) {
        super();

        if (premise == null) {
            A = null;
        } else {
            this.premise.addAll(Arrays.asList(premise).subList(0, premise.length - 1));
            A = premise[premise.length - 1];
        }
        B = result;
    }

    // premise |- A -> B
    public String getHeader() {
        String result = "";
        for (Expression p: premise) {
            if (!result.equals(""))
                result += ",";
            result += p.toString();
        }
        result += "|-" + (new Consequence(A, B)).toString();
        return result;
    }

    private boolean isPremise(Expression expression) {
        for (Expression p: premise) {
            if (p.equals(expression))
                return true;
        }
        return false;
    }

    // proof for A->C0, proven.get(mp[0]) is A->C1, proven.get(mp[1]) is A->(C1->C0)
    private Expression[] deductiveMP(Expression A,  int[] mp, int offset) {
        HashMap<String, Expression> map = new HashMap<String, Expression>();
        map.put("A", A);
        map.put("C1", proven.get(mp[1]).right);
        map.put("C1C0", proven.get(mp[0]).right);
        map.put("C0", proven.get(mp[0]).right.right);
        return new Expression[] {
                // 2nd axiom
                ExpressionFactory.parse("(A -> C1) -> (A -> (C1 -> C0)) -> (A -> C0)").replace(map),
                // M.P. mp[1], last statement
                ExpressionFactory.parse("(A -> (C1 -> C0)) -> (A -> C0)").replace(map),
                // M.P. mp[0], last statement
                ExpressionFactory.parse("A -> C0").replace(map),
        };
    }

    // returns number of lines (A->(Cj->Ci)) and A->Cj or null if there aren't such lines
    private int[] isDeductiveMP(Expression Ci) {
        for (Expression p: proven) {
            if (p instanceof Consequence && p.left.equals(A)
                    && p.right instanceof Consequence && p.right.right.equals(Ci)) {
                Expression Cj = p.right.left;
                for (Expression p1: proven) {
                    if (p1 instanceof Consequence && p1.left.equals(A) && p1.right.equals(Cj))
                        return new int[]{proven.indexOf(p), proven.indexOf(p1)};
                }
            }
        }

        return null;
    }

    public Expression[] nextLine( Expression line) throws Exception {
        int mp[];
        if (line.equals(A)) {
            HashMap<String, Expression> map = new HashMap<String, Expression>();
            map.put("X", line);
            Expression[] result = {
                    /// 1st axiom
                    ExpressionFactory.parse("X->X->X").replace(map),
                    // 2nd axiom
                    ExpressionFactory.parse("(X->(X->X)) -> (X->(X->X)->X) -> (X->X)").replace(map),
                    // MP two lasts
                    ExpressionFactory.parse("(X->(X->X)->X)->(X->X)").replace(map),
                    // first axiom
                    ExpressionFactory.parse("X->(X->X)->X").replace(map),
                    // MP two lasts
                    ExpressionFactory.parse("X->X").replace(map)
            };
            proven.addAll(Arrays.asList(result));
            return result;
        } else if (isAxiom(line) != -1 ||
                   isPremise(line)) {
            HashMap<String, Expression> map = new HashMap<String, Expression>();
            map.put("A", line);
            map.put("X", A);
            Expression[] result = new Expression[]{
                    // axiom
                    ExpressionFactory.parse("A").replace(map),
                    // 1st axiom
                    ExpressionFactory.parse("A->X->A").replace(map),
                    // MP two lasts
                    ExpressionFactory.parse("X->A").replace(map)
            };
            proven.addAll(Arrays.asList(result));
            return result;
        } else if ((mp = isDeductiveMP(line)) != null) {
            Expression[] result = deductiveMP(A, mp, proven.size());
            proven.addAll(Arrays.asList(result));
            return result;
        } else {
            System.err.println("Couldn't proceed expression " + line.toString());
        }
        return null;
    }

    // premise, a |- b
    public static ArrayList<Expression> deduct(ArrayList<Expression> premise, Expression a, Expression b,
                                               ArrayList<Expression> proof) throws Exception {
        ArrayList <Expression> result = new ArrayList<Expression>();
        premise.add(a);
        Deduction curr = new Deduction(premise.toArray(new Expression[premise.size()]), b);
        for(Expression e: proof) {
            Expression[] tmp = curr.nextLine(e);

            for(Expression e1: tmp) {
                result.add(e1);
            }
        }

        return result;
    }

    public static void main(String[] args) throws Exception {
        FastScanner in = new FastScanner(new File("input.txt"));
        PrintWriter out = new PrintWriter("output.txt");

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
            for (Expression r : result) {
                out.println(r.toString());
            }
        }

        out.close();
    }
}