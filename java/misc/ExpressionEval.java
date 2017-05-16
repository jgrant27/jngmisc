// package whatever; // don't place package name!

import java.io.*;

class ExpressionEval
{
    static String ops = "/*+-";
    static String digits = "01234567890";

    static Integer evaluate (String expression) {
        //TODO: Add input checks here

        Integer result = null;
        String currNumber = "";
        String currOp = "";

        for (Integer i=0; i<expression.length(); i++) {
            String ch = new Character(expression.charAt(i)).toString();
            if (digits.contains(ch)) {
                currNumber += ch;

            }
            if (ops.contains(ch) || i==expression.length()-1) {
                if (null == result) {
                    result = Integer.valueOf(currNumber);
                }
                if ((null != currOp && !ch.equals(" "))
                      || i==expression.length()-1) {
                    if (currOp.equals("/")) {
                        result /= Integer.valueOf(currNumber);
                    } else if (currOp.equals("*")) {
                        result *= Integer.valueOf(currNumber);
                    } else if (currOp.equals("+")) {
                        result += Integer.valueOf(currNumber);
                    } else if (currOp.equals("-")) {
                        result -= Integer.valueOf(currNumber);
                    }
                    currOp = ch;
                    currNumber = "";
                }
            }
        }

        return result;
    }

    public static void main (String[] args) throws java.lang.Exception
    {
        // Evaluates simple arithmetic integers
        // ignoring decimals, /, *, + and -
        String expression = "1 + 2 * 3 - 3 / 2"; // 3
        Integer result = evaluate(expression);
        System.out.println(expression + " = " + result);
    }
}
