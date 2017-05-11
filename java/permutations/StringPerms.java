// Print all string perms given a string

import java.util.List;
import java.util.ArrayList;

class StringPerms {

    public static void main(String args[]) {
        List<String> perms = getPerms(args[0]);
        for (String perm : perms) {
            System.out.println(perm);
        }
    }

    static List<String> getPerms(String str) {
        // Final list of all permutations
        List<String> perms = new ArrayList<String>();
        if (str==null || str.length()==1) {
            perms.add(str);
            return perms;
        }
        // Pick the sub string to generate a sub permutation based on the
        // next char
        String ch = str.substring(0,1);
        List<String> subPerms = getPerms(str.substring(1, str.length()));
        for (String subPerm : subPerms) {
            for(int i = 0; i < str.length() ; i++) {
                String newPerm = subPerm.substring(0,i) + ch +
                    subPerm.substring(i, subPerm.length());
                perms.add(newPerm);
            }
        }
        return perms;
    }

}
