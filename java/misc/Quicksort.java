import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

class Quicksort {

    static List<Integer> sort(List<Integer> nums, int pivot) {
        ArrayList<Integer> left = new ArrayList<Integer>();
        ArrayList<Integer> right = new ArrayList<Integer>();
        ArrayList sorted = new ArrayList<Integer>();

        for (Integer num : nums) {
            if (num<=pivot) {
                left.add(num);
            } else {
                right.add(num);
            }
        }

        List<Integer> leftSorted = left.size() > 1 ?
            sort(left.subList(1, left.size()), left.get(0)) : left;
        sorted.addAll(leftSorted);
        sorted.add(pivot);
        List<Integer> rightSorted = right.size() > 1 ?
            sort(right.subList(1, right.size()), right.get(0)) : right;
        sorted.addAll(rightSorted);
        return sorted;
    }

    public static void main (String[] args) throws java.lang.Exception
    {
        Integer[] numsArr = new Integer[]{6, 1, 3, 2, 4, 9, 5, 8, 7};
        List<Integer> nums = Arrays.asList(numsArr);
        List<Integer> newNums =
            sort(nums.subList(1, nums.size()), nums.get(0));
        for (int i=0; i<newNums.size(); i++) {
            System.out.print(newNums.get(i) + " ");
        }
        System.out.println();
    }

}
