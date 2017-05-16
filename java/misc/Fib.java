class Fib {

    static Long getNthFib(Long n) {
        if (n == 0 || n == 1)
            return n;

        Long a = 0L;
        Long b = 1L;
        Long c = a + b;
        for (int i=2; i<=n; i++) {
            c = a + b;
            a = b;
            b = c;
        }

        return c;
    }

    public static void main (String[] args) throws java.lang.Exception
    {
        Long n = 20L; // 6765
        System.out.println(getNthFib(n));
    }

}
