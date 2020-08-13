# O(1) space complexity
# O(n^3) time complexity

def longestPalindrome(s: str) -> str:
    longest = ""
    for i in range(0, len(s)):
        for j in range(i + 1, len(s) + 1):
            if s[i:j] == s[i:j][::-1] and len(longest) < len(s[i:j]):
                longest = s[i:j]
    return longest

assert "" == longestPalindrome("")
assert "a" == longestPalindrome("a")
assert "bab" == longestPalindrome("babad")
assert "bb" == longestPalindrome("bb")
assert "bb" == longestPalindrome("cbbd")
assert "ccc" == longestPalindrome("ccc")
assert "aaaa" == longestPalindrome("aaaa")
assert "anana" == longestPalindrome("bananas")
