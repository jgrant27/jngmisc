from typing import List

def twoSum(nums: List[int], target: int) -> List[int]:
    dnums = {num:i for i, num in enumerate(nums)}
    for i, num in enumerate(nums):
        complement = target - num
        if complement in dnums.keys() and i != dnums[complement]:
            return [i, dnums[complement]]

assert [0, 1] == twoSum([2, 7, 11, 15], 9)
assert [1, 2] == twoSum([3, 2, 4], 6)
