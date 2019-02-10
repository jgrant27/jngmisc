def collatz(n)
  arr = [n]
  while 1 != n
    n.even? ? arr << n / 2 : arr << (n * 3) + 1
    n = arr.last
  end
  return arr
end

(1..ARGV[0].to_i).each do |i|
  res = collatz i
  puts "#{res}\n#{res.count} steps"
end
