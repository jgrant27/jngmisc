channel = Channel(Int32).new

CNT = 10000

CNT.times do |i|
  spawn do
    #sleep 1.seconds
    channel.send(i * 2)
    #puts("Sent #{i * 2}")
  end
end

sum = 0
CNT.times do
  n = channel.receive
  sum += n
  puts("Receive #{n}")
end
puts sum # => 90
