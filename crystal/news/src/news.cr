# TODO: Write documentation for `News`
module News
  VERSION = "0.1.0"

  CNT = 1000
  #channel = Channel(Int32).new(CNT) # buffered
  channel = Channel(Int32).new() #unbuffered

  spawn do
    CNT.times do |i|
      #sleep 1.seconds
      puts("Send #{i+1}")
      channel.send(i+1)
    end
    puts "End of send fiber."
  end

  sum = 0
  #spawn do
    CNT.times do
      #spawn do
      n = channel.receive
      sum += n
      puts("Receive #{n}")
      #end
    end
  #end

  Fiber.yield # Allows printing "End of send fiber."

  puts sum

end
