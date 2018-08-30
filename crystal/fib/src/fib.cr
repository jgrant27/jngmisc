# TODO: Write documentation for `Helloworld`
module Helloworld
  VERSION = "0.1.0"

  def fib(n)
    if n <= 1
      1
    else
      fib(n - 1) + fib(n - 2)
    end
  end

  # TODO: Put your code here
end

include Helloworld
puts fib(42)
