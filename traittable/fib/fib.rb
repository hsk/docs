def fib(n)
  if n < 2 then
    1
  else
    fib(n-2)+fib(n-1)
  end
end

class Fib
  def initialize(n)
    @n = n
  end
 
  def fib
    if @n < 2 then
      1
    else
       Fib.new(@n-2).fib + Fib.new(@n-1).fib
    end
  end
end

def tim
  n = Time.now
  n.to_i * 1000 + n.usec / 1000
end

start = (tim)
puts(fib(40));
printf("%dms\n",(tim)-start, "ms");

start = (tim)
puts(Fib.new(40).fib);
printf("%dms\n",(tim)-start, "ms");
