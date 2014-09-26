class A
  attr :a
  def initialize(a)
    @a = a
  end
  def self.unapply(a)
    print("unapply\n")
    if (a.a > 0) then
      [a.a]
    else
      nil
    end
  end
end

a1 = A.new(1)
a2 = A.new(-1)
b =
  if a1.is_a?(A) && (tmp=A.unapply(a1)) != nil
  then (lambda{|a| a}).call(tmp[0])
  else (lambda{|k| -k.a}).call(a1)
  end
b2 =
  if a2.is_a?(A) && (tmp=A.unapply(a1)) != nil
  then (lambda{|a| a}).call(tmp[0])
  else (lambda{|k| -k.a}).call(a1)
  end

printf("b=%d\n",b)
printf("b2=%d\n",b2)
