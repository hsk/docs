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
b = if (a=A.unapply(a1)) != nil then a[0] else -a1.a end
b2 = if (a=A.unapply(a2)) != nil then a[0] else -a1.a end

printf("b=%d\n",b)
printf("b2=%d\n",b2)
