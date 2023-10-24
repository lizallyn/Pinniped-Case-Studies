# Python Tutorials
# October 24, 2023

# Switch to Python if needed
# reticulate::repl_python()


print("Hello World")

age = 20
print(age)

name = input("What is your name? ")
print("Hello " + name)

birth_year = input("Enter your birth year: ")
age = 2023 - birth_year
# this throws an error because input gives a string but need integer

age = 2023 - int(birth_year)
age
# all good now!
# types include int, float, bool, str

first = input("Give the first number: ") #10
second = input("Give the second number: ") #20
solution = int(first) + int(second) # if I did this without int, gives "1020"
solution

first = float(input("Give the first number: ")) #10
second = float(input("Give the second number: ")) #20.2
solution = first + second # bc now have decimals with float
print("Solution: " + str(solution))

# functions for objects
course = "python for Beginners"
course.upper() # makes it all uppercase as new object
# course is still lowercase, would need to save as new object
print(course.find("y")) # returns index of "y" in string
print(course.replace("for", "4")) # replaces "for" with "4"
# strings are immutable, cannot edit can only make a new one with the given changes

print("Python" in course) # returns false because not capitalized
print("python" in course)

# Arithmetic
x = 10
x = x + 3
x += 3 # same as above line

x = 10 + 3 * 2 # order of operations is built in
x = 3 > 2 # so x = True
3 == 2 # same as R

# logical operators
price = 25
print(price > 10 and price < 30) # and is like &&
price = 5
print(price > 10 or price < 30) # still returns true
print(not price > 10) # returns the opposite like a "-" in R

# if statements
temperature = 35
if temperature > 30:
  print("It's a hot day") # have to use double quotes when using an apostrophe '
  print("Drink plenty of water")
elif temperature > 20:
  print("It's a nice day")
elif temperature > 10:
  print("It's a bit cold")
print("Done")
# doesn't run everything automatically like R, highlight the whole thing and run

weight = int(input("Enter your weight: "))
unit = input("(K)g or (L)bs: ")
if unit.upper() == "K":
  converted = str(weight / 0.45)
  print("Weight in Lbs: " + converted)
else:
  converted = str(weight * 0.45)
  print("Weight in Kgs: " + converted)
# end
# have to run the blank line at the end of the statements or it just thinks forever!!!

# while loops
x = 1
while x <= 1000: # the 1_000 is easier to look at?
  print(x)
  x = x + 1
# end
while x <= 10: # the 1_000 is easier to look at?
  print(x * '*') # multiplies the asterisks by the value of x, so prints an asterisk pyramid
  x = x + 1
# end

# lists
names = ["Liz", "John", "Bob", "Sam"]
print(names)
print(names[0]) # indexing starts at 0
print(names[-2])
names[1] = "Jon"
print(names[0:2])

numbers = [1, 2, 3, 4, 5]
numbers_copy = numbers
numbers.append(6)
print(numbers)
print("the copy is" + str(numbers_copy))
# both lists point to the same underlying numbers
# so edits to one are edits to both!!

x = [1]
x + x
x * 3
# can build lists with those operators

numbers[::2] # specifies a "stride" of 2, so slices every second item

x = (1,) # creates a tuple of length 1, needs trailing comma
type(x)
len(x)
x = 1,2 # also a tuple
x = 1, # also a tuple
print(f"{type(x) = }; {len(x) = }; {x = }") # interpolated string literals (???)

x = (1,2,3)
a,b,c = x #"unpacking"
print(a)

xx = (("a", 1),
      ("b", 2))
for x1, x2 in xx:
  print("x1 = ", x1)
  print("x2 = ", x2)
# end

x = (1,2,3)
a, *the_rest = x # assigns leftovers to the_rest
a
the_rest

# dictionaries
# sort of like environments where you can refer to objects by name
d = {"key1": 1,
     "key2": 2}
d["key3"] = 3
d[1] # cannot index by number, unordered container

# sets
s = {1,2,3} # create with curlies?
type(s)
s.add(6) # adds to the set?
s

# iteration
l = [1,2,3]
it = iter(l)
it
next(it)

d = {"key1": 1,
     "key2": 2}
for key in d:
 print(key)

for value in d.values():
  print(value)

for key, value in d.items():
  print(key, ":", value)

# comprehensions (similar to lapply kinda)
x = [1,2,3]
l = [element + 100 for element in x]
l

# functions
def my_function(name = "World"):
  print("Hello", name) # requires explicit return function unlike R
my_function("Liz")

def my_func(*args, **kwargs):
  print("args = ", args)
  print("kwargs = ", kwargs)
# functions can take variable numbers of arguments
my_func(1,2,3,a=4, b=5, c=6)
unnamed = (1,2,3,4,5) # unnamed arguments
named = {"a":1, "b": 2, "c": 3} # named arguments
my_func(*unnamed, **named)

## classes are typically CamelCase and functions are snake_case ##

# classes
class MyClass:
  pass # pass means do nothing

MyClass
type(MyClass)

instance = MyClass()
instance
type(instance)
dir(MyClass) # has a bunch of biult-in attributes


