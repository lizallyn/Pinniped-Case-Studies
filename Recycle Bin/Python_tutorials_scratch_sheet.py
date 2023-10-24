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
while x <= 5:
  print(x)
  x = x + 1
# end

