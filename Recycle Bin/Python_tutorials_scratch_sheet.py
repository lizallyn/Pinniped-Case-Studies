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

