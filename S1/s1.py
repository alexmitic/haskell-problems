def dna():
    return "^[ACGT]+$" # Check if each character is one of ACGT

def sorted():
    return "^9*8*7*6*5*4*3*2*1*0*$" # Match all nines form the beginning of string, then all eights and so on...

def hidden1(subString):
    return subString

def hidden2(subString):
    temp = []

    for n in subString: # For all letter is given string
        temp.append("[^" + n + "]*") # All that is not n
        temp.append("[" + n + "]") # One of n

    return ''.join(temp)

# First check if a number. Negativ numbers are allowd
# Then check for zero or more operators followd by numbers  
# Lastly check for equal sign followd by same patter as before equal 
def equation():
     return "^[\+\-]?[\d]+(?:[\/\*\+\-][\d]+)*(?:[\=]?[\+\-]?[\d]+(?:[\/\*\+\-][\d]+)*)?$"

# Obvious. Bruteforce 
def parentheses():
     return "^(?:\((?:\((?:\((?:\((?:\(\))*\))*\))*\))*\))+$"

# Check if zero or more non-ascending numbers 
# Followed by three numbers in ascending order 
# And ending with zero or more non-ascending numbers
# 
def sorted3():
    return "^[\d]*(?:01[2-9]|[0-1]2[3-9]|[0-2]3[4-9]|[0-3]4[5-9]|[0-4]5[6-9]|[0-5]6[7-9]|[0-6]7[8-9]|[0-7]89)[\d]*$"