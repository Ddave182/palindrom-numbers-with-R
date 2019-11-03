# Problem description
# A palindromic number reads the same both ways. The largest palindrome made from the product of two
# 2-digit numbers is 9009 = 91 × 99.
# Write a program that finds the largest palindrome made from the product of two 3-digit numbers.

## creating a function, which checks if a number is a Palindrom.
palindromCheck <- function(arbitraryNumber) # initialize function
{
  vector <- as.numeric(strsplit(as.character(arbitraryNumber), "")[[1]]) # coverting the number into a vector, to catch every character
  length <- length(vector) # getting the length of the number
  for (count in 1:ceiling(length(vector)/2)) { # for loop which iterates over the vecotor from the edges to the center
    if (vector[count] != vector[length + 1 - count]) { # if to check if the current numbers are the same
      return(FALSE) # if not, the loop stops and returns FALSE
    }
  }
  return(TRUE) # if all the charcaters pass all ifs, the function will return TRUE
}


###--main--###
## Main program starts here
## We will now try to find the highest palindrom product 

nums <- 100:999 # creating the numbers from 100 to 999 / all 3 digits numbers
grid <- expand.grid(nums, nums) # generating a dataframe which consists of all possible multiplications of these numbers
grid # inspecting the dataframe
nrow(grid) # counting the rows of the dataframe

highestProduct  <- 0 # initialize the variable for the later highest product
# and its factors
factor1 <- 0
factor2 <- 0

## now comes the for loop, which calculates all the possible products
for (count in 1:nrow(grid)) { 
  multiplier <- grid[count, 1] # set the multiplier for this round
  multiplicand <- grid[count, 2] # set the multiplicand for this round
  product = multiplier * multiplicand # calculate the product
  if (palindromCheck(product)) { # check if it is a palindrom
    ### text output in the form of Product: 543345 | Factors: 555 & 979
    cat("Product:", product)  
    cat(" | Factors:", multiplier, "&", multiplicand, "\n")
    if (product > highestProduct) { # check if the product is higher, than the current highestProduct
      highestProduct <- product # set a new highest product
      # and its factors
      factor1 <- multiplier
      factor2 <- multiplicand
    } 
  }
}

# print the highest product, and its factors 
highestProduct # 906609
factor1 # 993
factor2 # 913




