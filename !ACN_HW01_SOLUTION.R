#ex1:
colsum_matrix = function(mat) {
  if (!is.matrix(mat)) {
    return("input must be of type 'matrix'")
  }
  colsums = replicate(ncol(mat), 0)
  for (i in 1:nrow(mat)) {
    for (j in 1:ncol(mat)) {
      colsums[j] = colsums[j] + mat[i, j]
    }
  }
  return(colsums)
}
#t1
mat1 = matrix(c(1, 2, 3, 11, 12, 13), nrow = 2, ncol = 3)
colsum_matrix(mat1)
#t2
name <- c("Bob", "Max", "Sam")
age <- c(25, 26, 23)
city <- c("New York", "Chicago", "Seattle")
df <- data.frame(name, age, city)
colsum_matrix(df)
#..............................................................................#
#ex2:
is_symmetric = function(mat) {
  if (!is.matrix(mat)) {
    return("input must be of type 'matrix'")
  }
  if (nrow(mat) != ncol(mat)) {
    return("matrix must be square")
  }
  for (i in 1:(nrow(mat)-1)) {
    for (j in (i+1):nrow(mat)) {
      if (mat[i, j] != mat[j, i]) {
        return("the matrix is not symmetric")
      }
    }
  }
  return("the matrix is symetric")
}
#t1
name <- c("Bob", "Max", "Sam")
age <- c(25, 26, 23)
city <- c("New York", "Chicago", "Seattle")
df <- data.frame(name, age, city)
is_symmetric(df)
#t2
mat1 = matrix(c(1, 2, 3, 2, 1, 1, 3, 1, 2), nrow = 3, ncol = 3)
is_symmetric(mat1[, 1:2])
#t3
is_symmetric(mat1)
#..............................................................................#
#ex3:
matrix_power <- function(m, k) {
  if (!is.matrix(m)) {
    return("input must be of type 'matrix'")
  }
  if (!ispositiveinteger(k)) {
    return("k must be a positive integer number")
  }
  if (dim(m)[1] != dim(m)[2]) {
    return('matrix must be square!')
  }
  matrix_power_rec(m, k)
}

ispositiveinteger <- function(k) {
  if (round(k) == k & k > 0) {
    return(TRUE)
  }
  return(FALSE)
}

matrix_power_rec <- function(m, k) {
  if (k == 1) {
    return(m)
  }
  else {
    if (k %% 2 == 0) {
      b = matrix_power_rec(m, k / 2)
      return(b %*% b)
    }
    else{
      b = matrix_power_rec(m, (k - 1) / 2)
      return(b %*% b %*% m)
    }
  }
}
#t1
name <- c("Bob", "Max", "Sam")
age <- c(25, 26, 23)
city <- c("New York", "Chicago", "Seattle")
df <- data.frame(name, age, city)
matrix_power(df, 3)
#t2
mat1 = matrix(c(1, 2, 3, 2, 1, 1, 3, 1, 2), nrow = 3, ncol = 3)
matrix_power(mat1, -3)
#t3
mat1 = matrix(c(1, 2, 3, 2, 1, 1, 3, 1, 2), nrow = 3, ncol = 3)
matrix_power(mat1, 2.5)
#t4
matrix_power(mat1[, 1:2], 3)
#t5
m = matrix(1,2,2)
m
matrix_power(m,4)