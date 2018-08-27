# -*- coding: utf-8 -*-
"""
Created on Mon Jul 02 13:52:54 2018

@author: AH0667765
"""

import numpy as np

my_arr = np.arange(100000)

my_list = list(range(100000))


%time for _ in range(10): my_arr2 = my_arr * 2
%time for _ in range(10): my_list2 = [x * 2 for x in my_list]


data = np.random.randn(2, 3)
data

data * 10

data + data

data.shape

data.dtype
#   Creating ndarrays

data1 = [6, 7.5, 8, 0, 1]
arra1 = np.array(data1)
arra1

data2 = [[1,2,3,4],[5,6,7,8]]
arra2 = np.array(data2)
arra2

arra1.ndim
arra2.ndim

arra1.shape
arra2.shape

arra1.dtype
arra2.dtype

np.zeros(10)
np.zeros((3,3))

np.empty((2,3,2))

np.arange(10)

# DATA TYPES FOR NDARRAYS

arr3 = np.array([1,2,3,4], dtype = np.float64)
arr4 = np.array([1,2,3,4], dtype = np.int32)

arr3.dtype
arr4.dtype

# Indexing with slicing

arr = np.arange(10)
arr[1:6]

arra2d = np.array(([1,2,3],[4,5,6],[7,8,9]))

arra2d[0,0]
arra2d[0,1]
arra2d[:2] # select the first two rows of arr2d

arra2d[:2,:1]

arra2d[1,:2] # second row and third column

# Q select the third column but only the first two rows
arra2d[:2, 2]

arra2d[:, :1]



# Boolean Indexing

names = np.array(['Bob', 'Joe', 'Will', 'Bob', 'Will', 'Joe', 'Joe'])
data = np.random.randn(7, 4)

names == 'Bob'
data[names == 'Bob']


data[data<0] = 0

data[names != 'Joe'] = 7


# Fancy Indexing

arr = np.empty((8, 4))

arr

for i in range(8) :
    arr[i] = i

arr[[4,3,0,6]]

arr[[-3,-5,-7]]


arr2 = np.arange(32).reshape((8, 4))

arr2[[1,5,7,2], [0,3,1,2]]

arr2[[1, 5, 7, 2]][:, [0, 3, 1, 2]]


# Transposing Arrays and Swapping Axes




























