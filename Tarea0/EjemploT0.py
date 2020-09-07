Python 3.8.5 (tags/v3.8.5:580fbb0, Jul 20 2020, 15:43:08) [MSC v.1926 32 bit (Intel)] on win32
Type "help", "copyright", "credits" or "license()" for more information.
>>> x=23
>>> x
23
>>> datos=[22,41,37,99,12]
>>> datos
[22, 41, 37, 99, 12]
>>> n=len(datos)
>>> n
5
>>> min(datos)
12
>>> max(datos)
99
>>> sum(datos)
211
>>> ord=sorted(datos)
>>> ord[0]
12
>>> ord[6]
Traceback (most recent call last):
  File "<pyshell#11>", line 1, in <module>
    ord[6]
IndexError: list index out of range
>>> ord[3]
41
>>> ord[-1]
99
>>> datos2=[22,41,34,66,99,89,41,63,39,52,101]
>>> import numpy as np
Traceback (most recent call last):
  File "<pyshell#15>", line 1, in <module>
    import numpy as np
ModuleNotFoundError: No module named 'numpy'
>>> import numpy as np
Traceback (most recent call last):
  File "<pyshell#16>", line 1, in <module>
    import numpy as np
ModuleNotFoundError: No module named 'numpy'
>>> import numpy as np
>>> np.median(datos2)
52.0
>>> from scipy.stats import describe
describe(datos2)
>>> describe(datos2)
DescribeResult(nobs=11, minmax=(22, 101), mean=58.81818181818182, variance=741.9636363636364, skewness=0.4320598413184881, kurtosis=-1.1655869324616286)
>>> np.quantile(data,0.5)
Traceback (most recent call last):
  File "<pyshell#21>", line 1, in <module>
    np.quantile(data,0.5)
NameError: name 'data' is not defined
>>> np.quantile(datos2,0.5)
52.0
>>> M=np.zeros((4,4))
>>> M
array([[0., 0., 0., 0.],
       [0., 0., 0., 0.],
       [0., 0., 0., 0.],
       [0., 0., 0., 0.]])
>>> a=[3,6,2]
>>> b=[1,9,5]
>>> c=[x==y for (x,y) in zip(a,b)]
>>> c
[False, False, False]
>>> all(c)
False
>>> any(c)
False
>>> a+b
[3, 6, 2, 1, 9, 5]
>>> a/b
Traceback (most recent call last):
  File "<pyshell#32>", line 1, in <module>
    a/b
TypeError: unsupported operand type(s) for /: 'list' and 'list'
>>> d=np.array(a)
>>> e=np.array(b)
>>> d/e
array([3.        , 0.66666667, 0.4       ])
>>> z=8.4
>>> from math import floor, ceil
>>> ceil(z)
9
>>> floor(z)
8
>>> round(z)
8
>>> from math import sqrt, exp, sin, cos, tan, log, factorial
>>> sqrt(z)
2.898275349237888
>>> exp(z)
4447.066747699858
>>> sin(z)
0.8545989080882804
>>> cos(z)
-0.5192886541166856
>>> tan(z)
-1.645710726227902
>>> log(z)
2.128231705849268
>>> factorial(3)
6
>>> seq=np.arange(1,100,5)
>>> seq
array([ 1,  6, 11, 16, 21, 26, 31, 36, 41, 46, 51, 56, 61, 66, 71, 76, 81,
       86, 91, 96])
>>> s=[2]*20
>>> s
[2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]
>>> len(s)
20
>>> w=3
>>> r=None
>>> r=w*2 if w>4 else 5-w
>>> r
2
>>> if r!=3:
	print('no es tres')

	
no es tres
>>> for i in range(1,5):
	print(2**i)

	
2
4
8
16
>>> while w>0:
	print('menos uno')
	w-=1

	
menos uno
menos uno
menos uno
>>> w
0
>>> def rutina(x):
	return 2**x

>>> rutina(4)
16
>>> from random import random
>>> random()
0.02902875508292002
>>> random(3)
Traceback (most recent call last):
  File "<pyshell#75>", line 1, in <module>
    random(3)
TypeError: random() takes no arguments (1 given)
>>> from numpy.random import rand
>>> rand(3)
array([0.13139184, 0.99020696, 0.59864568])
>>> from random import sample
>>> sample([i for i in range(0,10)],5)
[1, 5, 4, 0, 9]
>>> 