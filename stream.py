# a thunk is a function that takes no paramaters
# e.x. int thunk(void) in C
# a stream is an ordered pair of (value, generator) where generator is a thunk. value is the current value of the stream

# n is of type N (e.x. the natural numbers)
# next : N -> N produces the next value in a sequence given the current value
# make_s produces a stream with starting value n and a generator powered by next
def make_s(n, next):
    return (n, lambda : make_s(next(n), next))

# lambda notation:
# lambda x: x*2 is a function with no name which takes in x and returns x*2
# lambda x, y: x+y takes in x and y and returns x+y
# lambda : 1 takes in nothing and returns 1
# it is a quick way to construct functions without having to give them a name

# produces the next natural number
next_nat = lambda x: x+1

# stream of natural numbers
nat_s = make_s(0, next_nat)

# unroll gets the next n values of the stream s
def unroll(s, n):
    l = []
    for i in range(n): # repeat n times
        l.append(s[0]) # append the current value
        s = s[1]() # move the stream forward by calling the thunk

    return l

unroll(nat_s, 10) # print the first 10 natural numbers


# makes a 'dynamic' stream
# i.e. the next function changes over time, according to nextmod
def make_ds(n, next, nextmod):
    return (n, lambda : make_ds(next(n), nextmod(next), nextmod))


# we can think of next as 'velocity' and nextmod as 'acceleration':
def accel(nxt):
    return lambda x: next_nat(next_nat(nxt(x)))
# accel = 2
# velocity = integral(accel) = 2x
# displacement = integral(velocity) = x^2
# hence the double application of next_nat above
# i.e. first we add 1, then 3, then 5, then 7, etc.

parab_s = make_ds(0, next_nat, accel)

# print the path of a parabola
unroll(parab_s, 10)


# if we only apply next_nat once to nxt then we obtain the triangular numbers
def triangular(nxt):
    return lambda x: next_nat(nxt(x))

tri_s = make_ds(0, next_nat, triangular)
unroll(tri_s, 10)


from math import sqrt, floor
# this next-function finds the next perfect square above some number, which happens to also match up with our parabola above
def next_psquare(n):
    i = next_nat(n)
    while True: # search from n+1 to infinity
        if (floor(sqrt(i)))**2 == i: # perfect square
            return i
        i += 1 # otherwise keep searching

# and this stream uses the above next-function to find all the perfect squares
psquare_s = make_s(0, next_psquare)
unroll(psquare_s, 10)

# the following function creates a next-function which searches for the next number above a given number that passes a given test
def make_next(test):
    def nxt(n):
        i = next_nat(n)
        while True: # search from n+1 to infinity
            if test(i): # passes test
                return i
            i += 1 # otherwise keep searching

    return nxt

# we can now recreate next_psquare as a special case of this kind of next-function
is_psquare = lambda x: floor(sqrt(x))**2 == x
next_psquare2 = make_next(is_psquare)
psquare_s2 = make_s(0, next_psquare2)
unroll(psquare_s2, 10)

# another example, which gets the next number divisible by some given number
divisible = lambda x, d: x // d * d == x # general divisor-checking function
div_by_5 = lambda x: divisible(x, 5) # special case of this function (example of 'currying', you reduce the number of paramaters a function takes by making it constant)
next_div = make_next(div_by_5)
div_s = make_s(0, next_div)
unroll(div_s, 10)
# (we could also achieve this by just adding 5 each time)

# now we check multiple divisors instead of just 1
divs = [2,3]
div_by_any = lambda x, l: any([divisible(x, d) for d in l]) # we use 'list comprehension' (a cool python feature) to check if every element of l divides x
div_by_divs = lambda x: div_by_any(x, divs) # currying again
next_dbd = make_next(div_by_divs)
dbd_s = make_s(0, next_dbd)
unroll(dbd_s, 10)

# now we invert this
# skip the general case, curry immediately
not_dbd = lambda x: not div_by_any(x, divs)
next_ndbd = make_next(not_dbd)
ndbd_s = make_s(0, next_ndbd)
unroll(ndbd_s, 10)


# now we combine not_dbd with make_ds to create a stream of prime numbers

# the sieve of eratosthenes: 2 is a prime. the next number not divisible by 2 is also a prime (3). the next number not divisible by 2 or 3 is a prime (5). the next number not divisible by 2 or 5 or 5 is a prime (7). etc.
# unlike most prime-checking functions, the sieve of eratosthenes generates all the primes up to a certain point, instead of just checking one at a time. therefore it is useful if, say, we want to generate all the primes under 1,000,000, because it uses information about previous primes to more efficiently test for new primes.

# a nice way to demonstrate the sieve by hand: write out every number from 1 to 100. circle 2. cross out all other multiples of 2. circle the next not-crossed-out number (3). cross out all multiples of 3. etc.
# in our implementation, however, we just check for divisibility directly

# first, we modify make_ds to automatically apply make_next to a given test
def make_nds(n, test, testmod):
    nxt = make_next(test)
    return (n, lambda : make_nds(nxt(n), testmod(test, n), testmod))

test_prime = lambda x: not divisible(x, 2) # the sieve at the start is very generous
def test_add(test, n): # however it gets more strict as we find more primes
    return lambda x: test(x) and not divisible(x, n) # every time we add a requirement to our test function, we wrap another function around it. this is not computationally efficient but it is theoretically sound.

prime_s = make_nds(2, test_prime, test_add)
unroll(prime_s, 10)
