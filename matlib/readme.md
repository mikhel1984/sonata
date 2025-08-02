# Mathematical Library

This folder contais **Lua** modules for working with different mathematical objects.
They can be used either in the **Sonata** program or independently.

## Available modules 

### asciiplot (Ap)

Visualize data with pseudographic.

```lua
fig = Ap()    -- create figure
fig:setX {range={-3.14, 3.14}}              -- axis configuration
fig:plot(math.sin, 'sin', math.cos, 'cos')  -- add data
fig:title 'Trigonometry'                    -- set title
print(fig)    -- get as string
```

### bigint (Int) 

Operations with arbitrary long integers.

```lua
-- create from string (for example)
a = Int("12345678987654321") 
b = a * Int(456)    -- from number
c = b:factorize()   -- list of multipliers
d = b:digits(20)    -- list of digits for base 20
```

### complex (Z)

Operatoins with complex numbers. 

```lua
a = Z(1,2)       -- first number
b = 3 + 4*Z:i()  -- second number
c = a ^ b         -- complex power
```

### const (C)

Collection of scientific constants. 

```lua
C.phy.e             -- electron charge
C.phy.e_u           -- charge units
C:add("foo", -1/12)  -- add own value, call with C.foo
```

### data (D)

Data processing and statistics.

```lua
X = {1, 2, 3, 4, 5, 6}  -- data
a = D:moment(X, 2)     -- central moment
b = D:filter(X, "x > 2")  -- get X[i] > 2
```

### geodesy (Geo)

Coordinate transformations and other geodetic tasks.

```lua
wgs = Geo.WGS84      -- get ellipsoid parameters
p = wgs:toXYZ{B=10,L=20,H=-30} -- find geocentric coordinates
s, a1, a2 = wgs:solveInv({B=1,L=2},{B=10,L=20}) -- solve inverse problem
```

### gnuplot (Gp)

Interface functions for calling **Gnuplot**.

```lua
Gp.plot({0,1,2,3},math.sin,'sin') -- plot sine in points
a = Gp()             -- fine tune
a:add {math.sin, title='sin'}
a:add {math.cos, title='cos'}
a:show()
```

### graph (Graph)

Operations with graphs.

```lua
a = Graph()       -- undirected graph
a:addEdges {{'a','b'},{'a','c'},{'c','d',2}}  -- set edges
a:nodes()         -- list of nodes
a:isComplete()    -- check completenes
```

### lens (Lens)

Matrix methods in paraxial optics.

```lua
-- define system structure
sys = Lens:R(200,1,1.56)..Lens:T(5)..Lens:R(-200,1.56)
y, V = sys(5, 0.1)    -- ray transformation
pts = sys:cardinal()  -- find cardinal points
```

### matrix (Mat)

Matrix operations.

```lua
a = Mat {{1,2},{3,4}}  -- create matrix
b = a:det()          -- find determinant
c = a:inv()          -- matrix inversion 
```

### numeric (Num)

Functions for numerical calculations. 

```lua
a = Num:solve(math.sin, 3, 4) -- find root in range
b = Num:int(math.sin, 0, 1) -- numerical integration
-- solve ode x' = x*y for x = {0,3} and y(0) = 1
tbl, yn = Num:ode45(function (x,y) return x*y end, {0,3}, 1)
```

### polynom (Poly)

Operations with polynomials.

```lua
a = Poly {1,2,1}     -- new polynomial
b = a:val(0)         -- polynomial value
c = a:der()          -- derivative
```

### quaternion (Quat)

Operations with quaternions. 

```lua
a = Quat {1,2,3,4}   -- set {w,i,j,k}
b = Quat {w=2,k=1}   -- using names
c = a * b
```

### random (Rand)

Random number generator.

```lua
a = Rand()   -- get value from 0 to 1
b = Rand:norm()  -- normal distributed number
c = Rand:new()   -- custom uniform generator
```

### rational (Rat)

Computations with rational numbers.

```lua
a = Rat(1,2)         -- 1/2
b = a / Rat(4,8)
c = Rat:fromCont{2,3,4,5}  -- from continuated fraction
```

### special (Spec)

Special functions.

```lua
a = Spec:beta(3,4)   -- beta-function 
b = Spec:gamma(-1.5) -- gamma-funciton
c = Spec:besselj(3, 1.5) -- Bessel's function
```

### symbolic (Sym)

Symbolic variables.

```lua
a = Sym:parse('x^2+2*x+1')  -- from equation
b = a:diff(Sym('x'))   -- find derivative
c = a + b              -- some calculations
```
### units (Unit)

Units conversation.

```lua
Unit.rules['h'] = Unit(60,'min')  -- first rule
Unit.rules['min'] = Unit(60,'s')  -- second rule
a = 1 * Unit 'm/s'   -- set variable
b = a('km/h')        -- get value in km/h
```

### extremum (Ex)

Optimization problems.

```lua
-- scalar argument
a = Ex:minimum1D(function (x) return x*x end, -1, 1)
-- vector argument
b = Ex:maximum(function (y) return -y(1)^2-y(2)^2 end, Mat:V{2, 3})
```

### qubit (Qb)

Emulate quantum computations.

```lua
a = Qb'00' + Qb'11'   -- define state
b = Qb:gates(2):H()   -- defien system
c = b(a)              -- apply transforms
```
