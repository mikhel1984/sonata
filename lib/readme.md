# Mathematical Library

This folder contais **Lua** modules for working with different mathematical objects.
They can be used either in the **Sonata** program or independently.

## Available modules 

### array (Arr)

Manipulations with arrays of arbitrary elements.

```lua
-- create new array with specific size
a = Arr {2,2,4}
a:set({1,2,1}, 10)  -- set element, index in curly brackets
a:get{1,2,1}        -- get element
-- new array from the given
b = a:map(funciton (x) return x and 1 or 0 end) 
```

### bigint (Int) 

Operations with arbitrary long integers.

```lua
-- create from string (for example)
a = Int("12345678987654321")
b = a * Int{4,5,6}  -- table can be used
c = b:rebase(20)    -- get representation with base = 20
```

### complex (Comp)

Operatoins with complex numbers. 

```lua
a = Comp(1,2)       -- first number
b = 3 + 4*Comp._i   -- second number
c = a ^ b           -- complex power
```

### const (_C) 

Collection of scientific constants. 

```lua
_C.phy.e             -- electron charge
_C.phy.e_u           -- charge units
_C.add("foo",-1/12)  -- add own value, call with _C.foo
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
a = Graph {{'a','b'},{'a','c'},{'c','d',w=2}}  -- define graph
a:nodes()            -- list of nodes
a:isComplete()       -- check completenes
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
a = Num.solve(math.sin, 3, 4) -- find root in range
b = Num.trapez(math.sin, 0, 1) -- numerical integration
-- solve ode x' = x*y for x = {0,3} and y(0) = 1
tbl, yn = Num.ode45(function (x,y) return x*y end, {0,3}, 1)
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

### rational (Rat)

Computations with rational numbers.

```lua
a = Rat(1,2)         -- 1/2
b = a / Rat(4,8)
```

### special (Spec)

Special functions.

```lua
a = Spec.beta(3,4)   -- beta-function 
b = Spec.gamma(-1.5) -- gamma-funciton
c = Spec.besselj(3, 1.5) -- Bessel's function
```

### stat (Stat)

Statistical calculations.

```lua
a = Stat.mean({1,2,3,1,4,2}) 
b = Stat.max({2,6,1,3,2})
c = Stat.histcounts({2,5,1,2,6,3},3) -- find histrogram values
```

### units (Unit)

Units conversation.

```lua
Unit.add('h', Unit(60,'min'))  -- first rule
Unit.add('min', Unit(60,'s'))  -- second rule
a = Unit(1, 'm/s')   -- set variable
b = a['km/h']        -- get value in km/h
```
