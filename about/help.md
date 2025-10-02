# Sonata Lua Calculus

## Ap (asciiplot)
Use pseudography for data visualization.

**Ap (width_N=73, height_N=21) --> new_F** - Create new asciiplot.

**Ap:concat(F1, F2) --> str** - Horizontal concatenation of figures with the same height. Equal to F1..F2.

**F:addLine(x1_d, y1_d, x2_d, y2_d, char_s='*')** - Add line from (x1,y1) to (x2,y2).

**F:addPoint(x_d, y_d, char_s='*')** - Add point (x,y) using char.

**F:addPose(row_N, col_N, char_s='*')** - Add character to the given position.

**F:addString(row_N, col_N, str)** - Add string to the given position.

**F:axes() --> tbl** - Get {'size','log','range','view','fix'} for each axis.

**F:bar([tx,] ty) --> F** - Plot bar diargram for the given data.

**F:blocks([tx,] ty) --> F** - Plot data relations with blocks.

**F:contour(fn, {level=5, view='XY'}) --> F** - Find contours of projection for a function fn(x,y). Views: XY, XZ, YZ.

**F:copy() --> cpy_F** - Create a copy of the object.

**F:legend(str_t|flag_s)** - Update legend. Use off/on to hide or show the legend.

**F:redraw() --> F|nil** - Apply the last draw command. Equal to ~F.

**F:reset()** - Prepare a clean canvas.

**F:scale(factor_d | src_F)** - Change figure size w.r.t. initial size.

**F:setX(par_t)** - X axis configuration, set 'range' ({a,b}), 'view' ('min'/'mid'/'max'/false), 'log'-arithm (true/false), 'fix' range (true/false), 'size'.

**F:setY(par_t)** - Y axis configuration, set 'range' ({a,b}), 'view' ('min'/'mid'/'max'/false), 'log'-arithm (true/false), 'fix' range (true/false), 'size'.

**F:setZ(par_t)** - Z axis configuration, set 'range' ({a,b}), 'view' ('min'/'mid'/'max'/false), 'log'-arithm (true/false), 'fix' range (true/false), 'size'.

**F:title(str)** - Set new title.

**F:tplot(data_t, cols_t={x=1, polar=false, sym=nil}) --> F** - Plot the table data, choose columns if need.


## Int (bigint)
Operations with arbitrary long integers.

**B:F() --> B!** - Return factorial of non-negative integer B.

**B:FF() --> B!!** - Find double factorial.

**B:abs() --> abs_B** - Return module of arbitrary long number.

**B:digits(N=10) --> tbl** - Get digits in the new numeric base.

**B:factorize() --> prime_t** - Find prime multipliers.

**B:float() --> num** - Represent current big integer as number if it possible.

**B:isPrime(method_s=nil) --> bool** - Check if the number is prime. Set 'Fermat' method to use the small Fermat theorem.

**B:random() --> rand_B** - Generate pseudo-random value from 0 to B.

**B:sign() --> int** - Return +1/-1.

**B:subF() --> !B** - Find subfactorial of the number.

**Int (num|str|tbl) --> new_B** - Create number from integer, string or table.

**Int:C(n, k, isRepeat=false) --> combinations_B** - Number of combinations C(n,k) with or without repetition.

**Int:P(n, k, isRepeat=false) --> permutaions_B** - Find permutations with or without repetition.

**Int:gcd(...) --> B** - Find the greatest common divisor for the given integers.

**Int:lcm(...) --> B** - Find the least common multiple for the given integers.

**Int:ratF(num_B, denom_B) --> num!/denom!** - Find ratio of factorials.


## Z (complex)
Manipulations with complex numbers.

**C:abs() --> float** - Return module of complex number.

**C:acos() --> y_C** - Complex inverse cosine.

**C:acosh() --> y_C** - Complex inverse hyperbolic cosine.

**C:arg() --> float** - Return argument of complex number.

**C:asin() --> y_C** - Complex inverse sine.

**C:asinh() --> y_C** - Complex inverse hyperbolic sine.

**C:atan() --> y_C** - Complex inverse tangent.

**C:atanh() --> y_C** - Complex inverse hyperbolic tangent.

**C:conj() --> conj_C** - Return the complex conjugate. Equal to ~C.

**C:cos() --> y_C** - Return cosine of a complex number.

**C:cosh() --> y_C** - Return hyperbolic cosine of a real or complex number.

**C:exp() --> y_C** - Return exponent in for complex argument.

**C:im() --> var** - Get imaginary part.

**C:log() --> y_C** - Complex logarithm.

**C:re() --> var** - Get real part.

**C:sin() --> y_C** - Return sinus of a complex number.

**C:sinh() --> y_C** - Return hyperbolic sinus of a complex number.

**C:sqrt() --> y_C** - Return square root. Result can be real of complex.

**C:tan() --> y_C** - Return tangent of a complex number.

**C:tanh() --> y_C** - Return hyperbolic tangent of a complex number.

**Z (re=0, im=0) --> new_C** - Create new complex number.

**Z:E(phy) --> cos(phy)+i*sin(phy)** - Make complex number exp(i*phy).

**Z:i(x=1) --> new_C** - Return x*i.


## C (const)
Collection of constants.

**C.astro.au --> 1.5E11** - Astronomic unit.

**C.astro.k --> 0.017** - Gaussian gravitational constant.

**C.astro.ly --> 9.5E15** - One light year.

**C.astro.pc --> 3.1E16** - One parsec.

**C.math.e --> 2.72** - Base of the natural logarithm.

**C.math.gamma --> 0.577** - Euler-Mascheroni constant.

**C.math.phi --> 1.62** - Golden ratio.

**C.phi.Da --> 1.7E-27** - Unified atomic mass unit.

**C.phy.G --> 6.7E-11** - Gravitational constant.

**C.phy.NA --> 6E23** - Avogadro's number.

**C.phy.R --> 8.31** - Universal gas constant.

**C.phy.Rinf --> 1.1E7** - Rydberg constant.

**C.phy.Vm --> 2.2E-2** - Volume of one mole of ideal gas.

**C.phy.c --> 3E8** - Speed of light.

**C.phy.e --> 1.6E-19** - Electron charge.

**C.phy.eps0 --> 8.8E-12** - Permittivity of free space.

**C.phy.g --> 9.81** - Acceleration of free fall.

**C.phy.h --> 6.6E-34** - Planck's constant.

**C.phy.k --> 1.4E-23** - Boltzmann's constant.

**C.phy.mu0 --> 1.2E-6** - Permeability of free space.

**C.phy.sigma --> 5.6E-8** - Stefan-Boltzmann constant.

**C:add(name_s, value, units_s=nil)** - Temporary define constant.

**C:remove(name_s) --> bool** - Delete user-defined constant.


## D (data)
Data processing and statistics.

**D (data_t) --> new_L** - Create list wrapper.

**D:binsearch(sorted_t, value, [extract_fn]) --> index_i, value** - Find position of element in sorted list using binary search.

**D:col(src_t, col_N) --> ref_Col** - Make column reference.

**D:copy(t) --> copy_t** - Make deep copy of the table.

**D:corr(xs_t, ys_t) --> float** - Find correlation for two vectors.

**D:cov(data_t) --> cov_M** - Find covariance matrix for list of vectors.

**D:csvread(file_s, delim_s=',') --> tbl** - Read delimiter separated data as Lua table.

**D:csvwrite(data_t, file_s, delim_s=',')** - Save Lua table as delimiter separated data into file.

**D:filter(in_t, fn|str|tbl) --> out_t** - Get result of the table filtering. Condition is boolean function, string or table of weights.

**D:freq(data_t) --> tbl** - Return table with frequencies of elements.

**D:gen(in_t, fn|str, [cond_fn|cond_str=nil]) --> out_t** - Make new list using given transformation. Optional condition function of the form f(value,index).

**D:geomean(data_t, weigh_t=nil) --> num** - Geometrical mean.

**D:harmmean(data_t, weigh_t=nil) --> num** - Harmonic mean.

**D:histPlot(data_t, edges_t|N=10) --> fig** - Find and show histogram.

**D:histcounts(data_t, edges_t|N=10) --> sum_t, edges_t** - Calculate amount of bins. Edges can be either number or table.

**D:is(data_t, fn|str) --> weigh_t** - Find weights using condition (boolean function or string).

**D:isNot(data_t, fn|str) --> weigh_t** - Find inverted weights using condition (boolean function or string).

**D:max(data_t) --> var, ind_N** - Maximal element and its index.

**D:md(data_t, names_t=nil, row_fn=nil) --> str** - Markdown-like table representation. Rows can be processed using function row_fn(t)-->t.

**D:mean(data_t, wight_t=nil) --> num** - Calculate average value. Weights can be used.

**D:median(data_t) --> num** - Median of the list.

**D:min(data_t) --> var, ind_N** - Minimal element and its index.

**D:moment(data_t, order_N, weigth_t=nil) --> num** - Central moment of order N, weights can be defined.

**D:pack(obj) --> bin_s** - Pack object to binary string.

**D:range(begin_d, end_d, step_d=±1) --> new_R** - Generate range object.

**D:reduce(data, fn|str, initial=datadata_t[1]_t[1]) --> var** - Apply function to its previous result and next element.

**D:ref(data_t, begin_N=1, end_N=#src_t) --> new_R** - Return reference to the range of elements.

**D:reverse(data_t)** - Reverse table elements.

**D:row(src_t, row_N) --> ref_Row** - Make row reference.

**D:sort(data_t, fn|str)** - Sort elements of the list

**D:std(data_t, weight_t=nil) --> num** - Standard deviation. Weights can be used.

**D:sum(data_t) --> var** - Get sum of all elements.

**D:unpack(bin_s) --> obj** - Unpack object from binary string.

**D:zeros(n1, [n2,..]) --> tbl** - Make table with zeros.

**D:zip(fn|str, ...) --> tbl** - Sequentially apply function to list of tables.


## Ex (extremum)
Extremum search and optimization methods.

**Ex:annealing(task={energy=fn,update=fn,init=x0,T=energy(x0),alpha=0.9,loop=1}) -> x_M, energy_d** - Simulated annealing method.

**Ex:fit(model_fn, param_t, xs_t, ys_t) --> minParam_t, sqSum_d** - Fit data with nonlinear model y = fn(x,t), where t is a parameter dictionary.

**Ex:linprog(c_M, param={Au=nil,bu=nil,Ae=nil,be=nil,Al=nil,bl=nil}) -> x_M, min_d** - Solve LP problem c*x -> min with Au*x <= bu, Ae*x == be, Al*x >= bl.

**Ex:maximum(fn, p_M, param={method='Powel',dfun=nil}) -> x_M, min_d** - Find maximum of a multidimentional function. Parameters: method=Powel|simplex, dfun - gradient. p is matrix in case of simplex approach.

**Ex:maximum1D(fn, a_d, c_d, param={method='golden',b=millde,dfun=nil}) -> x_d, min_d** - Find maximum of a function with scalar argument. Parameters: method=Brent|nil, b - initial point, dfun - derivative (for Brent method).

**Ex:minimum(fn, p_M, param={method='Powel',dfun=nil}) -> x_M, min_d** - Find minimum of a multidimentional function. Parameters: method=Powel|simplex, dfun - gradient. p is matrix in case of simplex approach.

**Ex:minimum1D(fn, a_d, c_d, param={method='golden',b=middle,dfun=nil}) -> x_d, min_d** - Find minimum of a function with scalar argument. Parameters: method=Brent|nil, b - initial point, dfun - derivative (for Brent method).


## Fz (fuzzy)
Fuzzy logic elements.

**D:getName() --> str** - Get domain name.

**D:getRange() --> range_t** - Get domain range.

**D:setList() --> sets_t** - Get list of set names in domain.

**F:andf(F2) --> new_F** - Fuzzy set intersection. Equal to F & F2.

**F:asRule() --> str** - Return set description as a part of rule.

**F:defuzzify(range_t, method_s=centroid) --> value_d** - Defuzzification. Available methods are: centroid, bisector, lom, som, mom.

**F:notf() --> new_F** - Fuzzy set complement. Equal to ~F.

**F:orf(F2) --> new_F** - Fuzzy set union. Equal to F | F2.

**Fz (env_t=nil) --> F** - Create new fuzzy inference system.

**Fz:dsigmf(tilt1_d, inflect1_d, tilt2_d, inflect2_d) --> F** - Make new fuzzy set, member function is difference of two sigmoidal functions.

**Fz:gauss2mf(sigma1_d, mu1_d, sigma2_d, mu2_d) --> F** - Make new fuzzy set, member function combines two Gaussians.

**Fz:gaussmf(sigma_d, mu_d) --> F** - Make new fuzzy set with Gaussian member function.

**Fz:gbellmf(width_d, power_d, mean_d) --> F** - Make new fuzzy set with generalized bell-shaped member function.

**Fz:linsmf(left_d, right_d) --> F** - Make new fuzzy set with linear s-shaped saturation member function.

**Fz:linzmf(left_d, right_d) --> F** - Make new fuzzy set with linear z-shaped saturated member function.

**Fz:newmf(member_fn, name_s=nil) --> F** - Make new fuzzy set with user defined member function.

**Fz:pimf(lowLeft_d, upLeft_d, upRight_d, lowRight_d) --> F** - Make new fuzzy set with pi-shaped member function.

**Fz:psigmf(tilt1_d, inflect1_d, tilt2_d, inflect2_d) --> F** - Make new fuzzy set with product of two sigmoidal member functions.

**Fz:sigmf(tilt_d, inflection_d) --> F** - Make new fuzzy set with sigmoidal member function.

**Fz:smf(left_d, right_d) --> F** - Make new fuzzy set with s-shaped saturation member function.

**Fz:trapmf(lowLeft_d, upLeft_d, upRight_d, lowRight_d) --> F** - Make new fuzzy set with trapeze member function.

**Fz:trimf(left_d, up_d, right_d) --> F** - Make new fuzzy set with triangle member function.

**Fz:zmf(left_d, right_d) --> F** - Make new fuzzy set with z-shaped saturation member function.

**S:addDomain(range_t, name_s) --> D** - Add new domain to system, return reference.

**S:addRule(in_F, out_F, weight_d=1)** - Add new rule to system.

**S:apPlot(domain_s, set_s=nil) --> fig** - Visualize fuzzy set with asciiplot. Plot all the sets when name not defined.

**S:setEnv(params_t)** - Update system environment.


## Geo (geodesy)
Coordinate transformations and other geodetic tasks.

**E.blhInto[E2] --> fn** - Get function to transform geodetic coordinates from E to E2 system using the Molodensky method.

**E.xyzInto[E2] --> fn** - Get function to transform coordinates from E to E2 system.

**E:bl2utm(blh_t) --> utm_t** - Find UTM projection for the given coordinates.

**E:into(E2, lin, rot, m)** - Define transormation rules between ellipsoids.

**E:solveDir(blh_t, az1_d, dist_d) --> blh_t, az2_d** - Solve direct geodetic problem, find second point position and its orientation if the first point, azimuth and distance are given.

**E:solveInv(blh1_t, blh2_t) --> dist_d, az1_d, az2_d** - Solve inverse geodetic problem, find distance and azimuths for two points.

**E:toBLH(xyz_t) --> blh_t** - Transform Cartesian coordinates to Geodetic.

**E:toXYZ(blh_t) --> xyz_t** - Transform Geodetic coordinates to Cartesian.

**E:utm2bl(utm_t) --> blh_t** - Find Geodetic coordinates for the given UTM pose and zone.

**Geo (param_t=nil) --> E** - Produce ellipsoid with the given params {'a', 'f'}.

**Geo:deg2dms(deg_d) --> deg, min, sec** - Return degrees, minutes and seconds for the given angle value.

**Geo:dms2deg(deg_d, min_d=0, sec_d=0) --> deg** - Convert degrees, minutes and seconds to degrees.

**Geo:grav(latitude_d) --> acceleration** - International gravity formula, angle in degrees.

**Geo:hashDecode(hash_s) --> coord_t, range_t** - Find central point and range of the zone.

**Geo:hashEncode(coord_t, letter_N=6) --> hash_s** - Find hash for the given point.


## Gp (gnuplot)
Interface for calling Gnuplot from Sonata.

**G:add(curve_v)** - Add new curve to figure.

**G:copy() --> cpy_G** - Get copy of the plot options.

**G:show()** - Plot data, represented as Lua table.

**Gp () --> new_G** - Prepare Gnuplot object.

**Gp.keys** -   Options / examples:

{math.sin, title='sin'}       -- plot using function, define in Lua; add legend

{'sin.dat', ln=1, lw=2}       -- plot data from file, use given color and width

{tbl, with='lines'}           -- plot data from Lua table, use lines

title='Graph name'            -- set title

xrange={0,10}                 -- range of x from 0 to 10

yrange={-2,2}                 -- range of y

zrange={0,5}                  -- range of z

trange={1,2}                  -- range for parametric functions

xtitle='A', ytitle='B'        -- axes names

terminal='jpeg'               -- save result as jpeg image

output='my_plot.jpg'          -- file name

parametric=true               -- create parametric plot

size='square'                 -- set square size

polar=true                    -- use polar coordinate system

grid='polar'                  -- polar grid

legend=false                  -- don't use legend

surface=true                  -- plot surface in 3D

samples=200                   -- define number of points

raw='set pm3d'                -- set Gnuplot options manually



**Gp:plot(x1_t, [y1_t, nm_s, x2_t,..])** - 'x' is list of numbers, 'y' is either list or functin, 'nm' - curve name.

**Gp:polarplot(x1_t, y1_t, [nm_s, x2_t, y2_t,..])** - Make polar plot. 'x' is list of numbers, 'y' is either list or functin, 'nm' - curve name.

**Gp:surfplot(x1_t, y1_t, fn1, [nm_s, x2_t, y2_t,..])** - Make surfacе plot. 'x' and 'y' are lists of numbers, 'fn' is functin, 'nm' - surface name.

**Gp:tplot(var, [x_N, y1_N, y2_N,..])** - Plot table, matrix or data file. Optional elements define columns.

**Gp:tpolar(var, [x_N, y1_N, y2_N,..])** - Polar plot for table, matrix or data file. Optional elements define columns.

**Gp:tsurf(var, [x_N, y_N, z1_N, z2_N,..])** - Surface plot for table, matrix or data file. Optional elements define columns.


## Graph (graph)
Operations with graphs.

**G:add(n1, n2=nil, w_d=1)** - Add new node or edge.

**G:addEdges(list_t)** - Import edges and weights from list.

**G:addNodes(list_t)** - Import nodes from list.

**G:components() --> G_t** - Get list of connected components.

**G:copy() --> cpy_G** - Get copy of the graph.

**G:dot(fname_s=nil) --> str** - Save or return graph structure in dot notation.

**G:edge(pair_t) --> weight_d|nil** - Get weight of the edge.

**G:edges() --> edges_t** - Get list of edges.

**G:has(node) --> bool** - Check if the graph has the node.

**G:isComplete() --> bool** - Check completeness of the graph.

**G:isConnected() --> bool** - Check if the graph is connected.

**G:isDirected() --> bool** - Check if the graph is directed.

**G:isEuler() --> bool** - Check if the graph has Euler circle.

**G:isTree() --> bool** - Check if the graph is tree.

**G:isWeighted() --> bool** - Check if any edge has weight different from 1.

**G:matrix() --> adjacency_M, nodes_t** - Get adjacency matrix and node list.

**G:nin(node) --> nodes_t** - Find adjucent input nodes.

**G:nodes() --> node_t** - List of nodes.

**G:nout(node) --> nodes_t** - Find adjucent output nodes.

**G:rand(edge_N)** - Fill graph with random edges.

**G:randp(probability_d)** - Fill graph with random edges.

**G:remove(n1, n2=nil)** - Remove node or edge from the graph.

**G:search(node1, node2, method_s) --> path_t|nil** - Find path between two nodes. Methods are: bfs, dfs, dijkstra.

**G:size() --> nodes_N** - Get node number. Equal to #G.

**G:toSvg(name_s)** - Convert graph to SVG image using Graphviz.

**Graph (params_t={}) --> new_G** - Create graph. Parameters are {dir=bool, O|K|C|P=number|names_t, name='n'}.

**Graph:concat(G_t) --> new_G** - Combine graphs into one object.


## Lens (lens)
Matrix methods in paraxial optics.

**L:beam(inCurv_d, inSize_d, lambda_d) --> outCurv_d, outSize_d** - Find output beam curvature and spot radius.

**L:cardinal(nLft_d=1, nRht_d=1) --> points_t** - Find location of the cardinal points of the given system w.r.t input and output planes, use refractive indeces if need. Return table of distances.

**L:copy() --> cpy_L** - Create a copy of the object.

**L:emit(lambda_d) --> outCurv_d, outSize_d|nil, waist_d|nil, shift_d|nil ** - Find laser cavity output beam curvature. In the case of stable cavity also returns size radius, waist radius and its shift from the plane.

**L:inv() --> inv_L** - Get the inverted system matrix.

**L:matrix() --> M** - Get elements as matrix.

**L:transform(yIn_d, VIn_d) --> yOut_d, VOut_d** - Find the output ray position 'y' and optical angle 'V' (= v*n). Equal to L(y,V).

**Lens (A_d, B_d, C_d, D_d) --> new_L** - Make new lens component.

**Lens:M(rad_d, n_d=1) --> L** - Find reflection matrix for the given radius and refractive index.

**Lens:R(nin_d, rad_d, nout_d) --> L** - Find refraction matrix for the given radius of surface and input and output refractive indeces.

**Lens:T(dist_d, n_d=1) --> L** - Find translation matrix for the given distance and refractive index.

**Lens:afocal(magn_d) --> L** - Find matrix for the afocal system.

**Lens:gParam(waist_d, lambda_d) --> div_d, range_d** - Find divergence angle and Raileigh range for a Gaussian beam.

**Lens:gSize(waist_d, lambda_d, dist_d) --> curv_d, rad_d** - Find Gaussian beam radius and curvature at some distance.

**Lens:solve(fn, index_N, initial_d) --> found_d** - Find condition when component with the given index is equal to 0, use initial assumption.

**Lens:thin(focal_d) --> L** - Find the thin lens system matrix for the given focal distance.


## Main (main)
Lua based mathematics.

**Fn(expr_s) --> fn** - Generate function from expression 'args -> value'

**Map(fn, in_t) --> out_t** - Evaluate function for each table element.

**PI --> 3.14** - Number pi.

**Plot(...) --> str** - Plot arguments in form 't', 't1,t1', 'fn,nm', 'fn1,fn2' etc.

**Round(v, decimal_N=0) --> round_v** - Round value, define number of decimal digits or tolerance.

**abs(x) --> y** - Absolute value.

**acos(x) --> y** - Inverse cosine x.

**acosh(x) --> y** - Hyperbolic arc cosine.

**asin(x) --> y** - Inverse sine.

**asinh(x) --> y** - Hyperbolic inverse sine.

**atan(x) --> y** - Inverse tangent x.

**atan2(y_d, x_d) --> num** - Inverse tangent of y/x, use signs.

**atanh(x) --> y** - Hyperbolic inverse tangent.

**cos(x) --> y** - Cosine.

**cosh(x) --> y** - Hyperbolic cosine.

**eq(x, y) --> bool** - Check equality of two objects.

**exp(x) --> y** - Exponent.

**float(obj) --> x** - Convert object to float point number.

**help(fn='main') --> str** - Show information about the function.

**hypot(...)** - Hypotenuse.

**int(obj) --> x** - Convert object to integer number.

**log(x) --> y** - Natural logarithm.

**quit()** - Quit the program.

**sin(x) --> y** - Sine.

**sinh(x) --> y** - Hyperbolic sinus.

**sqrt(x) --> y** - Square root.

**tan(x) --> y** - Tangent.

**tanh(x) --> y** - Hyperbolic tangent.

**use([module_s]) --> str|nil** - Call use('module') or use{'module1','module2'} to load new functions.


## Mat (matrix)
Matrix operations. The matrices are spares by default. Indexation from 1.

**M:H() --> conj_Ref** - Return conjugabe transpose. 

**M:T() --> transpose_Ref** - Return matrix transpose.

**M:chol() --> lower_M|nil** - Cholesky decomposition of positive definite symmetric matrix.

**M:cols() --> N** - Get number of columns.

**M:copy() --> cpy_M** - Return copy of matrix.

**M:det() --> num** - Calculate determinant.

**M:diag() --> V** - Get diagonal of the matrix.

**M:eig() --> vectors_M, values_M** - Find matrices of eigenvectors and eigenvalues.

**M:exp() --> new_M** - Matrix exponential.

**M:inv() --> inv_M** - Return inverse matrix.

**M:kron(M2) --> M⊗M2** - Find Kronecker product.

**M:kronSum(M2) --> M⊕M2** - Find Kronecker sum.

**M:lu() --> L_M, U_M, perm_M** - LU decomposition for the matrix. Return L, U and P matrices.

**M:map(fn|str) --> found_M** - Apply the given function to all elements, return new matrix.

**M:minor(row_N, col_N) --> minor_M** - Find minor for the matrix element.

**M:norm() --> num** - Euclidean norm.

**M:pinv() --> inv_M** - Pseudo inverse matrix calculation.

**M:qr() --> Q_M, R_M** - QR decomposition of the matrix.

**M:rank() --> N** - Find rank of the matrix.

**M:reshape(row_N=(rows*cols), col_N=1) --> mat_Ref** - Matrix with rearranged elements.

**M:rows() --> N** - Get number of rows.

**M:rref() --> upd_M** - Perform transformations using Gauss method.

**M:svd() --> U_M, S_M, V_M** - Singular value decomposition, return U, S, V.

**M:table() --> tbl** - Convert to simple Lua table.

**M:tr() --> sum** - Get trace of the matrix.

**M:vec() --> vec_Ref|nil** - Create reference to vector data.

**M:vectorize() --> V** - Create vector as a stack of columns.

**Mat {row1_t, ...} --> new_M** - Create matrix from list of strings (tables).

**Mat:D(list_v, shift_N=0) --> M** - Create new matrix with the given diagonal elements.

**Mat:V {...} --> mat_Ref** - Create vector from list of numbers.

**Mat:eye(row_N, col_N=row_N) --> M** - Create identity matrix.

**Mat:fill(row_N, col_N, val=1) --> M** - Create matrix of given numbers (default is 1).

**Mat:hor(mat_t) --> mat_Ref** - Horizontal concatenation for the given list of matrices.

**Mat:ver(mat_t} --> mat_Ref** - Vertical concatenation for the given list of matrices.

**Mat:zeros(row_N, col_N=row_N) --> M** - Create matrix of zeros.

**Mat:zip(fn|str, ...) --> res_M** - Apply function to the given matrices element-wise.

**V:cross(V2) --> M** - Cross product of two 3-element vectors.

**V:dot(V2) --> num** - Scalar product of two vectors.

**V:normalize()** - Normalize to unit vector.

**V:outer(V2) --> M** - Outer product or two vectors.

**V:skew() --> M** - Make skew-symmetric matrix from the 3-element vector.


## Num (numeric)
Group of functions for numerical calculations.

**Num:der(fn, x_d) --> num** - Calculate the derivative value for the given function.

**Num:int(fn, x1_d, x2_d) --> num** - Get integral of the function. Improper integrals with infinite limits are possible.

**Num:lim(fn, xn_d, isPositive=false) --> y** - Estimate limit of a function.

**Num:newton(fn, x0_d) --> num** - Find root of equation using Newton's rule.

**Num:ode(fn, interval_t, y0, {dt=del/20,exit=nil}) --> ys_t** - Numerical approximation of the ODE solution.

List of parameters is optional and can includes time step and exit condition.

Return table of intermediate points in form {t, x(t)}.

**Num:solve(fn, low_d, up_d) --> num** - Find root of equation fn(x)=0 on interval [a,b].


## Poly (polynomial)
Operations with polynomials.

**P:copy() --> cpy_P** - Get copy of the polynomial.

**P:der() --> der_P** - Calculate derivative of polynomial.

**P:int(x0_d=0) --> int_P** - Calculate integral, define free coefficient if need.

**P:roots() --> roots_t** - Find all the polynomial roots.

**P:str(char_s='x') --> str** - Pretty print for polynomial.

**P:val(x) --> y** - Get value of polynomial P in point x. Equat to P(x).

**Poly {.., v1, v0} --> new_P** - Create a polynomial.

**Poly:R(roots_t) --> P** - Return polynomial with given roots.

**Poly:char(M) --> P** - Return characteristic polinomial for the given matrix.

**Poly:fit(xs_t, ys_t, order_N) --> P** - Find polynomial approximation for the line.

**Poly:lagrange(xs_t, ys_t) --> P** - Find interpolation polynomial in the Lagrange form.

**Poly:lin(xs_t, ys_t, before_d=nil, after_d=before_d) --> Ps_t** - Linear data interpolation. Return table with polynomials.

**Poly:spline(xs_t, ys_t) --> Ps_t** - Cubic spline data interpolation. Return table with polynomials.

**Poly:taylor(x_d, fx_d, [fx'_d, fx''_d,..]) --> P** - Get Taylor series.

**Poly:x() --> P** - Get object to define polynomial as a sum of k*x^n


## Quat (quaternion)
Operations with quaternions.

**Q:abs() --> num** - Value of the norm.

**Q:conj() --> conj_Q** - Get conjugation. Equal to ~Q.

**Q:exp() --> exp_Q** - Quaternion exponential.

**Q:inv() --> inv_Q** - Find inverted quaternion.

**Q:log() --> log_Q** - Quaternion logarithm.

**Q:normalized() --> unit_Q** - Return unit quaternion.

**Q:rotate(in_t|V) --> out_t** - Apply quaternion to rotate the vector.

**Q:toAA() --> angle_d, axis_t|nil** - Get angle and axis of rotation.

**Q:toRPY() --> roll_d, pitch_d, yaw_d** - Get Euler angles.

**Q:toRot() --> M** - Get equal rotation matrix.

**Q:w() --> var** - Get w component.

**Q:x() --> var** - Get x component.

**Q:y() --> var** - Get y component.

**Q:z() --> var** - Get z component.

**Quat {x, y, z, w} --> new_Q** - Create new quaternion.

**Quat:fromAA(angle_d, axis_t|V) --> Q** - Create quaternion using angle and axis.

**Quat:fromRPY(roll_d, pitch_d, yaw_d) --> Q** - Convert Euler angles to quaternion.

**Quat:fromRot(M) --> Q** - Convert rotation matrix to quaternion.

**Quat:slerp(beg_Q, end_Q, rat_f) --> rat_Q** - Spherical linear interpolation for the given ratio.


## Qb (qubit)
Quantum computing simulation

**G:CNOT(slave_i, master_i) --> upd_G** - Add CNOT gate.

**G:H([ind1, ind2 ...]) --> upd_G** - Add Hadamard gate.

**G:P(phase, [ind1, ind2 ...] --> G** - Add phase shift gate.

**G:R(axis_s, angle, [ind1, ind2 ...] --> G** - Add rotation for axis 'X', 'Y' or 'Z'.

**G:S([ind1, ind2 ...]) --> upd_G** - Add S gate.

**G:SWAP(ind1, ind2) --> upd_G** - Add gate to swap 2 qubits.

**G:T([ind1, ind2 ...]) --> upd_G** - Add T gate.

**G:X([ind1, ind2 ...]) --> upd_G** - Add X gate.

**G:Y([ind1, ind2 ...]) --> upd_G** - Add Y gate.

**G:Z([ind1, ind2 ...]) --> upd_G** - Add Z gate.

**G:fromMatrix(M) --> upd_G** - Make gate from matrix.

**G:fromTable(truth_t) --> upd_G** - Make gate from truth table.

**G:inverse() --> inv_G** - Get inverted gate sequence

**G:isUnitary() --> bool** - Check if the matrix is unitary

**Q:copy() --> cpy_Q** - Create a copy of the object.

**Q:matrix() --> M** - Get matrix representation.

**Q:meas(index=nil) --> Q** - Qubit state measurement.

**Q:normalize()** - Make norm equal to 1.

**Q:prob(state_s) --> probatility_d** - Get probability for the given state.

**Qb (state_s|num) --> Q** - Create new qubit.

**Qb:combine([Q1, Q2, ...]) --> Q|nil** - Make a system of qubits. Same as Q1..Q2 for two components.

**Qb:fromVector(V) --> Q** - Initialize qubit state from vector.

**Qb:gates(input_n) --> G** - Initialize gates for the given numer of inputs.


## Rand (random)
Random number generators.

**R:array(n1, [n2,..]) --> tbl** - Get multidimentional random array.

**R:binomial(p_d, N) --> int** - Binomial distributed random values.

**R:bytes(N) --> str** - Get sequence of random bytes.

**R:cauchy(mu_d=0, sigma_d=1) --> float** - Cauchy distributed random numbers.

**R:choice(tbl) --> element, index_N** - Get random table element.

**R:exp(lambda_d=1) --> float** - Exponential distributed random values.

**R:flip(p=0.5) --> bool** - Uniform distributed binary value.

**R:gamma(alpha_N, beta_d=1) --> float** - Gamma distributed random values.

**R:int(lower_i=1, upper_i) -> int** - Uniform distributed random integer in the given range.

**R:ipairs(tbl) --> iterator_fn** - Random iterator over the table elements.

**R:logistic(mu_d=0, sigma_d=1) --> float** - Logistic distributed random value.

**R:norm(mean_d=0, dev_d=1) --> float** - Normal distributed random value with the given mean and deviation.

**R:poisson(lambda_d) --> int** - Poisson distributed random values.

**R:rayleigh(sigma_d) --> float** - Rayleigh distributed random values.

**R:seed(N=os.time) --> R** - Set random generator seed.

**R:shuffle(tbl)** - Change order of elements in place.

**Rand () --> float** - Uniform distributed random number between 0 and 1.

**Rand:new() --> R** - Create random generator object.


## Rat (rational)
Computations with rational numbers.

**R:denom() --> var** - Return the denominator of the rational number.

**R:float() --> num** - Return rational number as decimal.

**R:num() --> var** - Return the numerator of rational number.

**R:toCF() --> coeff_t** - Transform rational number to continued fraction.

**Rat (num, denom=1) --> new_R** - Create rational number using num (and denom).

**Rat:from(src_f, err_f=1E-3) --> R** - Estimate ratio from floating point value.

**Rat:fromCF(coeff_t) --> R** - Transform continued fraction to rational number.


## Spec (special)
Special mathematical functions.

**Spec:besseli(order_N, x_d) --> num** - Modified Bessel function In(x).

**Spec:besselj(order_N, x_d) --> num** - Bessel function of the first kind.

**Spec:besselk(order_N, x_d) --> num** - Modified Bessel function Kn(x).

**Spec:bessely(order_N, x_d) --> num** - Bessel function of the second kind.

**Spec:beta(z_d, w_d) --> num** - Beta function.

**Spec:betainc(x_d, a_d, b_d) --> num** - Incomplete beta function Ix(a,b).

**Spec:betaln(z_d, w_d) --> num** - Natural logarithm of beta function.

**Spec:dawson(x_d) --> num** - Dawson integral.

**Spec:erf(x_d) --> num** - Error function.

**Spec:erfc(x_d) --> num** - Complementary error function.

**Spec:expint(pow_N, x_d) --> num** - Exponential integral En(x).

**Spec:gamma(x_d) --> num** - Gamma function.

**Spec:gammaln(x_d) --> num** - Natural logarithm of gamma function.

**Spec:gammp(order_N, x_d) --> num** - Incomplete gamma function P(N,x).

**Spec:gammq(order_N, x_d) --> num** - Incomplete gamma function Q(N,x) = 1-P(N,x).


## Sym (symbolic)
Symbolic calculations.

**S:diff(var_S) --> derivative_S** - Find symbolic derivative.

**S:eval(env_t={}) --> upd_S|num** - Evaluate symbolic expression with the given environment.

**S:expand() --> expanded_S** - Expand product of polynomials when possible.

**S:isFn() --> bool** - Return true if the symbol is function.

**S:ratDenom() --> denominator_S** - Get denominator of the expression.

**S:ratNum() --> numerator_S** - Get numerator of the expression.

**S:struct() --> str** - Show internal structure.

**S:val() --> num** - Get constant value.

**Sym (num|str) --> new_S** - Create new symbolic variable.

**Sym:def(name_s, args_t, expr_S) --> fn_S** - Define symbolical function. S is either symbolic expression or a Lua function.


## U (units)
Operations and conversations according the units.

**U (val=1, name_s) --> new_U** - Create new elements with units.

**U.prefix** - Table of possible prefixes for units.

**U.rules** - Table of rules for conversation.

**U:convert(new_s) --> upd_U|nil** - Convert one units to another, return new object or nil.

**U:copy() --> cpy_U** - Create copy of the element.

**U:u() --> str** - Get units.

