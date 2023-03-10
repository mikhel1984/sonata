---------- locale/eo.lua ----------

return {
----------
language = 'Esperanto',
authors  = [[Stanislav Mikhel]],
---------- dialog nil
Dialog = {
["intro"]                  = [[-------- help([functio]) = akiri helpon -----------
---------- use([modulo]) = piliigi la funcionalidad
----------------- quit() = finu -------------------
]],
["done"]                   = [[Preta.]],
},
---------- array.lua ----------
array = {
["A:set(ind_t, var) --> nil"] = [[Aldonu valoro al la tabelo. Indico estas tablo.]],
["A:dim() --> int"]        = [[Redonas dimension de la tabelo.]],
["A:map(fn) --> out_A"]    = [[Aplikas funkcion al la tabelo. Redonas novan tabelon.]],
["A:concat(A2, axis_N) --> A3"] = [[Kombini tabeloj laǔ donita akso.]],
["__module__"]             = [[Manipuladoj kun la tabelo de elementoj. Indeksado komencoj de unu. Indico estas tablo.]],
["A:ipairs() --> iter_fn"] = [[Revenas funkcion por preterpasi ĉiujn indicojn kaj valorojn.]],
[":zip(fn, ...) --> A"]    = [[Aplikas funkcion al la valoroj de du tabeloj. Redonas novan tabelon.]],
["A:sub(ind1_t, ind2_t) --> range_A"] = [[Redonas la parton de tabelo limigita de 2 indicoj.]],
["A:get(ind_t) --> var"]   = [[Redonai elementon de tabelo.]],
["A:isEqual(A2) --> bool"] = [[Kontroli egalecon de dimensioj.]],
["A:capacity() --> int"]   = [[Maksimuma nombro da elementoj en la tabelo. La sama kiel #A.]],
["A:copy() --> cpy_A"]     = [[Kreu kopion de la tabelo.]],
[" {size1_N, [size2_N, ..]} --> new_A"] = [[Krei malplenan tabelon. Dimensio estas tablo.]],
--["comparison"]             = [[a == b, a ~= b]],
},
---------- asciiplot.lua ----------
asciiplot = {
["__module__"]             = [[Vidigo de datumoj kun pseŭdografioj.]],
["F:addString(row_N, col_N, str) --> nil"] = [[Aldonu vorton ĉe donita pozicio.]],
["F:bar(t, vy=2, x_N=1) --> nil"] = [[Bardiagramo.Plot bar diargram for data. vy estas aŭ indekso en t aŭ tabelo de y-j.]],
["F:reset() --> nil"]      = [[Preparu malplenan kanvason.]],
["F:addPoint(x_d, y_d, char_s) --> nil"] = [[Aldonu punkton (x,y) per simbolo.]],
["F:plot(...) --> nil"]    = [[Montru argumentojn 't', 't1,t1', 'fn,nm', 'fn1,fn2' ktp.]],
["F:scale(factor_d, isDefault=false) --> F"] = [[Skali la diagramon rilate al la komenca grandeco.]],
["F:contour(fn, {view='XY'}) --> nil|str"] = [[Trovu konturojn de projekcio por funkcio fn(x,y). Vidoj: XY, XZ, YZ, XYZ.]],
["F:copy() --> cpy_F"]     = [[Kreu kopion de objekto.]],
["F:tplot(data_t, {yfix=false}) --> nil"] = [[Vidigo de tabelaj datumoj, vi povas specifi la kolumnombrojn.]],
["F:addPose(row_N, col_N, char_s) --> nil"] = [[Aldonu signon ĉe la donita pozicio.]],
[":concat(...) --> str"]   = [[Horizontala kuniĝo de grafikaĵoj de egala alteco.]],
--["F:setY(range_t) --> nil"] = [[Update Y range.]],
--["F:showY(pos_s|nil) --> nil"] = [[Define Y axis position.]],
--["F:axes() --> tbl"]       = [[Get {size, log, range, pose} for each size.]],
--["F:setX(range_t) --> nil"] = [[Update X range.]],
--["F:showX(pos_s|nil) --> nil"] = [[Define X axis position.]],
--["F:title(str) --> nil"]   = [[Set new title.]],
--["F:logY(isLog) --> nil"]  = [[Change Y axis type to logarithmic.]],
[" (width_N=73, height_N=21) --> new_F"] = [[Kreu novan diagramon.]],
--["F:showZ(pos_s|nil) --> nil"] = [[Define Z axis position.]],
--["F:logX(isLog) --> nil"]  = [[Change X axis type to logarithmic.]],
--["F:setZ(range_t) --> nil"] = [[Update Z range.]],
--["Plot(...) --> nil"]      = [[Plot arguments in form 't', 't1,t1', 'fn,nm', 'fn1,fn2' etc.]],
--["F:logZ(isLog) --> nil"]  = [[Change Z axis type to logarithmic.]],
--["F:resize(src_F | (width_N, height_N)) --> nil"] = [[Update size of canvas.]],
},
---------- bigint.lua ----------
bigint = {
[" (var) --> new_B"]       = [[Krei novan arbitra longa entjero bazitan sur la nombro, teksto aǔ tablo.]],
["B:abs() --> num"]        = [[Absoluta valoro.]],
["B:float() --> num"]      = [[Prezenti grandan entjeron en nombra formo se eble.]],
["B:base() --> int"]       = [[Radikso por nombro.]],
["__module__"]             = [[Operacioj kun la arbitraj longaj entjeroj.]],
["B:factorize() --> primeBs_t"] = [[Listo de primaj faktoroj.]],
["B:at(N) --> int"]        = [[Akiru N-an ciferon.]],
["B:rebase(N) --> upd_B"]  = [[Konvertu numbrom al nova numbrosistemo.]],
[":random(B) --> rand_B"]  = [[Hazarda nombro de 0 al B.]],
["B:fact() --> B!"]        = [[Revenas faktorialon de la ne indika entjera nombro n.]],
["B:isPrime([method_s]) --> bool"] = [[Kontrolante numeron por simpleco. Metita metodo 'Fermat' por usi la etan teoremon de Fermat.]],
["B:gcd(B2) --> B3"]       = [[Plej granda komuna faktoro.]],
["B:eq(x) --> bool"]       = [[Egaleco kontrolo.]],
--["comparison"]             = [[a<b, a<=b, a>b, a>=b, a==b, a~=b]],
--["arithmetic"]             = [[a+b, a-b, a*b, a/b, a%b, a^b, -a, #a]],
},
---------- complex.lua ----------
complex = {
["Z:asinh() --> y_Z"]      = [[Hyperbola sinusarko de kompleksa nombro.]],
["Z:im() --> var"]         = [[Kompleksa parto.]],
[":i(x=1) --> new_Z"]      = [[Redonas v*i.]],
["Z:atanh() --> y_Z"]      = [[Hyperbola tangentarko de kompleksa nombro.]],
["__module__"]             = [[Operacioj kun la kompleksaj nombroj.]],
["Z:abs() --> float"]      = [[Revenas la modulo de kompleksa nombro.]],
["Z:acos() --> y_Z"]       = [[Kompleksa kosinusarko.]],
["Z:exp() --> y_Z"]        = [[Kompleksa eksponento.]],
["Z:sinh() --> y_Z"]       = [[Hyperbola sinuso de kompleksa nombro.]],
[" (re=0, im=0) --> new_Z"] = [[Krei novan kompleksan nombron.]],
["Z:acosh() --> y_Z"]      = [[Hyperbola kosinusarko de kompleksa nombro.]],
[":trig(module, angle) --> new_Z"] = [[Uzi geometria prezento por kompleksa nombro.]],
["Z:tanh() --> y_Z"]       = [[Hyperbola tangento de kompleksa nombro.]],
["Z:arg() --> float"]      = [[Revenas la argumenton de kompleksa nombro.]],
["Z:sin() --> y_Z"]        = [[Kompleksa sinuso.]],
["Z:cosh() --> y_Z"]       = [[Hyperbola kosinuso de kompleksa nombro.]],
["Z:tan() --> y_Z"]        = [[Tangento de kompleksa nombro.]],
["Z:conj() --> conj_Z"]    = [[Kompleksa konjugata nombro. Egale al ~C.]],
["Z:sqrt() --> y_Z"]       = [[Kvadrata radiko. Rezulto povas esti vera aǔ imaginara nombro.]],
["Z:asin() --> y_Z"]       = [[Sinusarko de kompleksa nombro.]],
["Z:round(N=6) --> rounded_Z"] = [[Rondu nombron, difini kvanto de dekumaj ciferoj.]],
["Z:re() --> var"]         = [[Reala parto.]],
["Z:log() --> y_Z"]        = [[Kompleksa logaritmo.]],
["Z:atan() --> y_Z"]       = [[Tangentarko de complexa nombro.]],
["Z:cos() --> y_Z"]        = [[Kosinuso de kompleksa nombro.]],
--["arithmetic"]             = [[a+b, a-b, a*b, a/b, a^b, -a]],
--["comparison"]             = [[a==b, a~=b]],
},
---------- const.lua ----------
const = {
[".phi.Da"]                = [[Atommasunuo.]],
[".astro.pc"]              = [[Unu parsek.]],
[".phy.e"]                 = [[Electrono ŝarĝo.]],
[".phy.sigma"]             = [[Konstanto de Stefan-Boltzmann.]],
[".phy.NA"]                = [[Nombro de Avogadro.]],
[".math.phi"]              = [[Ora proporcio.]],
["__module__"]             = [[Iuj konstantaj valoroj.]],
[":add(name_s, value, [units_s]) --> nil"] = [[Krei tempan konstanton.]],
[".phy.c"]                 = [[Rapido de lumo.]],
[".phy.Rinf"]              = [[Konstanto de Rydberg.]],
[".phy.Vm"]                = [[Unu mole de ideala gaso volumo.]],
[".phy.g"]                 = [[Senpaga akcelo.]],
[":remove(name_s) --> bool"] = [[Forigi konstantan.]],
[".phy.G"]                 = [[Gravita konstanto.]],
[".phy.k"]                 = [[Konstanto de Boltzmann.]],
[".math.e"]                = [[Bazo de la natura logaritmo.]],
[".math.pi"]               = [[Ratio de cirkvarmo de rondo al ĝia diametro.]],
[".phy.h"]                 = [[Konstanto de Planck.]],
[".astro.au"]              = [[Aastronomia unuo.]],
[".phy.R"]                 = [[Universaa gaso konstanto.]],
[".phy.mu0"]               = [[Magneta konstanto.]],
[".phy.eps0"]              = [[Elektra konstanto.]],
[".astro.ly"]              = [[Unu uma jaro.]],
},
---------- data.lua ----------
data = {
[":freq(data_t) --> tbl"]  = [[Revenas tablon kun la oftecoj de elementoj.]],
[":histcounts(data_t, rng_v=10) --> sum_t, edges_t"] = [[Distribuado de nombroj per intertempoj. La gamo povas esti aro kun nombro aŭ listo de limoj.]],
[":xIn(num1, num2) --> cond_fn"] = [[Revenas funkcio por kondiĉo d1 <= x <= d2.]],
["__module__"]             = [[Prilaborado de datumoj kaj statistiko.]],
[":max(data_t) --> var, ind_N"] = [[La plej granda nombro kaj ĝia indekso.]],
[":cov(data_t) --> cov_M"] = [[Trovu kunvarianca matrico.]],
[":zip(fn,...) --> tbl"]   = [[Sinsekve apliki funkcion al la listo de vektoroj.]],
[":xEq(num) --> cond_fn"]  = [[Revenas funkcio por kondiĉo x == d.]],
[":filter(in_t, condition) --> out_t"] = [[Akiru rezulton de la tabelfiltrado. Kondiĉo estas aŭ bulea funkcio aŭ tabelo de pezoj.]],
[":harmmean(data_t, [weigh_t]) --> num"] = [[Harmona meznombro.]],
[":ref(src_t, begin_N=1, end_N=#src_t) --> new_R"] = [[Akiru referencon al la gamo de elementoj.]],
[":csvread(file_s, delim_s=',', isCol=false) --> tbl"] = [[Legas datumojn disigitaj per delimitilo en la dosiero, revenas Lua tabulon.]],
[":isNot(data_t, cond_fn) --> yesno_t"] = [[Trovu inversan pezon uzante bulean funkcion.]],
[":median(data_t) --> num"] = [[Mezo de la listo.]],
[":xGt(num) --> cond_fn"]  = [[Revenas funkcio por kondiĉo x > d.]],
[":xLt(num) --> cond_fn"]  = [[Revenas funkcio por kondiĉo x < d.]],
[":tpdf(x_d, deg_N) --> num"] = [[Dissendo denseco de la Studento.]],
[":Fn(expr_s, arg_N=2) --> fn"] = [[Generu funkcion el esprimo de x1, x2 ktp.]],
[":std(data_t, [weight_t]) --> dev_f, var_f"] = [[Norma devio kaj vario. Pezoj povas esti prenita en konto.]],
[":geomean(data_t, [weigh_t]) --> num"] = [[La geometria meznombro.]],
[":csvwrite(file_s, data_t, char=',', isCol=false) --> nil"] = [[Savi Lua tabulon kiel dosieron kun delimitilo disigita datumojn.]],
[":sum(data_t) --> var"]   = [[Kalkuli sumon de ĉiuj elementoj.]],
[":is(data_t, cond_fn) --> yesno_t"] = [[Trovu pezon uzante bulean funkcion.]],
[":mean(data_t, [wight_t]) --> num"] = [[Kalkuli averaĝo. Pezoj povas esti prenita en konto.]],
[":moment(order_N, data_t, [weigth_t]) --> num"] = [[Momento de t kun N ordo, tw estas listo de pezoj.]],
[":min(data_t) --> var, ind_N"] = [[La plej malgranda nombro kaj ĝia indekso.]],
[":tcdf(x_d, deg_N) --> num"] = [[Akumula distribuo-funkcio de la Studento.]],
[":cov2(xs_t, ys_t) --> float"] = [[Trovu kunvariancon por du vektoroj.]],
},
---------- geodesy.lua ----------
geodesy = {
[":toENU(blRef_t, xyzRef_t, xyzObs_t) --> top_t"] = [[Akiru topocentrajn koordinatojn de punkto en referenca kadro.]],
["E:toBLH(xyz_t) --> blh_t"] = [[Konvertas kartezajn koordinatojn al geodeziaj koordinatoj.]],
[":deg2dms(deg_d) --> num"] = [[Revenas gradoj, minutoj kaj sekundoj por donita angulo.]],
[":hashEncode(coord_t, letter_N=6) --> hash_s"] = [[Trovu haŝvaloro por la donita punkto.]],
[":fromENU(blRef_t, xyzRef_t, top_t) --> xyzObs_t"] = [[Akiru kartezajn koordinatojn de loka punkto en referenca kadro.]],
["__module__"]             = [[Kunordigi transformoj kaj aliaj geodeziaj taskoj.]],
[":hashDecode(hash_s) --> coord_t, range_t"] = [[Trovu centran punkton kaj gamon de la zono.]],
[":dms2rad(deg_d, min_d=0, sec_d=0) --> num"] = [[Konvertas gradojn, minutojn kaj sekundojn al radianoj.]],
["E:solveInv(blh1_t, blh2_t) --> dist_d, az1_d, az2_d"] = [[Solvu inversan geodezian problemon, trovu distancon kaj azimutojn por du punktoj.]],
["E:solveDir(blh_t, az1_d, dist_d) --> blh_t, az2_d"] = [[Solvu rektan geodezian problemon, trovu duan punktan pozicion kaj ĝian orientiĝon se la unua punkto, azimuto kaj distanco estas donitaj.]],
[":grav(latitude_d) --> num"] = [[Internacia gravita formulo, angulo en gradoj.]],
["E:toXYZ(blh_t) --> xyz_t"] = [[Konvertado de geodeziaj koordinatoj al karteziaj koordinatoj.]],
["E.blhInto[E2] --> fn"]   = [[Revenas funkcion por konverti geodeziajn koordinatojn de sistemo A al B per la Molodensky metodo.]],
["E.xyzInto[E2] --> fn"]   = [[Revenas funkcion por konverti koordinatojn de sistemo A al B.]],
--["E:ll2utm(blh_t) --> utm_t"] = [[Find UTM projection for the given coordinates.]],
--["E:utm2ll(utm_t) --> blh_t"] = [[Find Geodetic coordinates for the given UTM pose and zone]],
},
---------- gnuplot.lua ----------
gnuplot = {
[":surfplot(x1_t, y1_t, fn1, [nm_s, x2_t, y2_t,..]) --> nil"] = [[Grafikaĵo surfacan funkcion. 'x' kaj 'y' estas listoj de nombroj, 'fn' estas funkcio de du argumentoj, 'nm' estas la nomo.]],
["G:show() --> nil"]       = [[Konstrui funkcian grafeon. Parametroj estas specifita en Lua table G.]],
["G:copy() --> cpy_G"]     = [[Krei kopion la parametroj de grafeo.]],
["G:add(curve_v) --> nil"] = [[Aldonu novan funkcion al la grafiko.]],
[":polarplot(x1_t, y1_t, [nm_s, x2_t, y2_t,..]) --> nil"] = [[Grafikaĵo en polusaj koordinatoj. 'x' - listo de nombroj, 'y' - listo aŭ funkcio, 'nm' - kurba nomo.]],
[":tpolar(var, [x_N, y1_N, y2_N,..]) --> nil"] = [[Polusa intrigo por tabelo, matrico aŭ dosiero. Laŭvolaj elementoj difinas kolumnojn.]],
[":tplot(var, [x_N, y1_N, y2_N,..]) --> nil"] = [[Grafikaĵo surbaze de tabelo, matrico aŭ dosiero. Laŭvolaj elementoj difinas kolumnojn.]],
[" () --> new_G"]          = [[Krei tablon por 'Gnuplot'.]],
[":plot(x1_t, [y1_t, nm_s, x2_t,..]) --> nil"] = [['x' - listo de nombroj, 'y' - listo aŭ funkcio, 'nm' - kurba nomo.]],
[":tsurf(var, [x_N, y_N, z1_N, z2_N,..]) --> nil"] = [[Grafikaĵo surfacan funkcion surbaze de tablo, matrico aŭ dosiero. Laŭvolaj elementoj difinas kolumnojn.]],
["__module__"]             = [[Interfaco por laborado kun Gnuplot de Lua.]],
[".keys"]                  = [[  Priskribo de parametroj:
{math.sin, title='sin'}                      -- grafeon por Lua funkcio, aldoni priskribon
{'sin.dat', ln=1, lw=2}                      -- grafeon por datumndosieron, specifi koloron kaj dikecon
{tbl, with='lines'}                          -- grafeon por Lua tabulo, uzi linioj
title='Graph name'                           -- apudskribo
xrange={0,10}                                -- gamo x de 0 ĝis 10
yrange={-2,2}                                -- gamo y
zrange={0,5}                                 -- gamo z
trange={1,2}                                 -- gamo por parametria funkcio
xtitle='A', ytitle='B'                       -- aksaj nomoj
terminal='jpeg'                              -- savi la rezulton kiel jpeg bildon
output='my_plot.jpg'                         -- dosiernomo
parametric=true                              -- parametria grafeo
size='square'                                -- kvadrata bildo
polar=true                                   -- uzi polusa grafeo
grid='polar'                                 -- polusa koordinata sistemo
legend=false                                 -- malebligi priskribon
surface=true                                 -- tridimensia grafeo
samples=200                                  -- nombro da punktoj
raw='set pm3d'                               -- fiksi la Gnuplot parametrojn permane 
]],
},
---------- graph.lua ----------
graph = {
["G:isWeighted() --> bool"] = [[Kontrolu se la randoj havas pezon malsama al 1.]],
["G:isComplete() --> bool"] = [[Kontroli plenecon de la grafeo.]],
[" {v1, v2,..} --> new_G"] = [[Krei novan grafeon.]],
["G:pathD(startNode, [goalNode]) --> dist_d, path_t|prev_t"] = [[Trovi la plej mallongan vojon uzanta Dijkstra algoritmon.]],
["__module__"]             = [[Operacioj kun grafeoj. Nodo estas sola nomo. Rando estas tabulo de du nomoj.]],
["G:edges() --> edges_t"]  = [[Listo de grafeaj randoj.]],
["G:remove(var) --> nil"]  = [[Forigi nodo aŭ rando de la grafeo.]],
["G:isNegative() --> bool"] = [[Kontrolu se la randoj havas negativaj pezoj.]],
["G:dfs(startNote, goalNode) --> isFound, path_t"] = [[Profundo unua serĉo. Reveni rezulto kaj trovita vojon.]],
["G:copy() --> cpy_G"]     = [[Akiri copion de la grafeo.]],
["G:nodes() --> node_t"]   = [[Listo de nodoj.]],
["G:add(var) --> nil"]     = [[Aldoni novan nodo aŭ rando.]],
["G:pathBF(startNode, [goalNode]) --> dist_d, path_t|prev_t"] = [[Trovi la plej mallongan vojon uzanta Bellman-Ford algoritmon.]],
["G:bfs(startNode, goalNode) --> isFound, path_t"] = [[Larĝa unua serĉo. Reveni rezulto kaj trovita vojon.]],
["G:isDirected() --> bool"] = [[Kontroli se la grafeo havas direktajn randojn.]],
},
---------- lens.lua ----------
lens = {
[":gaussSize(waist_d, lambda_d, dist_d) --> rad_d, curv_d"] = [[Trovu gaŭsan radioradiuson kaj kurbecon je iu distanco.]],
["L:beam(inRad_d, inCurv_d, lambda_d) --> outRad_d, outCurv_d"] = [[Trovu la elirradiason kaj kurbecon.]],
["__module__"]             = [[Matricaj metodoj en paraksiala optiko.]],
[":ref(rad_d, n1_d, n2_d) --> L"] = [[Liveras la matricon por refrakto.]],
[":trans(dist_d, n_d) --> L"] = [[Liveras la matricon por movi la trabon.]],
[":gaussParam(waist_d, lambda_d) --> div_d, range_d"] = [[Trovu diverĝan angulon kaj Rayleigh-intervalon por Gaŭsa radio..]],
["L:isUnit() --> bool"]    = [[Kontrolante ĉu matrico estas identeco.]],
["L:copy() --> cpy_L"]     = [[Kreu kopion de objekto.]],
[" {A_d, B_d, C_d, D_d} --> new_L"] = [[Kreu komponanton kun la donitaj parametroj.]],
[":mirror(rad_d, n_d) --> L"] = [[Liveras la matricon por la spegulo.]],
["L:det() --> determinant_d"] = [[Matrica determinanto.]],
[":afocal(magn_d) --> L"]  = [[Liveras la matricon por la afoka sistemo.]],
["L:cardinal(nLft_d=1, nRht_d=1) --> points_t"] = [[Determini la pozicion de la kardinalaj punktoj de la sistemo rilate al la eniga kaj eligo ebenoj.]],
["L:inv() --> inv_L"]      = [[Inversa matrico.]],
[":solve(fn, index_N, initial_d) --> found_d"] = [[Determinu la kondiĉojn sub kiuj la komponanto kun donita indekso estas egala al 0.]],
["L:transform(yIn_d, VIn_d) --> yOut_d, VOut_d"] = [[Trovu la transformon de la pozicio 'dy' kaj optika angulo 'dV' (= v*n) de la trabo per la sistemo. Same kiel L(dy,dV).]],
[":thin(focalDist_d) --> L"] = [[Liveras la matricon por maldika lenso kun la donita fokusa distanco.]],
--["operations"]             = [[L1 == L2, L1 .. L2]],
},
---------- main.lua ----------
main = {
["_ans"]                   = [[Resulto de la lasta operacio.]],
["atanh(x) --> y"]         = [[Hiperbola tangentarco de nombro.]],
["__module__"]             = [[Komputado sistemo bazita sur Lua.]],
["log(x) --> y"]           = [[Natura logaritmo.]],
["exp(x) --> y"]           = [[La eksponenta dependeco.]],
["randn(mean_d=0, dev_d=1) --> float"] = [[Normale distribuita hazarda nombro.]],
["_pi"]                    = [[Nombro pi.]],
["Log(flag_s) --> nil"]    = [[Sesio-registrado al protokola dosiero. Uzu 'on' por komenci kaj 'off' por ĉesi.]],
["tanh(x) --> y"]          = [[Hiperbola tangento.]],
["Run(name_s, isInt=false) --> nil"] = [[Ekzekuti lua- aŭ note- skripton.]],
["sqrt(x) --> y"]          = [[Kvadrata radiko.]],
["asin(x) --> y"]          = [[La sinusarko de nombro d.]],
["acosh(x) --> y"]         = [[La hiperbola kosinusarko de nombro x.]],
["rand() --> float"]       = [[Hazarda nombro inter 0 kaj 1.]],
["Print(...) --> nil"]     = [[Presi Lua tablo en uzanta formato, usi 'scienca' formon de nombro.]],
["Round(x_d, N=0) --> num"] = [[Rondu nombron, difini kvanto de dekumaj ciferoj.]],
["sin(x) --> y"]           = [[La sinuso de nombro d.]],
["Map(fn, in_t) --> out_t"] = [[Taksi funkcion por ĉiu elemento de la tablo.]],
["_e"]                     = [[Eǔlera nombro.]],
["abs(x) --> num"]         = [[La absoluta valoro.]],
["tan(x) --> y"]           = [[La tangento de nombro d.]],
["Range(begin_d, end_d, [step_d]) --> new_R"] = [[Faru tabelon kun vico da nombroj.]],
["atan2(y_d, x_d) --> num"] = [[Tangentarco de nombro dy/dx kun signoj.]],
["sinh(x) --> y"]          = [[Hyperbola sinuso.]],
["randi(N) -> int"]        = [[Hazarda entrejo de 1 al N.]],
["cosh(x) --> y"]          = [[La hiperbola kosinuso de nombro d.]],
["asinh(x) --> y"]         = [[La hiperbola sinusarko de nombro d.]],
["Type(x) --> str"]        = [[Montri objektan tipon.]],
["acos(x) --> y"]          = [[La kosinusarko de nombro d.]],
["cos(x) --> y"]           = [[La kosinuso de nombro d.]],
["use([module_s]) --> str|nil"] = [[Alvoku use('modulo') aǔ use{'moduloA','moduloB'} por akiri ekstrajn modulojn.]],
--["help(fn='main') --> str"] = [[Show information about the function.]],
--["atan(x) --> y"]          = [[Inverse tangent x.]],
--["quit() --> nil"]         = [[Quit the program.]],
},
---------- matrix.lua ----------
matrix = {
["__module__"]             = [[Operacioj kun matricoj. La matricoj estas maldensa defaǔlte.]],
["M:pinv() --> inv_M"]     = [[Redonas pseǔdon-inversa matrico.]],
["M:det() --> num"]        = [[Matrico determinanto.]],
["M:svd() --> U_M, S_M, V_M"] = [[Unuopa valormalkomponaĵo, reveno U, S, V.]],
["M:rows() --> N"]         = [[Nombro da vicoj.]],
["M:chol() --> lower_M"]   = [[Ĉoleska transformo de pozitiva simetria matrico.]],
["M:lu() --> L_M, U_M, perm_M"] = [[LU transformo de la matrico. Redonas L,U kaj P valoroj.]],
["M:norm() --> num"]       = [[Eŭklida normo.]],
[":givensRot(x, y) --> cos_d, sin_d, len_d"] = [[Trovu parametrojn de Givens-rotacio (c,s,r).]],
[":V {...} --> new_V"]     = [[Krei vektoron el listo de nombroj.]],
["M:cols() --> N"]         = [[Nombro da kolumnoj.]],
[":zip(fn, M1, M2,..) --> res_M"] = [[Apliki la funkcion al ĉiu elemento de matricoj.]],
["M:H() --> conj_M"]       = [[Konjugaci transpon. ]],
[":zeros(row_N, col_N=row_N) --> M"] = [[Krei nula matrico.]],
[" {row1_t, row2_t,..} --> new_M"] = [[Krei novan matricon de la listo de vicoj (tabloj).]],
["M:map(fn) --> found_M"]  = [[Apliki la funkcion fn(row[,col,val]) al ĉiuj elementoj, redoni novan matricon.]],
["M:householder(V, start_N) --> hh_M"] = [[Trovu Householder matricon por la donita vektoro.]],
["M:T() --> transpose_M"]  = [[Redonas la transposta matrico.]],
["M:tr() --> sum"]         = [[Spuro de matrico.]],
["M:table() --> tbl"]      = [[Konverti al la simpla Lua tablo.]],
["M:diag() --> V"]         = [[Ĉerpti la diagonalajn elementojn.]],
["M:concat(M2, dir_s) --> comb_M"] = [[Kunmeti du matricoj, dir='h' - en horizontala directo, dir='v' - in vertikala direkto.
Uzi M1 .. M2 por horizontala kunmeto, kaj M1 // M2 por vertikala.]],
["M:eig() --> vectors_M, values_M"] = [[Trovu proprajn vektorojn kaj proprajn valorojn (matricoj).]],
[":fill(row_N, col_N, val=1) --> M"] = [[Krei matricon kun la donita nombroj.]],
["M:copy() --> cpy_M"]     = [[Krei kopion de la matrico.]],
["M:range(rows_t, cols_t) --> range_M"] = [[Akiru submatricon por la donita gamo de vicoj kaj kolumnoj.]],
["M:qr() --> Q_M, R_M"]    = [[QR transformo.]],
["M:rref() --> upd_M"]     = [[PGaǔsa transformo.]],
[":eye(row_N, col_N=row_N) --> M"] = [[Krei identeca matrico.]],
["V:dot(V2) --> num"]      = [[Skalara produkto de du vektoroj.]],
["M:round(N=6) --> nil"]   = [[Rondu nombron en loko.]],
["M:insert(rows_t, cols_t, M2) --> nil"] = [[Enigu duan matricon en la donitan gamon de indeksoj.]],
["M:inv() --> inv_M"]      = [[Redonas la inversa matrico.]],
["V:cross(V2) --> V3"]     = [[Kruco produkto de du 3-elementa vektoroj.]],
["M:bidiag() --> U_M, B_M, V_M"] = [[Bidiagonaligo de matrico, revenas U, B, V.]],
["M:reshape(row_N=size, col_N=1) --> upd_M"] = [[Ŝanĝu matricon.]],
["M:rank() --> N"]         = [[Trovi rangon de la matrico.]],
--["arithmetic"]             = [[a+b, a-b, a*b, a/b, a^b, -a]],
[":diagonal(list_v) --> M"] = [[krei novan matricon kun donita diagonalo.]],
--["comparison"]             = [[a==b, a~=b]],
--["M:minor(row_N, col_N) --> minor_M"] = [[Find minor for the matrix element.]],
},
---------- numeric.lua ----------
numeric = {
[":newton(fn, x0_d) --> num"] = [[Trovu la radiko de ekvacio uzante la metodon de Newton, x0 estas komenca proksimiĝo.]],
[":der(fn, x_d) --> num"]  = [[Kalkuli derivaĵon de la funkcio.]],
[".TOL=0.001"]             = [[Precizeco de kalkuloj.]],
[":ode45(fn, interval_t, y0, {dt=10*TOL,exit=nil}) --> ys_t, yLast"] = [[Nombra solvo de ODE.
Se la paŝo dx ne estas specifita, tiam ĝi estas kalkulita aǔtomate laǔ la postulata precizeco.
Redonas liston de interaj punktoj kaj rezulton yn.]],
["__module__"]             = [[Grupo de funkcioj por la numeraj kalkuloj. Toleremo estas difinita per TOL.]],
[":solve(fn, xLow_d, xUp_d) --> num"] = [[Trovu la radiko de ekvacio fn(x)=0 en la intervalo [a,b].]],
[":trapez(fn, x1_d, x2_d) --> num"] = [[Kalkuli la integralo uzante la metodon de trapezoj.]],
},
---------- polynomial.lua ----------
polynomial = {
[":char(M) --> P"]         = [[Trovu karakterizan polinomon por la matrico.]],
["P:int(x0_d=0) --> int_P"] = [[Trovi la integralo de polinomo, d0 - libera faktoro.]],
["__module__"]             = [[Operacioj kun polinomoj.]],
[":taylor(x_d, fx_d, [fx'_d, fx''_d,..]) --> P"] = [[Akiru Taylor-serialon.]],
[":lagrange(xs_t, ys_t) --> P"] = [[Trovu interpola polinomon en la formo de Lagrange.]],
[":spline(xs_t, ys_t) --> Ps_t"] = [[Kuba spline datuminterpolado. Revenas tabelon kun polinomoj.]],
["P:val(x) --> y"]         = [[Kalkulu la valoro de polinomo P je koordinato x. La sama al P(x).]],
[" {.., v1, v0} --> new_P"] = [[Krei novan poinoman.]],
["P:der() --> der_P"]      = [[Trovi la derivaĵon de polinomo.]],
["P:roots() --> roots_t"]  = [[Trovi la realajn kaj kompleksajn radikojn de polinomo.]],
[":ppval(Ps_t, x_d, [index_N]) --> num"] = [[Revenas valoron de peceta polinomo en la punkto, kaj la polinoma indekso.]],
[":fit(xs_t, ys_t, order_N) --> P"] = [[Trovi la polinoman alproksimiĝon por la kurbo.]],
["P:real() --> roots_t"]   = [[Trovi la realajn radikojn de polinomo.]],
["P:copy() --> cpy_P"]     = [[Krei kopion de polinomo.]],
[":lin(xs_t, ys_t, yBefore_d=0, yAfter_d=y0) --> P"] = [[Lineara dateninterpolado. Revenas tabelon kun polinomoj.]],
[":build(root1, [root2,..]) --> P"] = [[Kalkuli la polinomon per konataj radikoj.]],
--["P:str(char_s='x') --> str"] = [[Pretty print for polynomial.]],
--["arithmetic"]             = [[a+b, a-b, a*b, a/b, a^n, -a]],
--["comparison"]             = [[a==b, a~=b]],
},
---------- quaternion.lua ----------
quaternion = {
["Q:z() --> var"]          = [[z komponanto.]],
[":fromRot(M) --> Q"]      = [[Konverti rotacia matrico al kvaterniono.]],
["Q:mat() --> M"]          = [[Ekvivalenta matrica reprezentado.]],
["__module__"]             = [[Operacioj kun kvaternionoj.]],
["Q:rotate(inVec) --> outVec_t"] = [[Apliki kvaternionon por turni la vektoron.]],
["Q:im() --> imaginary_t"] = [[Akiri imaginan parton.]],
["Q:normalize() --> unit_Q"] = [[Redonas unuo-kvaterniono.]],
["Q:slerp(end_Q, rat_f) --> rat_Q"] = [[Sfera linea interpolado.]],
["Q:re() --> var"]         = [[Reala parto de la kvaterniono.]],
["Q:toAA() --> angle_d, axis_t"] = [[Akiri angulon kaj akson de rotacio.]],
["Q:conj() --> conj_Q"]    = [[Konjugacio.]],
["Q:toRot() --> M"]        = [[Akiru egalan turnan matricon.]],
[" {w, x, y, z} --> new_Q"] = [[Krei novan kvaternionon.]],
["Q:y() --> var"]          = [[y komponanto.]],
["Q:x() --> var"]          = [[x komponanto.]],
[":fromAA(angle_d, axis_d) --> Q"] = [[Krei quaternionon por angulo kaj akso de rotacio.]],
["Q:abs() --> num"]        = [[Valoro de la normo.]],
["Q:inv() --> inv_Q"]      = [[Reversa kvaterniono.]],
["Q:w() --> var"]          = [[Reala parto.]],
--["arithmetic"]             = [[a + b, a - b, a * b, a ^ k, -a]],
--["comparison"]             = [[a == b, a ~= b]],
},
---------- rational.lua ----------
rational = {
[":fromCont(coeff_t) --> R"] = [[Transformi daŭra frakcio al racian nombron.]],
["R:denom() --> var"]      = [[Denominatoro de racia nombro.]],
[":from(src_f, err_f=1E-3) --> R"] = [[Taksas racian nombron el dekuma nombro.]],
["R:toCont() --> coeff_t"] = [[Transformi racian nombron al daŭra frakcio.]],
["R:float() --> num"]      = [[Revenas racia nombron en dekuma formo.]],
["R:num() --> var"]        = [[Numeratoro de racia nombro.]],
[" (num, denom=1) --> new_R"] = [[Krei novan racian nombron.]],
["R:eq(x) --> bool"]       = [[Egaleco kontrolo.]],
[":gcd(a_f, b_f) --> num"] = [[Plej granda komuna divizoro.]],
["__module__"]             = [[Komputadoj kun la raciaj nombroj.]],
--["arithmetic"]             = [[R1+R2, R1-R2, R1*R2, R1/R2, -R, R1^R2]],
--["comparison"]             = [[R1<R2, R1<=R2, R1>R2, R1>=R2, R1==R2, R1~=R2]],
},
---------- special.lua ----------
special = {
[":besselk(order_N, x_d) --> num"] = [[Modifita Bessel-funkcio Kn(x).]],
[":betainc(x_d, a_d, b_d) --> num"] = [[Nekompleta beta-funkcio Ix(a,b).]],
[":gamma(x_d) --> num"]    = [[Gama-funkcio.]],
[":besselj(order_N, x_d) --> num"] = [[Bessel-funkcio de la unua speco.]],
[":betaln(z_d, w_d) --> num"] = [[Naturala logaritmo de la beta-funkcio.]],
[":bessely(order_N, x_d) --> num"] = [[Bessel-funkcio de la dua speco.]],
[":besseli(order_N,x_d) --> num"] = [[Modifita Bessel-funkcio In(x).]],
[":erf(x_d) --> num"]      = [[Eraro funkcio.]],
[":gammp(order_N, x_d) --> num"] = [[Nekompleta gama-funkcio P(N,x).]],
[":erfc(x_d) --> num"]     = [[Kompletiga eraro funkcio.]],
[":legendre(order_N, x_d) --> coeff_t"] = [[Listo de la Legendre polinomoj koeficientoj.]],
[":gammq(order_N, x_d) --> num"] = [[Nekompleta gama-funkcio Q(N,x) = 1-P(N,x).]],
[":beta(z_d, w_d) --> num"] = [[Beta-funkcio.]],
[":gammaln(x_d) --> num"]  = [[Naturala logaritmo de la gama-funkcio.]],
[":expint(pow_N, x_d) --> num"] = [[Eksponenta integralo En(x).]],
["__module__"]             = [[Specialaj matematikaj funkcioj.]],
[":dawson(x_d) --> num"]   = [[Dawson integralo.]],
},
---------- symbolic.lua ----------
symbolic = {
[":def(name_s, args_t, expr_S) --> fn_S"] = [[Difinu simbolan funkcion. S estas aŭ simbola esprimo aŭ Lua funkcio.]],
[":fn(name_s) --> fn_S|nil"] = [[Redonu simbolan funkcion se ĝi estis difinita.]],
["__module__"]             = [[Simbolaj kalkuloj.]],
[":parse(expr_s) --> S1, S2, .."] = [[Akiru simbolan esprimon el linio.]],
["S:eval(env_t={}) --> upd_S|num"] = [[Taksi simbolan esprimon kun la donita medio.]],
[" (num|str) --> new_S"]   = [[Kreu novan simbolan variablon.]],
--["S:isFn() --> bool"]      = [[Return true if the symbol is function.]],
--["S:introspect() --> str"] = [[Show the internal structure.]],
--["S:diff(var_S) --> derivative_S"] = [[Find symbolic derivative.]],
},
---------- units.lua ----------
units = {
["U:value() --> var"]      = [[Akiru objektovaloron. Same kiel #U.]],
["U:key() --> str"]        = [[Akiru mezurunuojn.]],
["__module__"]             = [[Operacioj kun unuoj de mezuroj.]],
[":setRule(name_s, val_U) --> nil"] = [[Aldoni regulon por konverti unuojn de mezuro.]],
["U:convert(new_s) --> upd_U|nil"] = [[Konvertu unu unuojn al alia, redonu novan objekton aŭ nulon.]],
["U:copy() --> cpy_U"]     = [[Krei kopion de objecto.]],
--[".prefix"]                = [[Table of possible prefixes for units.]],
--["comparison"]             = [[U1==U2, U1~=U2, U1<U2, U1<=U2, U1>U2, U1>=U2]],
[" (val=1, name_s) --> new_U"] = [[Krei novan valoron kun unuoj de mezuro.]],
--["arithmetic"]             = [[U1+U2, U1-U2, U1*U2, U1/U2, U1^N]],
},
}