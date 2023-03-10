---------- locale/ru.lua ----------

return {
----------
language = 'Русский',
authors  = [[Станистав Михель]],
---------- dialog nil
Dialog = {
["intro"]                  = [[-------- help([функция]) = справка --------------
---------- use([модуль]) = импорт функций -------
----------------- quit() = выход ----------------
]],
["done"]                   = [[Выполнено.]],
},
---------- array.lua ----------
array = {
["A:map(fn) --> out_A"]    = [[Формирует новый массив путём применения функции к элементам исходного.]],
["A:concat(A2, axis_N) --> A3"] = [[Формирует новый массив путём объединения двух исходных по заданной оси.]],
["A:copy() --> cpy_A"]     = [[Возвращает копию массива.]],
["A:ipairs() --> iter_fn"] = [[Итератор, который при каждом вызове возвращает индекс и элемент множества.]],
[":zip(fn, ...) --> A"]    = [[Применяет функцию к списку массивов для получения нового массива.]],
["A:sub(ind1_t, ind2_t) --> range_A"] = [[Возвращает массив, ограниченный двумя индексами.]],
["A:get(ind_t) --> var"]   = [[Возвращает значение заданного элемента.]],
["A:isEqual(A2) --> bool"] = [[Сравнение размерностей массивов.]],
["__module__"]             = [[Операции с массивами, т.е. многомерными упорядоченными последовательностями элементов. Индексы задаются в виде таблицы. Индексация с единицы.]],
["A:set(ind_t, var) --> nil"] = [[Присваивание значения заданному элементу.]],
["A:dim() --> int"]        = [[Возвращает размерность массива.]],
["A:capacity() --> int"]   = [[Возвращает максимальное число элементов в массиве. Эквивалентно #A.]],
--[" {size1_N, [size2_N, ..]} --> new_A"] = [[Create empty array with the given size.]],
--["comparison"]             = [[a == b, a ~= b]],
},
---------- asciiplot.lua ----------
asciiplot = {
["F:reset() --> nil"]      = [[Очистка холста.]],
["F:addString(row_N, col_N, str) --> nil"] = [[Добавление строки в указанную позицию.]],
["F:plot(...) --> nil"]    = [[Построение графика на основе таблицы, можно указать номера столбцов и дополнительные опции.]],
["F:scale(factor_d, isDefault=false) --> F"] = [[Изменить размер графика относительно начальных размеров.]],
["F:contour(fn, {view='XY'}) --> nil|str"] = [[Проекция контуров функции fn(x,y). Проекции: XY, XZ, YZ, XYZ.]],
["F:tplot(data_t, {yfix=false}) --> nil"] = [[Построение графика на основе таблицы, можно указать номера столбцов и дополнительные опции.]],
["F:addPoint(x_d, y_d, char_s) --> nil"] = [[Добавить точку с координатами (x,y) с помощью символа.]],
["F:addPose(row_N, col_N, char_s) --> nil"] = [[Установка символа в указанную позицию.]],
["F:copy() --> cpy_F"]     = [[Возвращает копию объекта.]],
["__module__"]             = [[Визуализация данных с помощью псевдо-графики.]],
["F:bar(t, vy=2, x_N=1) --> nil"] = [[Столбчатая диаграмма. vy может быть индексом y в таблице t либо списком y-в.]],
[":concat(...) --> str"]   = [[Горизонтальное объединение графиков равного размера. Для двух объектов можно использовать оператор '..' .]],
--[" (width_N=73, height_N=21) --> new_F"] = [[Create new asciiplot.]],
--["F:setX(range_t) --> nil"] = [[Update X range.]],
--["F:showX(pos_s|nil) --> nil"] = [[Define X axis position.]],
--["F:title(str) --> nil"]   = [[Set new title.]],
--["F:axes() --> tbl"]       = [[Get {size, log, range, pose} for each size.]],
--["F:setZ(range_t) --> nil"] = [[Update Z range.]],
--["F:showZ(pos_s|nil) --> nil"] = [[Define Z axis position.]],
--["F:logY(isLog) --> nil"]  = [[Change Y axis type to logarithmic.]],
--["F:showY(pos_s|nil) --> nil"] = [[Define Y axis position.]],
--["Plot(...) --> nil"]      = [[Plot arguments in form 't', 't1,t1', 'fn,nm', 'fn1,fn2' etc.]],
--["F:resize(src_F | (width_N, height_N)) --> nil"] = [[Update size of canvas.]],
--["F:setY(range_t) --> nil"] = [[Update Y range.]],
--["F:logX(isLog) --> nil"]  = [[Change X axis type to logarithmic.]],
--["F:logZ(isLog) --> nil"]  = [[Change Z axis type to logarithmic.]],
},
---------- bigint.lua ----------
bigint = {
["B:base() --> int"]       = [[Возвращает основание системы счисления.]],
["B:isPrime([method_s]) --> bool"] = [[Проверка числа на простоту. Установите метод 'Fermat' для использования малой теоремы Ферма.]],
["B:factorize() --> primeBs_t"] = [[Возвращает список простых множителей для данного числа.]],
["B:at(N) --> int"]        = [[Возвращает цифру в позиции N.]],
[" (var) --> new_B"]       = [[Создаёт большое целое число на основе числа, строки или таблицы.]],
["B:gcd(B2) --> B3"]       = [[Наибольший общий делитель.]],
["B:fact() --> B!"]        = [[Вычисляет факториал целого неотрицательного числа.]],
["B:rebase(N) --> upd_B"]  = [[Конвертирует число в новую систему счисления.]],
[":random(B) --> rand_B"]  = [[Псевдо-случайное число от 0 до B.]],
["B:eq(x) --> bool"]       = [[Проверка равенства двух чисел.]],
["__module__"]             = [[Вычисления с целыми числами произвольной длины.]],
["B:abs() --> num"]        = [[Возвращает модуль числа.]],
["B:float() --> num"]      = [[Представление в виде числа с плавающей точкой.]],
--["arithmetic"]             = [[a+b, a-b, a*b, a/b, a%b, a^b, -a, #a]],
--["comparison"]             = [[a<b, a<=b, a>b, a>=b, a==b, a~=b]],
},
---------- complex.lua ----------
complex = {
["Z:im() --> var"]         = [[Мнимая часть.]],
["Z:round(N=6) --> rounded_Z"] = [[Возвращает число с округлением до заданного количества знаков.]],
["Z:exp() --> y_Z"]        = [[Комплексная экспонента.]],
[" (re=0, im=0) --> new_Z"] = [[Создание комплексного числа.]],
["Z:atan() --> y_Z"]       = [[Комплексный обратный тангенс.]],
["Z:acos() --> y_Z"]       = [[Комплексный обратный косинус.]],
["Z:acosh() --> y_Z"]      = [[Комплексный обратный гиперболический косинус.]],
["Z:abs() --> float"]      = [[Модуль комплексного числа.]],
["Z:cosh() --> y_Z"]       = [[Комплексный гиперболический косинус.]],
["Z:asin() --> y_Z"]       = [[Комплексный обратный синус.]],
["Z:sin() --> y_Z"]        = [[Комплексный синус.]],
["Z:conj() --> conj_Z"]    = [[Комплексно-сопряженное число. Эквивалентно ~C.]],
["Z:asinh() --> y_Z"]      = [[Комплексный обратный гиперболический синус.]],
["Z:tanh() --> y_Z"]       = [[Комплексный гиперболический тангенс.]],
["Z:log() --> y_Z"]        = [[Комплексный логарифм.]],
["Z:sqrt() --> y_Z"]       = [[Комплексный квадратный корень.]],
["Z:cos() --> y_Z"]        = [[Комплексный косинус.]],
["Z:sinh() --> y_Z"]       = [[Комплексный гиперболический синус.]],
["Z:re() --> var"]         = [[Действительная часть.]],
["Z:arg() --> float"]      = [[Возвращает агрумент комплексного числа.]],
[":i(x=1) --> new_Z"]      = [[Возвращает комплексное число v*i.]],
["Z:atanh() --> y_Z"]      = [[Комплексный обратный гиперболический тангенс.]],
["__module__"]             = [[Вычисления с комплексными числами.]],
["Z:tan() --> y_Z"]        = [[Комплексный тангенс.]],
[":trig(module, angle) --> new_Z"] = [[Создание комплексного числа по модули и агрументу.]],
--["comparison"]             = [[a==b, a~=b]],
--["arithmetic"]             = [[a+b, a-b, a*b, a/b, a^b, -a]],
},
---------- const.lua ----------
const = {
[".phi.Da"]                = [[Атомная единица массы.]],
[".astro.pc"]              = [[Один парсек.]],
[".phy.e"]                 = [[Заряд электрона.]],
[".phy.sigma"]             = [[Постоянная Стефана-Больцмана.]],
[".phy.NA"]                = [[Число Авогадро.]],
[".math.phi"]              = [[Золотое сечение.]],
[".math.pi"]               = [[Отношение длины окружности к её диаметру.]],
[".phy.c"]                 = [[Скорость света.]],
[".phy.Rinf"]              = [[Постоянная Ридберга.]],
[".phy.Vm"]                = [[Объём моля идеального газа.]],
[".phy.g"]                 = [[Ускорение свободного падения.]],
[":add(name_s, value, [units_s]) --> nil"] = [[Добавление временной константы.]],
[".phy.G"]                 = [[Гравитационная постоянная.]],
[".phy.k"]                 = [[Постоянная Больцмана.]],
[":remove(name_s) --> bool"] = [[Удаление константы.]],
[".math.e"]                = [[Основание натурального логарифма.]],
[".astro.au"]              = [[Астрономическая единица.]],
[".phy.h"]                 = [[Число планка.]],
["__module__"]             = [[Разнообразные константы.]],
[".phy.R"]                 = [[Универсальная газовая постоянная.]],
[".phy.mu0"]               = [[Магнитная постоянная.]],
[".phy.eps0"]              = [[Электрическая постоянная.]],
[".astro.ly"]              = [[Световой год.]],
},
---------- data.lua ----------
data = {
[":ref(src_t, begin_N=1, end_N=#src_t) --> new_R"] = [[Возвращает "ссылку" на таблицу элементов.]],
[":geomean(data_t, [weigh_t]) --> num"] = [[Геометрическое среднее.]],
[":std(data_t, [weight_t]) --> dev_f, var_f"] = [[Возвращает стандартное отклонение и дисперсию.]],
[":is(data_t, cond_fn) --> yesno_t"] = [[Возвращает массив весов на основе булевой функции.]],
[":mean(data_t, [wight_t]) --> num"] = [[Вычисляет среднее значение.]],
[":cov2(xs_t, ys_t) --> float"] = [[Вычисляет ковариацию для двух списков.]],
[":tcdf(x_d, deg_N) --> num"] = [[Распределение Стьюдента.]],
[":zip(fn,...) --> tbl"]   = [[Последовательно применяет функцию к списку векторов.]],
[":freq(data_t) --> tbl"]  = [[Возвращает таблицу частот элементов.]],
[":histcounts(data_t, rng_v=10) --> sum_t, edges_t"] = [[Распределение данных по интервалам. Можно указать число интервалов разбиения, либо задать границы в виде таблицы.]],
[":xLt(num) --> cond_fn"]  = [[Возвращает функцию для условия x < d.]],
[":Fn(expr_s, arg_N=2) --> fn"] = [[Генерирует функцию из строки с параметрами x1, x2 и т.д.]],
[":max(data_t) --> var, ind_N"] = [[Максимальный элемент и его индекс.]],
[":cov(data_t) --> cov_M"] = [[Возвращает матрицу ковариации для списка векторов.]],
[":xIn(num1, num2) --> cond_fn"] = [[Возвращает функцию для условия d1 <= x <= d2.]],
[":sum(data_t) --> var"]   = [[Возвращает сумму элементов.]],
[":tpdf(x_d, deg_N) --> num"] = [[Плотность распределения Стьюдента.]],
[":xEq(num) --> cond_fn"]  = [[Возвращает функцию для условия x == d.]],
[":filter(in_t, condition) --> out_t"] = [[Вильтрует список данных. Условием является булева функция или массив весов.]],
[":harmmean(data_t, [weigh_t]) --> num"] = [[Гармоническое среднее.]],
[":csvwrite(file_s, data_t, char=',', isCol=false) --> nil"] = [[Сохраняет таблицу в файл с заданным разделителем.]],
[":csvread(file_s, delim_s=',', isCol=false) --> tbl"] = [[Формирует таблицу на основе файла с заданным разделителем.]],
[":isNot(data_t, cond_fn) --> yesno_t"] = [[Возвращает инвертированные веса для булевой функции.]],
[":median(data_t) --> num"] = [[Возвращает медиану распределения.]],
[":xGt(num) --> cond_fn"]  = [[Возвращает функцию для условия x > d.]],
[":moment(order_N, data_t, [weigth_t]) --> num"] = [[Центральный момент t порядка N (с весами tw).]],
[":min(data_t) --> var, ind_N"] = [[Возвращает наименьший элемент и его индекс.]],
["__module__"]             = [[Обработка данных и статистические расчёты.]],
},
---------- geodesy.lua ----------
geodesy = {
[":fromENU(blRef_t, xyzRef_t, top_t) --> xyzObs_t"] = [[Преобразовать топоцентрические координаты точки в геоцентрические.]],
["E:toBLH(xyz_t) --> blh_t"] = [[Приобразовать геоцентрические координаты в геодезические.]],
[":deg2dms(deg_d) --> num"] = [[Возвращает градусы, минуты и секунды для заданного угла в градусах.]],
[":hashEncode(coord_t, letter_N=6) --> hash_s"] = [[Вычисления геохэша для точки.]],
[":toENU(blRef_t, xyzRef_t, xyzObs_t) --> top_t"] = [[Преобразовать геоцентрические координаты точки в топоцентрические.]],
["E:toXYZ(blh_t) --> xyz_t"] = [[Преобразует геодезические координаты в геоцентрические.]],
[":hashDecode(hash_s) --> coord_t, range_t"] = [[Определения положения зоны по геохэшу.]],
["E:solveInv(blh1_t, blh2_t) --> dist_d, az1_d, az2_d"] = [[Решение обратной задачи геодеции, поиск расстояния и азимутов для двух заданных точек.]],
["__module__"]             = [[Преобразования координат и другие геодезические задачи.]],
["E:solveDir(blh_t, az1_d, dist_d) --> blh_t, az2_d"] = [[Решение прямой задачи геодезии, поиск положения и азимута второй точки при заданных начальной точке, направлении и расстоянии.]],
[":grav(latitude_d) --> num"] = [[Международная формула гравитации, аргумент в радианах.]],
[":dms2rad(deg_d, min_d=0, sec_d=0) --> num"] = [[Преобразует градусы, минуты и секунды в радианы.]],
--["E.blhInto[E2] --> fn"]   = [[Get function to transform geodetic coordinates from A to B system using the Molodensky method.]],
--["E.xyzInto[E2] --> fn"]   = [[Get function to transform coordinates from E to E2 system.]],
--["E:utm2ll(utm_t) --> blh_t"] = [[Find Geodetic coordinates for the given UTM pose and zone]],
--["E:ll2utm(blh_t) --> utm_t"] = [[Find UTM projection for the given coordinates.]],
},
---------- gnuplot.lua ----------
gnuplot = {
[":tpolar(var, [x_N, y1_N, y2_N,..]) --> nil"] = [[График в полярных координатах для таблицы, матрицы или файла. Опциональные элементы определяют стоблцы.]],
[" () --> new_G"]          = [[Подготовка таблицы для Gnuplot.]],
[":tplot(var, [x_N, y1_N, y2_N,..]) --> nil"] = [[Построение графика на основе таблицы, матрицы или файла. Опциональные элементы определяют необходимые столбцы.]],
[":plot(x1_t, [y1_t, nm_s, x2_t,..]) --> nil"] = [['x' - список чисел, 'y' - список или функция, 'nm' - имя кривой.]],
[":surfplot(x1_t, y1_t, fn1, [nm_s, x2_t, y2_t,..]) --> nil"] = [[Построение поверхности. 'x' и 'y' - списки чисел, 'fn' - функция двух агрументов, 'nm' - имя поверхности.]],
["G:show() --> nil"]       = [[Строит график на основе параметров, представленных в виде таблицы Lua.]],
["G:copy() --> cpy_G"]     = [[Возвращает копию параметров графика.]],
["__module__"]             = [[Интерфейс для взаимодействия с Gnuplot.]],
[":polarplot(x1_t, y1_t, [nm_s, x2_t, y2_t,..]) --> nil"] = [[График в полярных координатах. 'x' - список чисел, 'y' - список либо функция, 'nm' - имя кривой.]],
[":tsurf(var, [x_N, y_N, z1_N, z2_N,..]) --> nil"] = [[Построение графика поверхности на основе таблицы, матрицы или файла. Опциональные элементы определяют необходимые столбцы.]],
["G:add(curve_v) --> nil"] = [[Добавить функцию для построения.]],
--[=[[".keys"]                  = [[  Options / examples:
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
]],]=]
},
---------- graph.lua ----------
graph = {
["G:edges() --> edges_t"]  = [[Возвращает список рёбер.]],
["G:isNegative() --> bool"] = [[True если граф содержит отрицательные веса.]],
["G:dfs(startNote, goalNode) --> isFound, path_t"] = [[Поиск в глубину. Возвращает флаг результата и найденный путь.]],
["G:copy() --> cpy_G"]     = [[Создаёт копию графа.]],
["G:pathBF(startNode, [goalNode]) --> dist_d, path_t|prev_t"] = [[Поиск кратчайшего пути алгоритмом Беллмана-Форда. Возвращает таблицу расстояний и узлов. Если задана цель, возвращает путь и его длину.]],
["G:add(var) --> nil"]     = [[Добавляет узел (одиночное имя) или ребро. Ребро представлено списком, содержащим начало, конец и, при необходимости, вес (веса).]],
["G:remove(var) --> nil"]  = [[Удаляет узел или ребро. Узлы представляются именем, рёбра - двухэлементным списком.]],
["G:bfs(startNode, goalNode) --> isFound, path_t"] = [[Поиск в ширину. Возвращает флаг результата и найденный путь.]],
["G:pathD(startNode, [goalNode]) --> dist_d, path_t|prev_t"] = [[Поиск кратчайшего пути алгоритмом Дейкстры. Возвращает таблицу расстояний и узлов. Если задана цель, возвращает путь и его длину.]],
["G:isComplete() --> bool"] = [[True если граф полный.]],
["__module__"]             = [[Операции с графами.]],
[" {v1, v2,..} --> new_G"] = [[Создает новый граф.]],
["G:nodes() --> node_t"]   = [[Возвращает список узвло графа.]],
["G:isWeighted() --> bool"] = [[True если граф взвешенный.]],
["G:isDirected() --> bool"] = [[True если граф направленный.]],
},
---------- lens.lua ----------
lens = {
[":solve(fn, index_N, initial_d) --> found_d"] = [[Поиск условий, при которых компонент с заданным индексом равен 0. d0 - начальное предположение.]],
["L:transform(yIn_d, VIn_d) --> yOut_d, VOut_d"] = [[Определяет положение луча 'dy' и оптический угол 'dV' (= v*n) на выходе оптической системы. Эквивалентно вызову L(dy,dV).]],
[":gaussSize(waist_d, lambda_d, dist_d) --> rad_d, curv_d"] = [[Определение радиуса и кривизны гаусова пучка на заданном расстоянии.]],
["L:beam(inRad_d, inCurv_d, lambda_d) --> outRad_d, outCurv_d"] = [[Определение радиуса и кривизны фронта гаусова пуска на выходе из системы.]],
[" {A_d, B_d, C_d, D_d} --> new_L"] = [[Новый объект с произвольными параметрами A, B, C, D.]],
[":ref(rad_d, n1_d, n2_d) --> L"] = [[Возвращает матрицу для преломления с учётом показателей преломления и радиуса кривизны поверхности.]],
[":trans(dist_d, n_d) --> L"] = [[Возвращает матрицу перемещения с учётом показателя преломления.]],
[":gaussParam(waist_d, lambda_d) --> div_d, range_d"] = [[Определение расходимости и размера ближней зоны пучка.]],
["L:isUnit() --> bool"]    = [[Проверка, является ли матрица единичной.]],
["L:copy() --> cpy_L"]     = [[Возвращает копию объекта.]],
[":mirror(rad_d, n_d) --> L"] = [[Возвращает матрицу отражения на кривой поверхности с учётом показателя преломления.]],
[":thin(focalDist_d) --> L"] = [[Возвращает матрицу для тонкой линзы при заданном фокальном расстоянии.]],
["L:det() --> determinant_d"] = [[Возвращает определитель матрицы.]],
[":afocal(magn_d) --> L"]  = [[Возвращает матрицу для афокальной системы.]],
["__module__"]             = [[Матричные методы в параксиальной оптике.]],
["L:cardinal(nLft_d=1, nRht_d=1) --> points_t"] = [[Поиск положения кардинальных точек системы относительно входных и выходных плоскостей, с учётом показателей преломления. Возвращает список расстояний.]],
["L:inv() --> inv_L"]      = [[Инвертированная матрица системы.]],
--["operations"]             = [[L1 == L2, L1 .. L2]],
},
---------- main.lua ----------
main = {
["_ans"]                   = [[Результат последней операции.]],
["abs(x) --> num"]         = [[Абсолютная величина.]],
["tan(x) --> y"]           = [[Тангенс x.]],
["Range(begin_d, end_d, [step_d]) --> new_R"] = [[Генерация диапазона чисел.]],
["asinh(x) --> y"]         = [[Гиперболический арксинус.]],
["acos(x) --> y"]          = [[Арккосинус x.]],
["randi(N) -> int"]        = [[Случайное целое число от 1 до N.]],
["atanh(x) --> y"]         = [[Гиперболический арктангенс.]],
["log(x) --> y"]           = [[Натуральный логарифм.]],
["exp(x) --> y"]           = [[Экспонента.]],
["randn(mean_d=0, dev_d=1) --> float"] = [[Нормально распределённая случайная величина с заданным средним значением и дисперсией.]],
["cos(x) --> y"]           = [[Косинус x.]],
["tanh(x) --> y"]          = [[Гиперболический тангенс.]],
["Run(name_s, isInt=false) --> nil"] = [[Выполнить lua- или note- файл. Установите флаг bInt для интерактивного выполнения.]],
["sqrt(x) --> y"]          = [[Квадратный корень.]],
["asin(x) --> y"]          = [[Арксинус x.]],
["acosh(x) --> y"]         = [[Гиперболический арккосинус.]],
["rand() --> float"]       = [[Случайное число от 0 до 1.]],
["Print(...) --> nil"]     = [[Дополненная функция печати, показывает элементы таблицы, представляет числа в н "научно" виде.]],
["_e"]                     = [[Число Эйлера.]],
["Round(x_d, N=0) --> num"] = [[Округление числа до заданного количества десятичных знаков.]],
["_pi"]                    = [[Число pi.]],
["atan2(y_d, x_d) --> num"] = [[Арктангенс dy/dx с учётом знака.]],
["Log(flag_s) --> nil"]    = [[Сохранение сессии в файл лога. Используйте 'on'/'off' чтобы запустить/остановить процесс.]],
["sin(x) --> y"]           = [[Синус x.]],
["__module__"]             = [[Программа для математических расчётов на Lua.]],
["sinh(x) --> y"]          = [[Гиперболический синус.]],
["cosh(x) --> y"]          = [[Гиперболический косинус.]],
["Map(fn, in_t) --> out_t"] = [[Вычисляет функцию для всех элементов списка (таблицы), возвращает новый список.]],
["Type(x) --> str"]        = [[Печатает тип объекта, распознаёт типы, заданные в Sonata.]],
--["use([module_s]) --> str|nil"] = [[Call use('module') or use{'module1','module2'} to load new functions.]],
--["help(fn='main') --> str"] = [[Show information about the function.]],
--["atan(x) --> y"]          = [[Inverse tangent x.]],
--["quit() --> nil"]         = [[Quit the program.]],
},
---------- matrix.lua ----------
matrix = {
["V:cross(V2) --> V3"]     = [[Векторное произведение 3-элементных векторов.]],
["V:dot(V2) --> num"]      = [[Скалярное произведение векторов.]],
["M:tr() --> sum"]         = [[След матрицы.]],
["M:qr() --> Q_M, R_M"]    = [[QR разложение матрицы. Возвращает Q и R.]],
[":fill(row_N, col_N, val=1) --> M"] = [[Создать матрицу, заполненную заданным числом.]],
["M:pinv() --> inv_M"]     = [[Вовзращает псевдо-обратную матрицу.]],
["M:map(fn) --> found_M"]  = [[Формирует новую матрицу путём применения указанной функции к исходной. Функция может зависеть как от элементов, f(x) так и от индексов f(x,row,col).]],
[" {row1_t, row2_t,..} --> new_M"] = [[Создаёт матрицу на основе списка строк.]],
["M:cols() --> N"]         = [[Число столбцов.]],
["M:concat(M2, dir_s) --> comb_M"] = [[Объединяет две матрица горизонтально (dir='h') или вертикально (dir='v').
Горизонтальная конкатенация доступна в виде M1 .. M2, а вертикальная - M1 // M2.]],
["M:rows() --> N"]         = [[Число строк.]],
["M:bidiag() --> U_M, B_M, V_M"] = [[Бидиагонализация матрицы, возвращает U, B, V.]],
[":givensRot(x, y) --> cos_d, sin_d, len_d"] = [[Возвращает параметры вращения Гивенса (c,s,r).]],
["M:inv() --> inv_M"]      = [[Обратная матрица.]],
["M:table() --> tbl"]      = [[Преобразует матрицу в обыкновенную Lua таблицу.]],
["M:insert(rows_t, cols_t, M2) --> nil"] = [[Вставляет матрицу в заданный диапазон строк и столбцов.]],
[":zip(fn, M1, M2,..) --> res_M"] = [[Поэлементно применяет функцию к матрицам для формирования новой матрицы.]],
[":zeros(row_N, col_N=row_N) --> M"] = [[Формирует матрицу нулей.]],
["M:T() --> transpose_M"]  = [[Транспонирование матрицы. Эквивалентно T().]],
["M:svd() --> U_M, S_M, V_M"] = [[Сингулярное разложение матрицы, возвращает U, S, V.]],
["M:rref() --> upd_M"]     = [[Преобразование матрицы методом Гаусса.]],
["M:eig() --> vectors_M, values_M"] = [[Возвращает матрицы из собственных векторов и чисел.]],
["M:chol() --> lower_M"]   = [[Преобразование Холески для положительно определённой симметричной матрицы.]],
[":eye(row_N, col_N=row_N) --> M"] = [[Единичная матрица.]],
["M:reshape(row_N=size, col_N=1) --> upd_M"] = [[Изменить размер матрицы.]],
["M:H() --> conj_M"]       = [[Сопряженная транспонированная матрица.]],
["M:det() --> num"]        = [[Определитель матрицы.]],
["M:householder(V, start_N) --> hh_M"] = [[Возвращает матрицу преобразования Хаусхолдера.]],
["M:norm() --> num"]       = [[Евклидова норма.]],
["M:diag() --> V"]         = [[Бидиагонализация матрицы, возвращает U, B, V.]],
["M:round(N=6) --> nil"]   = [[Округлить все элементы матрицы до заданного числа знаков.]],
["__module__"]             = [[Операции с матрицами. На нулевые элементы память не расходуется. Индексация с единицы.]],
["M:lu() --> L_M, U_M, perm_M"] = [[LU преобразование матрицы. Возвращает L, U и P.]],
["M:copy() --> cpy_M"]     = [[Возвращает копию матрицы.]],
["M:range(rows_t, cols_t) --> range_M"] = [[Возвращает подматрицу с заданным интервалом строк и столбцов.]],
["M:rank() --> N"]         = [[Возвращает ранг матрицы.]],
--[":V {...} --> new_V"]     = [[Create vector from list of numbers.]],
--["arithmetic"]             = [[a+b, a-b, a*b, a/b, a^b, -a]],
--["M:minor(row_N, col_N) --> minor_M"] = [[Find minor for the matrix element.]],
--[":diagonal(list_v) --> M"] = [[Create new matrix with the given diagonal elements.]],
--["comparison"]             = [[a==b, a~=b]],
},
---------- numeric.lua ----------
numeric = {
[":der(fn, x_d) --> num"]  = [[Оценка производной функции в точке.]],
[":newton(fn, x0_d) --> num"] = [[Поиск корня методом Ньютона в окрестностях заданной точки.]],
["__module__"]             = [[Функции для численных расчётов. Все функции работают с точностью, определяемой параметром TOL.]],
[":trapez(fn, x1_d, x2_d) --> num"] = [[Интегрирование методом трапеций.]],
[":ode45(fn, interval_t, y0, {dt=10*TOL,exit=nil}) --> ys_t, yLast"] = [[Численное решение ОДУ.
Первый параметр задаёт уравнение, второй - временной интервал, третий - начальное значение функции. Дополнительно можно определить такие параметры как шаг интегрирования и условие прерывания.
Возвращает таблицу промежуточных точек и конечное значение yn.]],
[":solve(fn, xLow_d, xUp_d) --> num"] = [[Поиск корня уравнения fn(x)=0 на интервале [a,b].]],
[".TOL=0.001"]             = [[Точность решения.]],
},
---------- polynomial.lua ----------
polynomial = {
[":fit(xs_t, ys_t, order_N) --> P"] = [[Аппроксимация точек полиномом заданной степени.]],
[":lagrange(xs_t, ys_t) --> P"] = [[Интерполяция данных полиномом Лагранжа.]],
[":spline(xs_t, ys_t) --> Ps_t"] = [[Интерполяция данных кубическими сплайнами. Возвращает таблицу полиномов.]],
[" {.., v1, v0} --> new_P"] = [[Создаёт полином на основе списка коэффициентов.]],
[":build(root1, [root2,..]) --> P"] = [[Строит полином на основе списка корней.]],
[":taylor(x_d, fx_d, [fx'_d, fx''_d,..]) --> P"] = [[Формирует полином Тейлора в окрестности заданной точки.]],
[":char(M) --> P"]         = [[Возвращает характеристический полином матрицы.]],
["P:val(x) --> y"]         = [[Вычисляет значение полинома в заданной точке. Эквивалентно вызаву P(x).]],
["P:int(x0_d=0) --> int_P"] = [[Первообразная полинома, x0 - свободный коэффициент.]],
["P:copy() --> cpy_P"]     = [[Возвращает копию полинома.]],
["P:der() --> der_P"]      = [[Первая производная полинома.]],
[":lin(xs_t, ys_t, yBefore_d=0, yAfter_d=y0) --> P"] = [[Линейная интерполяция. Возвращает таблицу полиномов.]],
["__module__"]             = [[Действия над полиномами.]],
[":ppval(Ps_t, x_d, [index_N]) --> num"] = [[Вычисляет значение интерполяции из таблицы полиномов, можно использовать индекс полинома для ускорения расчёта.]],
["P:roots() --> roots_t"]  = [[Поиск действительных и комплексных корней полинома.]],
["P:real() --> roots_t"]   = [[Возвращает список действительных корней полинома.]],
--["arithmetic"]             = [[a+b, a-b, a*b, a/b, a^n, -a]],
--["P:str(char_s='x') --> str"] = [[Pretty print for polynomial.]],
--["comparison"]             = [[a==b, a~=b]],
},
---------- quaternion.lua ----------
quaternion = {
["Q:im() --> imaginary_t"] = [[Возвращает таблицу мнимых элементов кватерниона.]],
["Q:normalize() --> unit_Q"] = [[Возвращает единичный кватернион.]],
["Q:slerp(end_Q, rat_f) --> rat_Q"] = [[Сферическая линейная интерполяция двух кватернионов.]],
["Q:re() --> var"]         = [[Действительная часть (эквивалентно Q.w).]],
["Q:toAA() --> angle_d, axis_t"] = [[Возвращает угол поворота и ось вращения.]],
["Q:conj() --> conj_Q"]    = [[Сопряжённый кватернион.]],
["Q:toRot() --> M"]        = [[Возвращает эквивалентную матрицу вращения.]],
[":fromAA(angle_d, axis_d) --> Q"] = [[Возвращает кватернион для заданного угла и оси вращения.]],
["Q:abs() --> num"]        = [[Норма кватерниона.]],
["Q:inv() --> inv_Q"]      = [[Возвращает крватернион, обратный к данному.]],
["Q:z() --> var"]          = [[Компонента z.]],
["Q:y() --> var"]          = [[Компонента y.]],
["Q:x() --> var"]          = [[Компонента x.]],
[":fromRot(M) --> Q"]      = [[Строит кватернион на основе угла поворота и оси вращения.]],
["Q:mat() --> M"]          = [[Представление в виде эквивалентной матрицы.]],
["Q:w() --> var"]          = [[Действительная часть w.]],
["Q:rotate(inVec) --> outVec_t"] = [[Возвращает вектор, полученный при вращении с помощью заданного кватерниона.]],
["__module__"]             = [[Операции над кватернионами.]],
--[" {w, x, y, z} --> new_Q"] = [[Create new quaternion.]],
--["arithmetic"]             = [[a + b, a - b, a * b, a ^ k, -a]],
--["comparison"]             = [[a == b, a ~= b]],
},
---------- rational.lua ----------
rational = {
["R:float() --> num"]      = [[Представление рационального числа в десятичном виде.]],
["R:num() --> var"]        = [[Возвращает числитель.]],
["R:eq(x) --> bool"]       = [[Проверка равенства двух чисел.]],
[":gcd(a_f, b_f) --> num"] = [[Наибольший общий делитель.]],
[" (num, denom=1) --> new_R"] = [[Для создания рационального числа укажите числитель и (опционально) знаменатель.]],
[":fromCont(coeff_t) --> R"] = [[Преобразование цепной дроби в рациональное число.]],
["R:denom() --> var"]      = [[Возвращает знаменатель.]],
[":from(src_f, err_f=1E-3) --> R"] = [[Оценка дроби рациональным числом с заданной точностью.]],
["R:toCont() --> coeff_t"] = [[Преобразование рационального числа в цепную дробь.]],
["__module__"]             = [[Операции с числами, представленными в виде дроби.]],
--["comparison"]             = [[R1<R2, R1<=R2, R1>R2, R1>=R2, R1==R2, R1~=R2]],
--["arithmetic"]             = [[R1+R2, R1-R2, R1*R2, R1/R2, -R, R1^R2]],
},
---------- special.lua ----------
special = {
[":betaln(z_d, w_d) --> num"] = [[Натуральный логарифм бета-функции.]],
[":besseli(order_N,x_d) --> num"] = [[Модифицированная функция Бесселя In(x).]],
[":bessely(order_N, x_d) --> num"] = [[Функция Бесселя второго рода.]],
[":erf(x_d) --> num"]      = [[Функция ошибки.]],
[":gammp(order_N, x_d) --> num"] = [[Неполная гамма-функция P(N,x).]],
[":erfc(x_d) --> num"]     = [[Дополнительная функция ошибки.]],
[":gammq(order_N, x_d) --> num"] = [[Неполная гамма-функция Q(N,x) = 1-P(N,x).]],
[":beta(z_d, w_d) --> num"] = [[Бета-функция.]],
[":gammaln(x_d) --> num"]  = [[Натуральный логарифм гамма-функции.]],
[":expint(pow_N, x_d) --> num"] = [[Экспоненциальный интеграл En(x).]],
[":legendre(order_N, x_d) --> coeff_t"] = [[Список коэффициентов полиномов Лежандра.]],
[":betainc(x_d, a_d, b_d) --> num"] = [[Неполная бета-функция Ix(a,b).]],
["__module__"]             = [[Специальные функции.]],
[":gamma(x_d) --> num"]    = [[Гамма функция.]],
[":besselj(order_N, x_d) --> num"] = [[Функция Бесселя первого рода.]],
[":besselk(order_N, x_d) --> num"] = [[Модифицированная функция Бесселя Kn(x).]],
[":dawson(x_d) --> num"]   = [[Интеграл Доусона.]],
},
---------- symbolic.lua ----------
symbolic = {
[":fn(name_s) --> fn_S|nil"] = [[Возвращает символьную функцию с заданным именем или nil.]],
[":parse(expr_s) --> S1, S2, .."] = [[Формирует символьное выражение из строки.]],
["__module__"]             = [[Символьные преобразования.]],
["S:eval(env_t={}) --> upd_S|num"] = [[Вычисляет выражение с заданным окружением.]],
[":def(name_s, args_t, expr_S) --> fn_S"] = [[Создаёт символьную функцию. S либо символьное выражение, либо Lua функция.]],
--["S:diff(var_S) --> derivative_S"] = [[Find symbolic derivative.]],
--["S:isFn() --> bool"]      = [[Return true if the symbol is function.]],
--[" (num|str) --> new_S"]   = [[Create new symbolic variable.]],
--["S:introspect() --> str"] = [[Show the internal structure.]],
},
---------- units.lua ----------
units = {
["U:convert(new_s) --> upd_U|nil"] = [[Преобразование единиц измерения, возвращает новый объект или nil.]],
["U:value() --> var"]      = [[Возвращает значение объекта. Эквивалентно #U.]],
[" (val=1, name_s) --> new_U"] = [[Создание числа с единицами измерения.]],
["U:key() --> str"]        = [[Возвращает единицы измерения.]],
["U:copy() --> cpy_U"]     = [[Создание копии объекта с его единицами измерения.]],
[":setRule(name_s, val_U) --> nil"] = [[Добавление нового правила преобразования единиц измерения.]],
["__module__"]             = [[Операции с единицами измерения.]],
--["arithmetic"]             = [[U1+U2, U1-U2, U1*U2, U1/U2, U1^N]],
--["comparison"]             = [[U1==U2, U1~=U2, U1<U2, U1<=U2, U1>U2, U1>=U2]],
--[".prefix"]                = [[Table of possible prefixes for units.]],
},
}