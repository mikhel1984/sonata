-- LuaCalc russian localisation
{
-------------------- base module ---------------
Calc = {
     __main__       = "Lua как расширяемый калькулятор",
      intro         = [[
Чтобы загрузить дополнительные модули, используйте 'import(module)'.
Справка доступна по команде 'help([функция])'.
Чтобы завершить работу, наберите 'quit()'.
]],
     modules        = "Доступные модули:",
["abs(x)"]          = "Абсолютная величина.",
["exp(x)"]          = "Экспоненциальная зависимость.",
["ln(x)"]           = "Натуральный логарифм.",
["lg(x)"]           = "Десятичный логарифм.",
["pow(a,b)"]        = "Вычисляет a^b.",
["sqrt(a)"]         = "Квадратный корень числа.",
["max(...)"]        = "Находит максимальное из чесел.",
["min(...)"]        = "Находит минимальное из чисел.",

},
----------------------- 'set' ---------------------------
Set = {
     __main__       = "Работа с множествами элементов",
["check(set,val)"]  = "Проверить пренадлежность элемента множеству",
["insert(set,val)"] = "Добавить в множество заданный элемент",
["remove(set,val)"] = "Удалить из множества заданный элемент",
["table(set)"]      = "Представить множество в виде Lua-таблицы",
["Set(t)"]          = "Создать множество из элементов таблицы",
},
--------------------- 'bigint' ----------------------
Big = {
     __main__       = "Операции с целыми числами произвольной точности.",
['BASE']            = "Основание системы счисления.",
["abs(v)"]          = "Абсолютное значение целого числа.",
["copy(v)"]         = "Создаёт копию числа.",
["eq(a,b)"]         = "Проверяет равенство двух чисел.",
["tonumber(v)"]     = "Преобразует число к целому либо с плавающей точкой."
["factorial(n)"]    = "Вычисляет факториал заданного числа.",
["Big(v)"]          = "Создаёт целое число произвольной точности из обычного числа или строки.",
},
-------------------- 'complex' ----------------------
Cmp = {
     __main__       = "Комплексные числа и манипуляции с ними.",
["arg(v)"]          = "Вычисляет аргумент (угол) комплексного числа.",
["abs(v)"]          = "Возвращает модуль комплексного числа.",
["conj(v)"]         = "Возвращает комплексно-сопряжённое число.",
["Re(v)"]           = "Действительная часть числа.",
["Im(v)"]           = "Мнимая часть числа.",
["sqrt(v)"]         = "Квадратный корень положительного или отрицательного числа.",
["_i"]              = "Комплексная единица",
["Cmp(a [,b])"]     = "Создаёт комплексное число через указание действительной (и мнимой) частей.",
},
-------------------- 'gnuplot' ----------------------
Gnu = {
},
--------------------- 'matrix' ----------------------
Mat = {
},
-------------------- 'polynom' ----------------------
Poly = {
},
-------------------- 'rational' ---------------------
Rat = {
},
}