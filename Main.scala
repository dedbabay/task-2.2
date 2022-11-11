import scala.io.StdIn.readLine
import Function.tupled
import scala.annotation.tailrec

object Main {

  // 3.b. Напишите программу, которая вычисляет ежемесячный оклад сотрудника после вычета налогов.
  // На вход вашей программе подается значение годового дохода до вычета налогов,
  // размер премии – в процентах от годового дохода и компенсация питания.
  def year_compensation (year_salary : Double, year_benefit_percents : Double, daily_dinner_compensation : Double): Double =
    (year_salary + (year_salary * year_benefit_percents / 100) + daily_dinner_compensation * 200) * 0.87

  // 3.с. Напишите программу, которая рассчитывает для каждого сотрудника отклонение(в процентах)
  // от среднего значения оклада на уровень всего отдела. В итоговом значении должно учитываться
  // в большую или меньшую сторону отклоняется размер оклада.
  // На вход вышей программе подаются все значения, аналогичные предыдущей программе,
  // а также список со значениями окладов сотрудников отдела 100, 150, 200, 80, 120, 75.
  def get_salary_diff_from_avg (list_salary : List[Int]) = {
    val avg_salary = list_salary.sum / list_salary.size
    list_salary.map(a => a - avg_salary)
  }

  def get_range_from_sorted_list_by_bounds (list : List[Int], value_from: Int, value_to : Int) = {
    val index_from = list.indexWhere ( x => x >= value_from )
    val index_to = list.lastIndexWhere( x=> x <= value_to )
    ( index_from, index_to )
  }

  def get_array_with_avg_salaries_by_level ( list_sorted_salaries : List[Double], salary_high_middle: Int, salary_high_senior: Int ) = {
    val arr_avg_salaries:Array[Array[Double]] = Array.ofDim[Double](3, 3)
    list_sorted_salaries.foreach(x => {
      if (x < salary_high_middle) {
        arr_avg_salaries(0)(0) = arr_avg_salaries(0)(0) + 1
        arr_avg_salaries(0)(1) = arr_avg_salaries(0)(1) + x
        arr_avg_salaries(0)(2) = arr_avg_salaries(0)(1) / arr_avg_salaries(0)(0)
      }
      else if (x < salary_high_senior) {
        arr_avg_salaries(1)(0) = arr_avg_salaries(1)(0) + 1
        arr_avg_salaries(1)(1) = arr_avg_salaries(1)(1) + x
        arr_avg_salaries(1)(2) = arr_avg_salaries(1)(1) / arr_avg_salaries(1)(0)
      }
      else {
        arr_avg_salaries(2)(0) = arr_avg_salaries(2)(0) + 1
        arr_avg_salaries(2)(1) = arr_avg_salaries(2)(1) + x
        arr_avg_salaries(2)(2) = arr_avg_salaries(2)(1) / arr_avg_salaries(2)(0)
      }
    })
    arr_avg_salaries
  }

  def roundAt(p: Int)(n: Double): Double = { val s = math pow (10, p); (math round n * s) / s }

  def pow_of_2 ( p : Int ) : Int= {
    if ( p == 1 ) 2
    else 2 * pow_of_2(p - 1)
  }

  def pow_of_2_tail (p : Int ) : Int ={
    @tailrec def pow_of_2_tail_do ( p: Int, n: Int ): Int = {
      if ( n <= 1 ) p
      else pow_of_2_tail_do(2 * p, n - 1)
    }
    pow_of_2_tail_do(2, p)
  }

  def main(args: Array[String]): Unit = {
    val msg = "Hello Scala"

    println( "3.a ---------------------------------- ")
    // 3.a. Напишите программу, которая:
    //  i.   выводит фразу «Hello, Scala!» справа налево
    println(msg.reverse)
    //  ii.  переводит всю фразу в нижний регистр
    println(msg.toLowerCase)
    //  iii. удаляет символ!
    println(msg.filter(_ != 'S'))
    //  iv.  добавляет в конец фразы «and goodbye python!»
    println(msg + " and goodbye python (no, I'm kidding)")

    /*
    println("Enter gross salary a year: ")
    val year_salary = readLine().toInt
    println("Enter benefit in percent")
    val year_benefit_percents = readLine().toFloat
    println("Enter daily dinner compensation")
    val daily_dinner_compensation = readLine().toInt
    */

    println( "3.b ---------------------------------- ")
    val year_salary = 3600000
    val year_benefit_percents = 25
    val daily_dinner_compensation = 300
    println ( year_compensation(year_salary, year_benefit_percents, daily_dinner_compensation))

    println( "3.c ---------------------------------- ")
    val salary_list = List (100, 150, 200, 80, 120, 75)
    println("Salary list : ", salary_list)
    println(" - avg salary", salary_list.sum / salary_list.length )
    println(" - diff salary from avg: ", get_salary_diff_from_avg( salary_list ) )

    println( "3.d ---------------------------------- ")
    // 3.d. Попробуйте рассчитать новую зарплату сотрудника, добавив(или отняв, если сотрудник плохо себя вел)
    // необходимую сумму с учетом результатов прошлого задания.
    // Добавьте его зарплату в список и вычислите значение самой высокой зарплаты и самой низкой.
    // Заметка: Задание насктолько нечеткое, что я решил использовать результаты предыдущих задач и
    // "скорректировать" зарплаты исходя из их отклонений от среднего на половину отклонения.
    var salary_list1 = salary_list ::: List(115)
    val salary_diff_from_avg = get_salary_diff_from_avg( salary_list1 )
    println("Max salary: ", salary_list1.max)
    println("Min salary: ", salary_list1.min)
    println("Corrected salary: ")
    val salary_list_corrected = salary_list1.zip(salary_diff_from_avg) map tupled { _ + (-0.5)*_ }
    println (salary_list_corrected)

    println( "3.e ---------------------------------- ")
    // 3.e. Также в вашу команду пришли два специалиста с окладами 350 и 90 тысяч рублей.
    // Попробуйте отсортировать список сотрудников по уровню оклада от меньшего к большему.
    salary_list1 = salary_list1 ::: List ( 350, 90)
    var salary_sorted_list = salary_list1.sorted
    println ( salary_sorted_list )

    println( "3.f ---------------------------------- ")
    // 3. f. Кажется, вы взяли в вашу команду еще одного сотрудника и предложили ему оклад 130 тысяч.
    // Вычислите самостоятельно номер сотрудника в списке так, чтобы сортировка не нарушилась
    // и добавьте его на это место.
    val i = salary_sorted_list.lastIndexWhere( x => x <= 130)
    // split, insert and concat
    val ( left, right ) = salary_sorted_list.splitAt(i + 1)
    val s1 = left ++ List( 130 ) ++ right
    // or patch
    val s2 = salary_sorted_list.patch(i + 1, List(130), 0)
    println ( s1 )
    println ( s2 )

    println( "3.g ---------------------------------- ")
    // 3.g. Попробуйте вывести номера сотрудников из полученного списка, которые попадают под категорию middle.
    // На входе программе подается «вилка» зарплаты специалистов уровня middle.
    val middle_low_salary = 100
    val middle_high_salary = 120
    val ift = get_range_from_sorted_list_by_bounds(s2, middle_low_salary, middle_high_salary)
    for ( i <- ift._1 to ift._2 ) println ( i )

    println( "3.g ---------------------------------- ")
    // 3.h. Однако наступил кризис и ваши сотрудники требуют повысить зарплату.
    // Вам необходимо проиндексировать зарплату каждого сотрудника на уровень инфляции – 7%
    val new_salary = s2.map(x => x+x*0.07)
    println ( new_salary )

    println( "3.i * ---------------------------------- ")
    // 3.i. *Ваши сотрудники остались недовольны и просят индексацию на уровень рынка.
    // Попробуйте повторить ту же операцию, как и в предыдущем задании, но теперь вам нужно
    // проиндексировать зарплаты на процент отклонения от среднего по рынку с учетом уровня специалиста.
    // На вход вашей программе подается 3 значения – среднее значение зарплаты на рынке для каждого уровня
    // специалистов(junior, middle и senior)

    // set boundaries
    val hs_junior = 99
    val hs_middle = 120
    val hs_senior = 200

    val arr_avg_salaries  = get_array_with_avg_salaries_by_level( new_salary, hs_middle, hs_senior )
    for ( r <- 0 to 2; c <- 0 to 2 ) {
      print (r, c)
      println ("=" + arr_avg_salaries(r)(c) )
    }
    println(arr_avg_salaries(0)(0))
    println(arr_avg_salaries(0)(0).getClass)

    // let's set average salaries by market
    val avg_s_by_market_junior = 110
    val avg_s_by_market_middle = 145
    val avg_s_by_market_senior = 280

    // calculate diff between averages (fact vs market by level)
    val junior_diff_percent = (avg_s_by_market_junior - arr_avg_salaries(0)(2)) / arr_avg_salaries(0)(2)
    val middle_diff_percent = (avg_s_by_market_middle - arr_avg_salaries(1)(2)) / arr_avg_salaries(1)(2)
    val senior_diff_percent = (avg_s_by_market_senior - arr_avg_salaries(2)(2)) / arr_avg_salaries(2)(2)

    // correct salaries for junior, middle and senior
    val new_corrected_by_market_salary = new_salary.map ( x => {
      if ( x <= hs_middle )
        roundAt(2)(x + (x * junior_diff_percent))
      else if ( x <= hs_senior )
        roundAt(2)(x + (x * middle_diff_percent))
      else
        roundAt(2)(x + (x * senior_diff_percent))
    })
    println( new_corrected_by_market_salary )

    println("3.k * ---------------------------------- ")
    // 3.k. *Попробуйте деанонимизировать ваших сотрудников – составьте структуру,
    // которая позволит иметь знания о том, сколько зарабатывает каждый сотрудник(Фамилия и имя)
    val staff = List (
      User("Vasya", "Pupkin", 102),
      User("Petya", "Siskin", 570),
      User("Kolya", "Zhopkin", 78),
      User("Ira", "Pusya", 117),
      User("Masha", "Kasha", 100),
      User("Sasha", "Rasha", 300),
      User("Nguen", "Tsuyeng", 50),
      User("Samuel", "Jackson", 350)
    )

    println("3.l * ---------------------------------- ")
    // 3.l. * Выведите фамилию и имя сотрудников с самой высокой и самой низкой зарплатой(только не рассказывайте им об этом факте)
    var min_e = User("", "", 9999)
    var max_e = User("", "", 0)
    staff.foreach { u =>
      if ( u.salary > max_e.salary ) max_e = u
      if ( u.salary < min_e.salary ) min_e = u
    }
    println ( "Minimum salary has: " + min_e )
    println ( "Maximum_salary has: " + max_e )

    println("3.m * ---------------------------------- ")
    // 3.m * Попробуйте запутать тех, кто может случайно наткнуться на эти данные – удалите для каждого сотрудника имя,
    // переведите строку в нижний регистр, удалите гласные и разверните оставшиеся символы справа налево(abc -> cb)
    println("Minimum salary has: " + min_e.GetCodedString)
    println("Maximum_salary has: " + max_e.GetCodedString)

    // 3 n.*ПОпробуйте завернуть программу из пункта 3.b в функцию и входные значения переделать в параметры функции.
    // done, see before.

    println("3.o * ---------------------------------- ")
    // 3. o * Попробуйте написать функцию, которая вычисляет значение степени двойки:
    //  С помощью обычной рекурсии
    println("-- regular recursion ")
    println ( "2 ^ 2 = " + pow_of_2(2))
    println ( "2 ^ 10 = " + pow_of_2(10))
    // ** С помощью хвостовой рекурсии
    println("-- tail recursion ")
    println( "2 ^ 3 = " + pow_of_2_tail(3))
    println( "2 ^ 16 = " + pow_of_2_tail(16))
  }


}